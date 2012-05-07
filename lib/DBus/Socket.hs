{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- Copyright (C) 2009-2012 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | D-Bus sockets are used for communication between two peers. In this model,
-- there is no \"bus\" or \"client\", simply two endpoints sending messages.
module DBus.Socket
	(
	
	-- * Types
	  Socket
	, socketAddress
	, SocketError
	, socketErrorMessage
	
	-- * Opening and closing sockets
	, open
	, openWith
	, close
	, SocketOptions
	, socketAuthenticators
	, socketTransportOptions
	, defaultSocketOptions
	
	-- * Sending and receiving messages
	, send
	, receive
	
	-- * Authentication
	, Authenticator
	, authExternal
	) where

import           Prelude hiding (getLine)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord)
import           Data.IORef
import           Data.Typeable (Typeable)
import qualified System.Posix.User
import           Text.Printf (printf)

import           DBus
import           DBus.Transport
import           DBus.Types (Serial(..))
import           DBus.Wire (unmarshalMessageM)
import           DBus.Util (readUntil, dropEnd)

-- | Stores information about an error encountered while creating or using a
-- 'Socket'.
data SocketError = SocketError String
	deriving (Eq, Show, Typeable)

instance Exception SocketError

-- | Get an error message describing a 'SocketError'.
socketErrorMessage :: SocketError -> String
socketErrorMessage (SocketError msg) = msg

-- | TODO
newtype Authenticator t = Authenticator (t -> IO Bool)

data SomeTransport = forall t. (Transport t) => SomeTransport t

instance Transport SomeTransport where
	data TransportOptions SomeTransport = SomeTransportOptions
	transportDefaultOptions = SomeTransportOptions
	transportPut (SomeTransport t) = transportPut t
	transportGet (SomeTransport t) = transportGet t
	transportClose (SomeTransport t) = transportClose t

-- | An open socket to another process. Messages can be sent to the remote
-- peer using 'send', or received using 'receive'.
data Socket = Socket
	{ socketAddress_ :: Address
	, socketTransport :: SomeTransport
	, socketSerial :: IORef Serial
	, socketReadLock :: MVar ()
	, socketWriteLock :: MVar ()
	}

-- | Get the address of the remote peer.
socketAddress :: Socket -> Address
socketAddress = socketAddress_

-- | Used with 'openWith' to provide custom authenticators or transport options.
data SocketOptions t = SocketOptions
	{
	-- | A list of available authentication mechanisms, whican can be used
	-- to authenticate the socket.
	  socketAuthenticators :: [Authenticator t]
	
	-- | Options for the underlying transport, to be used by custom transports
	-- for controlling how to connect to the remote peer.
	--
	-- See "DBus.Transport" for details on defining custom transports
	, socketTransportOptions :: TransportOptions t
	}

-- | Default 'SocketOptions', which uses the authenticators built into
-- @haskell-dbus@, and the default socket-based transport.
defaultSocketOptions :: SocketOptions SocketTransport
defaultSocketOptions = SocketOptions
	{ socketTransportOptions = transportDefaultOptions
	, socketAuthenticators = [authExternal]
	}

-- | Open a socket to a remote peer listening at the given address.
--
-- @
--open = 'openWith' 'defaultSocketOptions'
-- @
open :: Address -> IO (Either SocketError Socket)
open = openWith defaultSocketOptions

-- | Open a socket to a remote peer listening at the given address.
--
-- Most users should use 'open'. This function is for users who need to define
-- custom authenticators or transports.
openWith :: TransportOpen t => SocketOptions t -> Address -> IO (Either SocketError Socket)
openWith opts addr = toEither $ bracketOnError
	(transportOpen (socketTransportOptions opts) addr)
	transportClose
	(\t -> do
		authed <- authenticate t (socketAuthenticators opts)
		when (not authed) $ do
			throwIO (SocketError "Authentication failed")
		serial <- newIORef (Serial 1)
		readLock <- newMVar ()
		writeLock <- newMVar ()
		return (Socket addr (SomeTransport t) serial readLock writeLock))

-- | Close an open 'Socket'. Once closed, the 'Socket' is no longer valid and
-- must not be used.
close :: Socket -> IO ()
close = transportClose . socketTransport

-- | Send a single message, with a generated 'Serial'. The second parameter
-- exists to prevent race conditions when registering a reply handler; it
-- receives the serial the message /will/ be sent with, before it&#8217;s
-- actually sent.
--
-- Sockets are thread-safe. Only one message may be sent at a time; if
-- multiple threads attempt to send messages concurrently, one will block
-- until after the other has finished.
send :: Message msg => Socket -> msg -> (Serial -> IO a) -> IO (Either SocketError a)
send sock msg io = do
	serial <- nextSerial sock
	case marshalMessage LittleEndian serial msg of
		Right bytes -> toEither $ do
			let t = socketTransport sock
			a <- io serial
			withMVar (socketWriteLock sock) (\_ -> transportPut t bytes)
			return a
		Left err -> return (Left (SocketError ("Message cannot be sent: " ++ show err)))

nextSerial :: Socket -> IO Serial
nextSerial sock = atomicModifyIORef
	(socketSerial sock)
	(\serial@(Serial x) -> (Serial (x + 1), serial))

-- | Receive the next message from the socket , blocking until one is available.
--
-- Sockets are thread-safe. Only one message may be received at a time; if
-- multiple threads attempt to receive messages concurrently, one will block
-- until after the other has finished.
receive :: Socket -> IO (Either SocketError ReceivedMessage)
receive sock = toEither $ do
	-- TODO: instead of fromIntegral, unify types
	-- TODO: after reading the length, read all bytes from the
	--       handle, then return a closure to perform the parse
	--       outside of the lock.
	let t = socketTransport sock
	let get = transportGet t . fromIntegral
	received <- withMVar (socketReadLock sock) (\_ -> unmarshalMessageM get)
	case received of
		Left err -> throwIO (SocketError ("Error reading message from socket: " ++ show err))
		Right msg -> return msg

toEither :: IO a -> IO (Either SocketError a)
toEither io = catches (fmap Right io) handlers where
	handlers =
		[ Handler catchSocketError
		, Handler catchTransportError
		, Handler catchIOException
		]
	catchSocketError exc = return (Left exc)
	catchTransportError (TransportError err) = return (Left (SocketError err))
	catchIOException exc = return (Left (SocketError (show (exc :: IOException))))

authenticate :: Transport t => t -> [Authenticator t] -> IO Bool
authenticate t authenticators = go where
	go = do
		transportPut t (Data.ByteString.pack [0])
		loop authenticators
	loop [] = return False
	loop ((Authenticator auth):next) = do
		success <- auth t
		if success
			then return True
			else loop next

putLine :: Transport t => String -> ReaderT t IO ()
putLine line = do
	t <- ask
	liftIO (transportPut t (Char8.pack (line ++ "\r\n")))

getLine :: Transport t => ReaderT t IO String
getLine = do
	t <- ask
	let getchr = Char8.head `fmap` transportGet t 1
	liftIO (do
		raw <- readUntil "\r\n" getchr
		return (dropEnd 2 raw))

-- | TODO
authExternal :: Transport t => Authenticator t
authExternal = Authenticator $ runReaderT $ do
	uid <- liftIO System.Posix.User.getRealUserID
	let token = concatMap (printf "%02X" . ord) (show uid)
	putLine ("AUTH EXTERNAL " ++ token)
	resp <- getLine
	case takeWhile (/= ' ') resp of
		"OK" -> do
			putLine "BEGIN"
			return True
		_ -> return False
