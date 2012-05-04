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
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord)
import           Data.IORef
import qualified System.Posix.User
import           Text.Printf (printf)

import           DBus
import           DBus.Transport
import           DBus.Types (Serial(..))
import           DBus.Wire (unmarshalMessageM)
import           DBus.Util (readUntil, dropEnd)
import           DBus.Util.MonadError

-- | Stores information about an error encountered while creating or using a
-- 'Socket'.
data SocketError = SocketError String
	deriving (Eq, Ord, Show)

-- | Get an error message describing a 'SocketError'.
socketErrorMessage :: SocketError -> String
socketErrorMessage (SocketError msg) = msg

type SocketM = ErrorT SocketError IO

socketError :: String -> SocketM a
socketError = throwErrorT . SocketError

socketIO :: IO a -> SocketM a
socketIO io = do
	tried <- liftIO (Control.Exception.try io)
	case tried of
		Left err -> socketError (show (err :: IOException))
		Right a -> return a

-- | TODO
newtype Authenticator t = Authenticator (t -> SocketM Bool)

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
openWith opts addr = do
	eTrans <- transportOpen (socketTransportOptions opts) addr
	case eTrans of
		Left err -> return (Left (SocketError (show err)))
		Right trans -> do
			authed <- runErrorT (authenticate trans (socketAuthenticators opts))
			case authed of
				Left err -> do
					transportClose trans
					return (Left err)
				Right False -> do
					transportClose trans
					return (Left (SocketError "Authentication failed"))
				Right True -> do
					serial <- newIORef (Serial 1)
					readLock <- newMVar ()
					writeLock <- newMVar ()
					return (Right (Socket addr (SomeTransport trans) serial readLock writeLock))

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
		Right bytes -> do
			a <- io serial
			tried <- Control.Exception.try (withMVar
				(socketWriteLock sock)
				(\_ -> transportPut (socketTransport sock) bytes))
			case tried of
				Left err -> return (Left (SocketError (show (err :: IOException))))
				Right _ -> return (Right a)
		Left err -> return (Left (SocketError ("Message cannot be written: " ++ show err)))

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
receive sock = do
	tried <- Control.Exception.try (withMVar
		(socketReadLock sock)
		-- TODO: instead of fromIntegral, unify types
		-- TODO: after reading the length, read all bytes from the
		--       handle, then return a closure to perform the parse
		--       outside of the lock.
		(\_ -> unmarshalMessageM (transportGet (socketTransport sock) . fromIntegral)))
	case tried of
		Left err -> return (Left (SocketError (show (err :: IOException))))
		Right unmarshaled -> case unmarshaled of
			Left err -> return (Left (SocketError ("Error reading message from socket: " ++ show err)))
			Right msg -> return (Right msg)

authenticate :: Transport t => t -> [Authenticator t] -> SocketM Bool
authenticate t authenticators = go where
	go = do
		socketIO (transportPut t (Data.ByteString.pack [0]))
		loop authenticators
	loop [] = return False
	loop ((Authenticator auth):next) = do
		success <- auth t
		if success
			then return True
			else loop next

type AuthM t = ReaderT t SocketM

putLine :: Transport t => String -> AuthM t ()
putLine line = do
	t <- ask
	lift (socketIO (transportPut t (Char8.pack (line ++ "\r\n"))))

getLine :: Transport t => AuthM t String
getLine = do
	t <- ask
	let getchr = Char8.head `fmap` transportGet t 1
	lift (socketIO (do
		raw <- readUntil "\r\n" getchr
		return (dropEnd 2 raw)))

-- | TODO
authExternal :: Transport t => Authenticator t
authExternal = Authenticator $ runReaderT $ do
	uid <- lift (socketIO System.Posix.User.getRealUserID)
	let token = concatMap (printf "%02X" . ord) (show uid)
	putLine ("AUTH EXTERNAL " ++ token)
	resp <- getLine
	case takeWhile (/= ' ') resp of
		"OK" -> do
			putLine "BEGIN"
			return True
		_ -> return False
