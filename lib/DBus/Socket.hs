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
--
-- Most users will want to use the "DBus.Client" or "DBus.Client.Simple"
-- modules instead.
module DBus.Socket
	(
	
	-- * Sockets
	  Socket
	, SocketListener
	, SocketError
	, socketErrorMessage
	
	-- * Socket options
	, SocketOptions
	, socketAuthenticators
	, socketTransportOptions
	, defaultSocketOptions
	
	-- * Opening and closing sockets
	, open
	, openWith
	, listen
	, listenWith
	, accept
	, close
	, closeListener
	
	-- * Sending and receiving messages
	, send
	, receive
	
	-- * Authentication
	, Authenticator
	, authenticator
	, authenticatorClient
	, authenticatorServer
	
	-- ** Built-in authenticators
	, authExternal
	) where

import           Prelude hiding (getLine)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad (when)
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
	{ socketTransport :: SomeTransport
	, socketSerial :: IORef Serial
	, socketReadLock :: MVar ()
	, socketWriteLock :: MVar ()
	}

-- | An Authenticator defines how the local peer (client) authenticates
-- itself to the remote peer (server).
data Authenticator t = Authenticator
	{
	-- | Defines the client-side half of an authenticator.
	  authenticatorClient :: t -> IO Bool
	
	-- | Defines the server-side half of an authenticator.
	, authenticatorServer :: t -> IO Bool
	}

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
		authed <- authClient t (socketAuthenticators opts)
		when (not authed) $ do
			throwIO (SocketError "Authentication failed")
		serial <- newIORef (Serial 1)
		readLock <- newMVar ()
		writeLock <- newMVar ()
		return (Socket (SomeTransport t) serial readLock writeLock))

data SocketListener = forall t. (TransportListen t) => SocketListener (TransportListener t) [Authenticator t]

-- | Begin listening at the given address.
--
-- Use 'accept' to create sockets from incoming connections.
--
-- Use 'closeListener' to stop listening, and to free underlying transport
-- resources such as file descriptors.
listen :: Address -> IO (Either SocketError SocketListener)
listen = listenWith defaultSocketOptions

-- | Begin listening at the given address.
--
-- Use 'accept' to create sockets from incoming connections.
--
-- Use 'closeListener' to stop listening, and to free underlying transport
-- resources such as file descriptors.
--
-- This function is for users who need to define custom authenticators
-- or transports.
listenWith :: TransportListen t => SocketOptions t -> Address -> IO (Either SocketError SocketListener)
listenWith opts addr = toEither $ bracketOnError
	(transportListen (socketTransportOptions opts) addr)
	transportListenerClose
	(\l -> return (SocketListener l (socketAuthenticators opts)))

-- | Accept a new connection from a socket listener.
accept :: SocketListener -> IO (Either SocketError Socket)
accept (SocketListener l auths) = toEither $ bracketOnError
	(transportAccept l)
	transportClose
	(\t -> do
		authed <- authServer t auths
		when (not authed) $ do
			throwIO (SocketError "Authentication failed")
		serial <- newIORef (Serial 1)
		readLock <- newMVar ()
		writeLock <- newMVar ()
		return (Socket (SomeTransport t) serial readLock writeLock))

-- | Close an open 'Socket'. Once closed, the socket is no longer valid and
-- must not be used.
close :: Socket -> IO ()
close = transportClose . socketTransport

-- | Close an open 'SocketListener'. Once closed, the listener is no longer
-- valid and must not be used.
closeListener :: SocketListener -> IO ()
closeListener (SocketListener l _) = transportListenerClose l

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
	-- TODO: after reading the length, read all bytes from the
	--       handle, then return a closure to perform the parse
	--       outside of the lock.
	let t = socketTransport sock
	received <- withMVar (socketReadLock sock) (\_ -> unmarshalMessageM (transportGet t))
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

authClient :: Transport t => t -> [Authenticator t] -> IO Bool
authClient t authenticators = go where
	go = do
		transportPut t (Data.ByteString.pack [0])
		loop authenticators
	loop [] = return False
	loop (auth:next) = do
		success <- authenticatorClient auth t
		if success
			then do
				transportPut t "BEGIN\r\n"
				return True
			else loop next

authServer :: Transport t => t -> [Authenticator t] -> IO Bool
authServer t authenticators = go where
	go = do
		c <- transportGet t 1
		if c == "\x00"
			then loop authenticators
			else return False
	-- TODO: this is broken, we need to handle "AUTH ...", then parse
	-- out the method and check if it's supported by the list of
	-- authenticators.
	loop [] = return False
	loop (auth:next) = do
		success <- authenticatorServer auth t
		if success
			then do
				transportPut t "OK guid-goes-here\r\n"
				return True
			else loop next

-- | An empty authenticator. Use 'authentictorClient' or 'authenticatorServer'
-- to control how the authentication is performed.
--
-- @
--myAuthenticator :: Authenticator MyTransport
--myAuthenticator = authenticator
--    { 'authenticatorClient' = clientMyAuth
--    , 'authenticatorServer' = serverMyAuth
--    }
--
--clientMyAuth :: MyTransport -> IO Bool
--serverMyAuth :: MyTransport -> IO Bool
-- @
authenticator :: Authenticator t
authenticator = Authenticator (\_ -> return False) (\_ -> return False)

-- | Implements the D-Bus @EXTERNAL@ mechanism, which uses credential
-- passing over a UNIX socket.
authExternal :: Authenticator SocketTransport
authExternal = authenticator
	{ authenticatorClient = clientAuthExternal
	}

clientAuthExternal :: Transport t => t -> IO Bool
clientAuthExternal t = do
	uid <- System.Posix.User.getRealUserID
	let token = concatMap (printf "%02X" . ord) (show uid)
	transportPutLine t ("AUTH EXTERNAL " ++ token)
	resp <- transportGetLine t
	return $ case takeWhile (/= ' ') resp of
		"OK" -> True
		_ -> False

transportPutLine :: Transport t => t -> String -> IO ()
transportPutLine t line = transportPut t (Char8.pack (line ++ "\r\n"))

transportGetLine :: Transport t => t -> IO String
transportGetLine t = do
	let getchr = Char8.head `fmap` transportGet t 1
	raw <- readUntil "\r\n" getchr
	return (dropEnd 2 raw)
