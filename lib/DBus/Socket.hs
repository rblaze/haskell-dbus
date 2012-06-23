{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
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
-- Most users will want to use the "DBus.Client" module instead.
module DBus.Socket
	(
	
	-- * Sockets
	  Socket
	, send
	, receive
	
	-- * Socket errors
	, SocketError
	, socketError
	, socketErrorMessage
	, socketErrorFatal
	, socketErrorAddress
	
	-- * Socket options
	, SocketOptions
	, socketAuthenticator
	, socketTransportOptions
	, defaultSocketOptions
	
	-- * Opening and closing sockets
	, open
	, openWith
	, close
	
	-- * Listening for connections
	, SocketListener
	, listen
	, listenWith
	, accept
	, closeListener
	, socketListenerAddress
	
	-- * Authentication
	, Authenticator
	, authenticator
	, authenticatorClient
	, authenticatorServer
	) where

import           Prelude hiding (getLine)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad (mplus)
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord)
import           Data.IORef
import           Data.List (isPrefixOf)
import           Data.Typeable (Typeable)
import qualified System.Posix.User
import           Text.Printf (printf)

import           DBus
import           DBus.Transport
import           DBus.Wire (unmarshalMessageM)

-- | Stores information about an error encountered while creating or using a
-- 'Socket'.
data SocketError = SocketError
	{ socketErrorMessage :: String
	, socketErrorFatal :: Bool
	, socketErrorAddress :: Maybe Address
	}
	deriving (Eq, Show, Typeable)

instance Exception SocketError

socketError :: String -> SocketError
socketError msg = SocketError msg True Nothing

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
	, socketAddress :: Maybe Address
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
	
	-- | Defines the server-side half of an authenticator. The UUID is
	-- allocated by the socket listener.
	, authenticatorServer :: t -> UUID -> IO Bool
	}

-- | Used with 'openWith' and 'listenWith' to provide custom authenticators or
-- transport options.
data SocketOptions t = SocketOptions
	{
	-- | Used to perform authentication with the remote peer. After a
	-- transport has been opened, it will be passed to the authenticator.
	-- If the authenticator returns true, then the socket was
	-- authenticated.
	  socketAuthenticator :: Authenticator t
	
	-- | Options for the underlying transport, to be used by custom transports
	-- for controlling how to connect to the remote peer.
	--
	-- See "DBus.Transport" for details on defining custom transports
	, socketTransportOptions :: TransportOptions t
	}

-- | Default 'SocketOptions', which uses the default Unix/TCP transport and
-- authenticator.
defaultSocketOptions :: SocketOptions SocketTransport
defaultSocketOptions = SocketOptions
	{ socketTransportOptions = transportDefaultOptions
	, socketAuthenticator = authExternal
	}

-- | Open a socket to a remote peer listening at the given address.
--
-- @
--open = 'openWith' 'defaultSocketOptions'
-- @
--
-- Throws 'SocketError' on failure.
open :: Address -> IO Socket
open = openWith defaultSocketOptions

-- | Open a socket to a remote peer listening at the given address.
--
-- Most users should use 'open'. This function is for users who need to define
-- custom authenticators or transports.
--
-- Throws 'SocketError' on failure.
openWith :: TransportOpen t => SocketOptions t -> Address -> IO Socket
openWith opts addr = toSocketError (Just addr) $ bracketOnError
	(transportOpen (socketTransportOptions opts) addr)
	transportClose
	(\t -> do
		authed <- authenticatorClient (socketAuthenticator opts) t
		if not authed
			then throwIO (socketError "Authentication failed")
				{ socketErrorAddress = Just addr
				}
			else do
				serial <- newIORef firstSerial
				readLock <- newMVar ()
				writeLock <- newMVar ()
				return (Socket (SomeTransport t) (Just addr) serial readLock writeLock))

data SocketListener = forall t. (TransportListen t) => SocketListener (TransportListener t) (Authenticator t)

-- | Begin listening at the given address.
--
-- Use 'accept' to create sockets from incoming connections.
--
-- Use 'closeListener' to stop listening, and to free underlying transport
-- resources such as file descriptors.
--
-- Throws 'SocketError' on failure.
listen :: Address -> IO SocketListener
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
--
-- Throws 'SocketError' on failure.
listenWith :: TransportListen t => SocketOptions t -> Address -> IO SocketListener
listenWith opts addr = toSocketError (Just addr) $ bracketOnError
	(transportListen (socketTransportOptions opts) addr)
	transportListenerClose
	(\l -> return (SocketListener l (socketAuthenticator opts)))

-- | Accept a new connection from a socket listener.
--
-- Throws 'SocketError' on failure.
accept :: SocketListener -> IO Socket
accept (SocketListener l auth) = toSocketError Nothing $ bracketOnError
	(transportAccept l)
	transportClose
	(\t -> do
		let uuid = transportListenerUUID l
		authed <- authenticatorServer auth t uuid
		if not authed
			then throwIO (socketError "Authentication failed")
			else do
				serial <- newIORef firstSerial
				readLock <- newMVar ()
				writeLock <- newMVar ()
				return (Socket (SomeTransport t) Nothing serial readLock writeLock))

-- | Close an open 'Socket'. Once closed, the socket is no longer valid and
-- must not be used.
close :: Socket -> IO ()
close = transportClose . socketTransport

-- | Close an open 'SocketListener'. Once closed, the listener is no longer
-- valid and must not be used.
closeListener :: SocketListener -> IO ()
closeListener (SocketListener l _) = transportListenerClose l

-- | Get the address to use to connect to a listener.
socketListenerAddress :: SocketListener -> Address
socketListenerAddress (SocketListener l _) = transportListenerAddress l

-- | Send a single message, with a generated 'Serial'. The second parameter
-- exists to prevent race conditions when registering a reply handler; it
-- receives the serial the message /will/ be sent with, before it's
-- actually sent.
--
-- Sockets are thread-safe. Only one message may be sent at a time; if
-- multiple threads attempt to send messages concurrently, one will block
-- until after the other has finished.
--
-- Throws 'SocketError' on failure.
send :: Message msg => Socket -> msg -> (Serial -> IO a) -> IO a
send sock msg io = toSocketError (socketAddress sock) $ do
	serial <- nextSocketSerial sock
	case marshal LittleEndian serial msg of
		Right bytes -> do
			let t = socketTransport sock
			a <- io serial
			withMVar (socketWriteLock sock) (\_ -> transportPut t bytes)
			return a
		Left err -> throwIO (socketError ("Message cannot be sent: " ++ show err))
			{ socketErrorFatal = False
			}

nextSocketSerial :: Socket -> IO Serial
nextSocketSerial sock = atomicModifyIORef (socketSerial sock) (\x -> (nextSerial x, x))

-- | Receive the next message from the socket , blocking until one is available.
--
-- Sockets are thread-safe. Only one message may be received at a time; if
-- multiple threads attempt to receive messages concurrently, one will block
-- until after the other has finished.
--
-- Throws 'SocketError' on failure.
receive :: Socket -> IO ReceivedMessage
receive sock = toSocketError (socketAddress sock) $ do
	-- TODO: after reading the length, read all bytes from the
	--       handle, then return a closure to perform the parse
	--       outside of the lock.
	let t = socketTransport sock
	let get n = if n == 0
		then return Data.ByteString.empty
		else transportGet t n
	received <- withMVar (socketReadLock sock) (\_ -> unmarshalMessageM get)
	case received of
		Left err -> throwIO (socketError ("Error reading message from socket: " ++ show err))
		Right msg -> return msg

toSocketError :: Maybe Address -> IO a -> IO a
toSocketError addr io = catches io handlers where
	handlers =
		[ Handler catchTransportError
		, Handler updateSocketError
		, Handler catchIOException
		]
	catchTransportError err = throwIO (socketError (transportErrorMessage err))
		{ socketErrorAddress = addr
		}
	updateSocketError err = throwIO err
		{ socketErrorAddress = mplus (socketErrorAddress err) addr
		}
	catchIOException exc = throwIO (socketError (show (exc :: IOException)))
		{ socketErrorAddress = addr
		}

-- | An empty authenticator. Use 'authenticatorClient' or 'authenticatorServer'
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
--serverMyAuth :: MyTransport -> String -> IO Bool
-- @
authenticator :: Authenticator t
authenticator = Authenticator (\_ -> return False) (\_ _ -> return False)

-- | Implements the D-Bus @EXTERNAL@ mechanism, which uses credential
-- passing over a Unix socket.
authExternal :: Authenticator SocketTransport
authExternal = authenticator
	{ authenticatorClient = clientAuthExternal
	, authenticatorServer = serverAuthExternal
	}

clientAuthExternal :: SocketTransport -> IO Bool
clientAuthExternal t = do
	transportPut t (Data.ByteString.pack [0])
	uid <- System.Posix.User.getRealUserID
	let token = concatMap (printf "%02X" . ord) (show uid)
	transportPutLine t ("AUTH EXTERNAL " ++ token)
	resp <- transportGetLine t
	case splitPrefix "OK " resp of
		Just _ -> do
			transportPutLine t "BEGIN"
			return True
		Nothing -> return False

serverAuthExternal :: SocketTransport -> UUID -> IO Bool
serverAuthExternal t uuid = do
	let waitForBegin = do
		resp <- transportGetLine t
		if resp == "BEGIN"
			then return ()
			else waitForBegin
	
	let checkToken token = do
		(_, uid, _) <- socketTransportCredentials t
		let wantToken = concatMap (printf "%02X" . ord) (show uid)
		if token == wantToken
			then do
				transportPutLine t ("OK " ++ formatUUID uuid)
				waitForBegin
				return True
			else return False
	
	c <- transportGet t 1
	if c /= Char8.pack "\x00"
		then return False
		else do
			line <- transportGetLine t
			case splitPrefix "AUTH EXTERNAL " line of
				Just token -> checkToken token
				Nothing -> if line == "AUTH EXTERNAL"
					then do
						dataLine <- transportGetLine t
						case splitPrefix "DATA " dataLine of
							Just token -> checkToken token
							Nothing -> return False
					else return False

transportPutLine :: Transport t => t -> String -> IO ()
transportPutLine t line = transportPut t (Char8.pack (line ++ "\r\n"))

transportGetLine :: Transport t => t -> IO String
transportGetLine t = do
	let getchr = Char8.head `fmap` transportGet t 1
	raw <- readUntil "\r\n" getchr
	return (dropEnd 2 raw)

-- | Drop /n/ items from the end of a list
dropEnd :: Int -> [a] -> [a]
dropEnd n xs = take (length xs - n) xs

splitPrefix :: String -> String -> Maybe String
splitPrefix prefix str = if isPrefixOf prefix str
	then Just (drop (length prefix) str)
	else Nothing

-- | Read values from a monad until a guard value is read; return all
-- values, including the guard.
readUntil :: (Monad m, Eq a) => [a] -> m a -> m [a]
readUntil guard getx = readUntil' [] where
	guard' = reverse guard
	step xs | isPrefixOf guard' xs = return (reverse xs)
	        | otherwise            = readUntil' xs
	readUntil' xs = do
		x <- getx
		step (x:xs)
