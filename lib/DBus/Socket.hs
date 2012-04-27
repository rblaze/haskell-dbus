{-# LANGUAGE OverloadedStrings #-}

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

module DBus.Socket
	(
	
	-- * DBus sockets
	  Socket
	, SocketError
	, socketErrorMessage
	, socketAddress
	, connect
	, close
	, send
	, receive
	
	-- ** Advanced connection
	, connectWith
	, SocketOptions
	, socketTransports
	, socketAuthenticators
	, defaultSocketOptions
	
	-- * Authentication
	, Authenticator
	, authExternal
	
	-- * Transports
	, Transport
	, transportUnix
	, transportTCP
	) where

import           Prelude hiding (getLine)

import           Control.Concurrent
import           Control.Exception
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord)
import           Data.IORef
import qualified Data.Map
import qualified Network
import qualified Network.Socket
import           System.IO hiding (getLine)
import qualified System.Posix.User
import           Text.Printf (printf)

import           DBus
import           DBus.Types (Serial(..))
import           DBus.Wire (unmarshalMessageM)
import           DBus.Util (readUntil, dropEnd, readPortNumber)
import           DBus.Util.MonadError

-- | TODO
data SocketError = SocketError String
	deriving (Eq, Ord, Show)

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
data Transport = Transport ByteString (Address -> SocketM Handle)

-- | TODO
newtype Authenticator = Authenticator (Handle -> SocketM Bool)

-- | TODO
data Socket = Socket
	{ socketAddress_ :: Address
	, socketHandle :: Handle
	, socketSerial :: IORef Serial
	, socketReadLock :: MVar ()
	, socketWriteLock :: MVar ()
	}

-- | TODO
socketAddress :: Socket -> Address
socketAddress = socketAddress_

-- | TODO
data SocketOptions = SocketOptions
	{ socketTransports :: [Transport]
	, socketAuthenticators :: [Authenticator]
	}

defaultSocketOptions :: SocketOptions
defaultSocketOptions = SocketOptions
	{ socketTransports = [transportUnix, transportTCP]
	, socketAuthenticators = [authExternal]
	}

connect :: Address -> IO (Either SocketError Socket)
connect = connectWith defaultSocketOptions

connectWith :: SocketOptions -> Address -> IO (Either SocketError Socket)
connectWith opts addr = do
	connected <- runErrorT (connectTransport (socketTransports opts) addr)
	case connected of
		Left err -> return (Left err)
		Right h -> do
			authed <- runErrorT (authenticate h (socketAuthenticators opts))
			case authed of
				Left err -> do
					hClose h
					return (Left err)
				Right False -> do
					hClose h
					return (Left (SocketError "Authentication failed"))
				Right True -> do
					serial <- newIORef (Serial 1)
					readLock <- newMVar ()
					writeLock <- newMVar ()
					return (Right (Socket addr h serial readLock writeLock))

-- | Close an open 'Socket'. Once closed, the 'Socket' is no longer valid and
-- must not be used.
close :: Socket -> IO ()
close = hClose . socketHandle

-- | Send a single message, with a generated 'Serial'. The second parameter
-- exists to prevent race conditions when registering a reply handler; it
-- receives the serial the message /will/ be sent with, before it&#8217;s
-- actually sent.
--
-- Only one message may be sent at a time; if multiple threads attempt to
-- send messages in parallel, one will block until after the other has
-- finished.
send :: Message msg => Socket -> msg -> (Serial -> IO a) -> IO (Either SocketError a)
send sock msg io = do
	serial <- nextSerial sock
	case marshalMessage LittleEndian serial msg of
		Right bytes -> do
			a <- io serial
			tried <- Control.Exception.try (withMVar
				(socketWriteLock sock)
				(\_ -> Data.ByteString.hPut (socketHandle sock) bytes))
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
-- Only one message may be received at a time; if multiple threads attempt
-- to receive messages in parallel, one will block until after the other has
-- finished.
receive :: Socket -> IO (Either SocketError ReceivedMessage)
receive sock = do
	tried <- Control.Exception.try (withMVar
		(socketReadLock sock)
		-- TODO: instead of fromIntegral, unify types
		(\_ -> unmarshalMessageM (Data.ByteString.hGet (socketHandle sock) . fromIntegral)))
	case tried of
		Left err -> return (Left (SocketError (show (err :: IOException))))
		Right unmarshaled -> case unmarshaled of
			Left err -> return (Left (SocketError ("Error reading message from socket: " ++ show err)))
			Right msg -> return (Right msg)

connectTransport :: [Transport] -> Address -> SocketM Handle
connectTransport transports addr = loop transports where
	method = addressMethod addr
	loop [] = socketError ("Unknown address method: " ++ show method)
	loop ((Transport m io):ts) = if m == method
		then io addr
		else loop ts

-- | TODO
transportUnix :: Transport
transportUnix = Transport "unix" $ \a -> let
	params = addressParameters a
	param key = Data.Map.lookup key params
	
	tooMany = "Only one of 'path' or 'abstract' may be specified for the\
	          \ 'unix' transport."
	tooFew = "One of 'path' or 'abstract' must be specified for the\
	         \ 'unix' transport."
	
	path = case (param "path", param "abstract") of
		(Just _, Just _) -> socketError tooMany
		(Nothing, Nothing) -> socketError tooFew
		(Just x, Nothing) -> return (Char8.unpack x)
		(Nothing, Just x) -> return ('\x00' : Char8.unpack x)
	
	getHandle = do
		port <- fmap Network.UnixSocket path
		socketIO (Network.connectTo "localhost" port)
	
	in getHandle >>= connectHandle

-- | TODO
transportTCP :: Transport
transportTCP = Transport "tcp" $ \a -> let
	params = addressParameters a
	param key = Data.Map.lookup key params
	
	getHandle = do
		port <- getPort
		family <- getFamily
		addrs <- socketIO (getAddresses family)
		sock <- openSocket port addrs
		socketIO (Network.Socket.socketToHandle sock System.IO.ReadWriteMode)
	hostname = maybe "localhost" Char8.unpack (param "host")
	unknownFamily x = "Unknown socket family for TCP transport: " ++ show x
	getFamily = case param "family" of
		Just "ipv4" -> return Network.Socket.AF_INET
		Just "ipv6" -> return Network.Socket.AF_INET6
		Nothing     -> return Network.Socket.AF_UNSPEC
		Just x      -> socketError (unknownFamily x)
	missingPort = "TCP transport requires the `port' parameter."
	badPort x = "Invalid socket port for TCP transport: " ++ show x
	getPort = case param "port" of
		Nothing -> socketError missingPort
		Just x -> case readPortNumber (Char8.unpack x) of
			Just port -> return port
			Nothing -> socketError (badPort x)

	getAddresses family = do
		let hints = Network.Socket.defaultHints
			{ Network.Socket.addrFlags = [Network.Socket.AI_ADDRCONFIG]
			, Network.Socket.addrFamily = family
			, Network.Socket.addrSocketType = Network.Socket.Stream
			}
		-- TODO: catch exception? can this call fail?
		Network.Socket.getAddrInfo (Just hints) (Just hostname) Nothing

	setPort port (Network.Socket.SockAddrInet  _ x)     = Network.Socket.SockAddrInet port x
	setPort port (Network.Socket.SockAddrInet6 _ x y z) = Network.Socket.SockAddrInet6 port x y z
	setPort _    addr                                   = addr

	openSocket _ [] = socketError ("Failed to open socket to address " ++ show a)
	openSocket port (addr:addrs) = do
		tried <- liftIO (Control.Exception.try (openSocket' port addr))
		case tried :: Either IOException Network.Socket.Socket of
			Left err -> case err :: IOException of
				_ -> openSocket port addrs 
			Right sock -> return sock
	openSocket' port addr = do
		sock <- Network.Socket.socket (Network.Socket.addrFamily addr)
		                  (Network.Socket.addrSocketType addr)
		                  (Network.Socket.addrProtocol addr)
		Network.Socket.connect sock (setPort port (Network.Socket.addrAddress addr))
		return sock
	
	in getHandle >>= connectHandle

connectHandle :: System.IO.Handle -> SocketM Handle
connectHandle h = socketIO $ do
	System.IO.hSetBuffering h System.IO.NoBuffering
	System.IO.hSetBinaryMode h True
	return h

authenticate :: Handle
             -> [Authenticator]
             -> SocketM Bool
authenticate h authenticators = go where
	go = do
		socketIO (Data.ByteString.hPut h (Data.ByteString.pack [0]))
		loop authenticators
	loop [] = return False
	loop ((Authenticator auth):next) = do
		success <- auth h
		if success
			then return True
			else loop next

type AuthM = ReaderT Handle SocketM

putLine :: String -> AuthM ()
putLine line = do
	h <- ask
	lift (socketIO (Data.ByteString.hPut h (Char8.pack (line ++ "\r\n"))))

getLine :: AuthM String
getLine = do
	h <- ask
	let getchr = Char8.head `fmap` Data.ByteString.hGet h 1
	lift (socketIO (do
		raw <- readUntil "\r\n" getchr
		return (dropEnd 2 raw)))

authExternal :: Authenticator
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
