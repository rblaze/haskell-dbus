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
import qualified Control.Exception
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Reader
import           Data.Binary.Get (runGet, getWord16host)
import           Data.Binary.Put (runPut, putWord16be)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord)
import           Data.IORef
import qualified Data.Map
import           Data.Word (Word32)
import qualified Network
import qualified Network.Socket
import qualified System.IO
import qualified System.Posix.User
import           Text.Printf (printf)

import           Text.ParserCombinators.Parsec

import           DBus
import           DBus.Types (Serial(..))
import           DBus.Wire (unmarshalMessageM)
import           DBus.Util (readUntil, dropEnd)
import           DBus.Util.MonadError

-- | TODO
data SocketError = SocketError String
	deriving (Eq, Ord, Show)

type SocketM = ErrorT SocketError IO

socketError :: String -> SocketM a
socketError = throwErrorT . SocketError

-- | TODO
data Transport = Transport ByteString (Address -> SocketM Impl)

-- | TODO
newtype Authenticator = Authenticator (Impl -> SocketM Bool)

data Impl = Impl
	(Word32 -> SocketM ByteString)
	(ByteString -> SocketM ())
	(IO ())

-- | TODO
data Socket = Socket
	{ socketAddress :: Address
	, socketRead :: Word32 -> SocketM ByteString
	, socketWrite :: ByteString -> SocketM ()
	, socketClose :: IO ()
	, socketSerial :: IORef Serial
	, socketReadLock :: MVar ()
	, socketWriteLock :: MVar ()
	}

-- | TODO
data SocketOptions = SocketOptions
	{ socketTransports :: [Transport]
	, socketAuthenticators :: [Authenticator]
	}

defaultSocketOptions :: SocketOptions
defaultSocketOptions = undefined

connect :: Address -> IO (Either SocketError Socket)
connect = connectWith defaultSocketOptions

connectWith :: SocketOptions -> Address -> IO (Either SocketError Socket)
connectWith opts addr = do
	connected <- runErrorT (connectTransport (socketTransports opts) addr)
	case connected of
		Left err -> return (Left err)
		Right impl@(Impl sRead sWrite sClose) -> do
			authed <- runErrorT (authenticate impl (socketAuthenticators opts))
			case authed of
				Left err -> do
					sClose
					return (Left err)
				Right False -> do
					sClose
					return (Left (SocketError "Authentication failed"))
				Right True -> do
					serial <- newIORef (Serial 1)
					readLock <- newMVar ()
					writeLock <- newMVar ()
					return (Right (Socket addr sRead sWrite sClose serial readLock writeLock))

-- | Close an open 'Socket'. Once closed, the 'Socket' is no longer valid and
-- must not be used.
close :: Socket -> IO ()
close = socketClose

-- | Send a single message, with a generated 'Serial'. The second parameter
-- exists to prevent race conditions when registering a reply handler; it
-- receives the serial the message /will/ be sent with, before it&#8217;s
-- actually sent.
--
-- Only one message may be sent at a time; if multiple threads attempt to
-- send messages in parallel, one will block until after the other has
-- finished.
send :: Message msg => Socket -> msg -> (Serial -> IO a) -> IO (Either MarshalError a)
send sock msg io = do
	serial <- nextSerial sock
	case marshalMessage LittleEndian serial msg of
		Right bytes -> do
			a <- io serial
			-- TODO: catch IO errors and report via return value
			ret <- withMVar
				(socketWriteLock sock)
				(\_ -> runErrorT (socketWrite sock bytes))
			case ret of
				Left err -> undefined -- TODO: return (Left err)
				Right _ -> return (Right a)
		Left err -> return (Left err)

nextSerial :: Socket -> IO Serial
nextSerial sock = atomicModifyIORef
	(socketSerial sock)
	(\serial@(Serial x) -> (Serial (x + 1), serial))

-- | Receive the next message from the socket , blocking until one is available.
--
-- Only one message may be received at a time; if multiple threads attempt
-- to receive messages in parallel, one will block until after the other has
-- finished.
receive :: Socket -> IO (Either UnmarshalError ReceivedMessage)
receive sock = do
	ret <- withMVar
		(socketReadLock sock)
		(\_ -> runErrorT (unmarshalMessageM (socketRead sock)))
	case ret of
		Left err -> undefined -- TODO: return (Left err)
		Right unmarshaled -> return unmarshaled

connectTransport :: [Transport] -> Address -> SocketM Impl
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
		liftIO (Network.connectTo "localhost" port)
	
	in getHandle >>= connectHandle

-- | TODO
transportTCP :: Transport
transportTCP = Transport "tcp" $ \a -> let
	params = addressParameters a
	param key = Data.Map.lookup key params
	
	getHandle = do
		port <- getPort
		family <- getFamily
		addrs <- liftIO (getAddresses family)
		sock <- openSocket port addrs
		liftIO (Network.Socket.socketToHandle sock System.IO.ReadWriteMode)
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
		Just x -> case parse parseWord16 "" (Char8.unpack x) of
			Right x' -> return (Network.Socket.PortNum x')
			Left  _  -> socketError (badPort x)

	parseWord16 = do
		chars <- many1 digit
		eof
		let value = read chars :: Integer
		unless (value > 0 && value <= 65535) $
			-- Calling 'fail' is acceptable here, because Parsec 2
			-- offers no other error reporting mechanism, and
			-- implements 'fail'.
			fail "bad port"
		let word = fromIntegral value
		return (runGet getWord16host (runPut (putWord16be word)))

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
		mSock <- liftIO $ Control.Exception.catch
			(Just `fmap` openSocket' port addr)
			(\(Control.Exception.SomeException _) -> return Nothing)
		case mSock of
			Just sock -> return sock
			Nothing -> openSocket port addrs
	openSocket' port addr = do
		sock <- Network.Socket.socket (Network.Socket.addrFamily addr)
		                  (Network.Socket.addrSocketType addr)
		                  (Network.Socket.addrProtocol addr)
		Network.Socket.connect sock (setPort port (Network.Socket.addrAddress addr))
		return sock
	
	in getHandle >>= connectHandle

connectHandle :: System.IO.Handle -> SocketM Impl
connectHandle h = liftIO $ do
	System.IO.hSetBuffering h System.IO.NoBuffering
	System.IO.hSetBinaryMode h True
	return (Impl
		(liftIO . Data.ByteString.hGet h . fromIntegral)
		(liftIO . Data.ByteString.hPut h)
		(System.IO.hClose h))

authenticate :: Impl
             -> [Authenticator]
             -> SocketM Bool
authenticate impl authenticators = go where
	go = do
		let Impl _ put _ = impl
		put (Data.ByteString.pack [0])
		loop authenticators
	loop [] = return False
	loop ((Authenticator auth):next) = do
		success <- auth impl
		if success
			then return True
			else loop next

type AuthM = ReaderT Impl SocketM

putLine :: String -> AuthM ()
putLine line = do
	Impl _ put _ <- ask
	lift (put (Char8.pack (line ++ "\r\n")))

getLine :: AuthM String
getLine = do
	Impl get _ _ <- ask
	let getchr = Char8.head `fmap` get 1
	lift $ do
		raw <- readUntil "\r\n" getchr
		return (dropEnd 2 raw)

authExternal :: Authenticator
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
