{-# LANGUAGE DeriveDataTypeable #-}
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

-- | Support for defining custom transport mechanisms. Most users will not
-- need to care about the types defined in this module.
module DBus.Transport
	(
	-- * Transports
	  Transport(..)
	, TransportOpen(..)
	, TransportListen(..)
	
	-- * Transport errors
	, TransportError
	, transportError
	, transportErrorMessage
	, transportErrorAddress
	
	-- * Socket transport
	, SocketTransport
	, socketTransportOptionBacklog
	, socketTransportCredentials
	) where

import           Control.Exception
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.Map as Map
import           Data.Typeable (Typeable)
import           Foreign.C (CUInt)
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString (sendAll, recv)
import qualified System.Info

import qualified Data.Serialize.Builder as Builder

import           DBus

-- | Thrown from transport methods when an error occurs.
data TransportError = TransportError
	{ transportErrorMessage :: String
	, transportErrorAddress :: Maybe Address
	}
	deriving (Eq, Show, Typeable)

instance Exception TransportError

transportError :: String -> TransportError
transportError msg = TransportError msg Nothing

-- | A 'Transport' can exchange bytes with a remote peer.
class Transport t where
	-- | Additional options that this transport type may use when establishing
	-- a connection.
	data TransportOptions t :: *
	
	-- | Default values for this transport's options.
	transportDefaultOptions :: TransportOptions t
	
	-- | Send a 'ByteString' over the transport.
	--
	-- Throws a 'TransportError' if an error occurs.
	transportPut :: t -> ByteString -> IO ()
	
	-- | Receive a 'ByteString' of the given size from the transport. The
	-- transport should block until sufficient bytes are available, and
	-- only return fewer than the requested amount if there will not be
	-- any more data.
	--
	-- Throws a 'TransportError' if an error occurs.
	transportGet :: t -> Int -> IO ByteString
	
	-- | Close an open transport, and release any associated resources
	-- or handles.
	transportClose :: t -> IO ()

-- | A 'Transport' which can open a connection to a remote peer.
class Transport t => TransportOpen t where
	-- | Open a connection to the given address, using the given options.
	--
	-- Throws a 'TransportError' if the connection could not be
	-- established.
	transportOpen :: TransportOptions t -> Address -> IO t

-- | A 'Transport' which can listen for and accept connections from remote
-- peers.
class Transport t => TransportListen t where
	-- | Used for transports that listen on a port or address.
	data TransportListener t :: *
	
	-- | Begin listening for connections on the given address, using the
	-- given options.
	--
	-- Throws a 'TransportError' if it's not possible to listen at that
	-- address (for example, if the port is already in use).
	transportListen :: TransportOptions t -> Address -> IO (TransportListener t)
	
	-- | Accept a new connection.
	--
	-- Throws a 'TransportError' if some error happens before the
	-- transport is ready to exchange bytes.
	transportAccept :: TransportListener t -> IO t
	
	-- | Close an open listener.
	transportListenerClose :: TransportListener t -> IO ()
	
	-- | Get the address to use to connect to a listener.
	transportListenerAddress :: TransportListener t -> Address
	
	-- | Get the UUID allocated to this transport listener.
	--
	-- See 'randomUUID'.
	transportListenerUUID :: TransportListener t -> UUID

-- | Supports connecting over Unix or TCP sockets.
--
-- Unix sockets are similar to pipes, but exist as special files in the
-- filesystem. On Linux, /abstract sockets/ have a path-like address, but do
-- not actually have entries in the filesystem.
--
-- TCP sockets may use either IPv4 or IPv6.
data SocketTransport = SocketTransport (Maybe Address) Socket

instance Transport SocketTransport where
	data TransportOptions SocketTransport = SocketTransportOptions
		{
		-- | The maximum size of the connection queue for a listening
		-- socket.
		  socketTransportOptionBacklog :: Int
		}
	transportDefaultOptions = SocketTransportOptions 30
	transportPut (SocketTransport addr s) bytes = catchIOException addr (sendAll s bytes)
	transportGet (SocketTransport addr s) n = catchIOException addr (recvLoop s n)
	transportClose (SocketTransport addr s) = catchIOException addr (sClose s)

recvLoop :: Socket -> Int -> IO ByteString
recvLoop s = loop Builder.empty where
	chunkSize = 4096
	loop acc n = if n > chunkSize
		then do
			chunk <- recv s chunkSize
			let builder = Builder.append acc (Builder.fromByteString chunk)
			loop builder (n - Data.ByteString.length chunk)
		else do
			chunk <- recv s n
			case Data.ByteString.length chunk of
				-- Unexpected end of connection; maybe the remote end went away.
				-- Return what we've got so far.
				0 -> return (Builder.toByteString acc)
				
				len -> do
					let builder = Builder.append acc (Builder.fromByteString chunk)
					if len == n
						then return (Builder.toByteString builder)
						else loop builder (n - Data.ByteString.length chunk)

instance TransportOpen SocketTransport where
	transportOpen _ a = case addressMethod a of
		"unix" -> openUnix a
		"tcp" -> openTcp a
		method -> throwIO (transportError ("Unknown address method: " ++ show method))
			{ transportErrorAddress = Just a
			}

instance TransportListen SocketTransport where
	data TransportListener SocketTransport = SocketTransportListener Address UUID Socket
	transportListen opts a = do
		uuid <- randomUUID
		(a', sock) <- case addressMethod a of
			"unix" -> listenUnix uuid a opts
			"tcp" -> listenTcp uuid a opts
			method -> throwIO (transportError ("Unknown address method: " ++ show method))
				{ transportErrorAddress = Just a
				}
		return (SocketTransportListener a' uuid sock)
	transportAccept (SocketTransportListener a _ s) = catchIOException (Just a) $ do
		(s', _) <- accept s
		return (SocketTransport Nothing s')
	transportListenerClose (SocketTransportListener a _ s) = catchIOException (Just a) (sClose s)
	transportListenerAddress (SocketTransportListener a _ _) = a
	transportListenerUUID (SocketTransportListener _ uuid _) = uuid

-- | Returns the processID, userID, and groupID of the socket's peer.
--
-- See 'getPeerCred'.
socketTransportCredentials :: SocketTransport -> IO (CUInt, CUInt, CUInt)
socketTransportCredentials (SocketTransport a s) = catchIOException a (getPeerCred s)

openUnix :: Address -> IO SocketTransport
openUnix transportAddr = go where
	params = addressParameters transportAddr
	param key = Map.lookup key params
	
	tooMany = "Only one of 'path' or 'abstract' may be specified for the\
	          \ 'unix' transport."
	tooFew = "One of 'path' or 'abstract' must be specified for the\
	         \ 'unix' transport."
	
	path = case (param "path", param "abstract") of
		(Just x, Nothing) -> Right x
		(Nothing, Just x) -> Right ('\x00' : x)
		(Nothing, Nothing) -> Left tooFew
		_ -> Left tooMany
	
	go = case path of
		Left err -> throwIO (transportError err)
			{ transportErrorAddress = Just transportAddr
			}
		Right p -> catchIOException (Just transportAddr) $ bracketOnError
			(socket AF_UNIX Stream defaultProtocol)
			sClose
			(\sock -> do
				connect sock (SockAddrUnix p)
				return (SocketTransport (Just transportAddr) sock))

openTcp :: Address -> IO SocketTransport
openTcp transportAddr = go where
	params = addressParameters transportAddr
	param key = Map.lookup key params
	
	hostname = maybe "localhost" id (param "host")
	unknownFamily x = "Unknown socket family for TCP transport: " ++ show x
	getFamily = case param "family" of
		Just "ipv4" -> Right AF_INET
		Just "ipv6" -> Right AF_INET6
		Nothing     -> Right AF_UNSPEC
		Just x      -> Left (unknownFamily x)
	missingPort = "TCP transport requires the `port' parameter."
	badPort x = "Invalid socket port for TCP transport: " ++ show x
	getPort = case param "port" of
		Nothing -> Left missingPort
		Just x -> case readPortNumber x of
			Just port -> Right port
			Nothing -> Left (badPort x)
	
	getAddresses family_ = getAddrInfo (Just (defaultHints
		{ addrFlags = [AI_ADDRCONFIG]
		, addrFamily = family_
		, addrSocketType = Stream
		})) (Just hostname) Nothing
	
	openSocket [] = throwIO (transportError "openTcp: no addresses")
		{ transportErrorAddress = Just transportAddr
		}
	openSocket (addr:addrs) = do
		tried <- Control.Exception.try $ bracketOnError
			(socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
			sClose
			(\sock -> do
				connect sock (addrAddress addr)
				return sock)
		case tried of
			Left err -> case addrs of
				[] -> throwIO (transportError (show (err :: IOException)))
					{ transportErrorAddress = Just transportAddr
					}
				_ -> openSocket addrs
			Right sock -> return sock
	
	go = case getPort of
		Left err -> throwIO (transportError err)
			{ transportErrorAddress = Just transportAddr
			}
		Right port -> case getFamily of
			Left err -> throwIO (transportError err)
				{ transportErrorAddress = Just transportAddr
				}
			Right family_ -> catchIOException (Just transportAddr) $ do
				addrs <- getAddresses family_
				sock <- openSocket (map (setPort port) addrs)
				return (SocketTransport (Just transportAddr) sock)

listenUnix :: UUID -> Address -> TransportOptions SocketTransport -> IO (Address, Socket)
listenUnix uuid origAddr opts = getPath >>= go where
	params = addressParameters origAddr
	param key = Map.lookup key params
	
	tooMany = "Only one of 'abstract', 'path', or 'tmpdir' may be\
	          \ specified for the 'unix' transport."
	tooFew = "One of 'abstract', 'path', or 'tmpdir' must be specified\
	         \ for the 'unix' transport."
	
	getPath = case (param "abstract", param "path", param "tmpdir") of
		(Just path, Nothing, Nothing) -> let
			addr = address_ "unix"
				[ ("abstract", path)
				, ("guid", formatUUID uuid)
				]
			in return (Right (addr, '\x00' : path))
		(Nothing, Just path, Nothing) -> let
			addr = address_ "unix"
				[ ("path", path)
				, ("guid", formatUUID uuid)
				]
			in return (Right (addr, path))
		(Nothing, Nothing, Just x) -> do
			let fileName = x ++ "/haskell-dbus-" ++ formatUUID uuid
			
			-- Abstract paths are supported on Linux, but not on
			-- other Unix-like systems.
			let (addrParams, path) = if System.Info.os == "linux"
				then ([("abstract", fileName)], '\x00' : fileName)
				else ([("path", fileName)], fileName)
			
			let addr = address_ "unix" (addrParams ++ [("guid", formatUUID uuid)])
			return (Right (addr, path))
		(Nothing, Nothing, Nothing) -> return (Left tooFew)
		_ -> return (Left tooMany)
	
	go path = case path of
		Left err -> throwIO (transportError err)
			{ transportErrorAddress = Just origAddr
			}
		Right (addr, p) -> catchIOException (Just origAddr) $ bracketOnError
			(socket AF_UNIX Stream defaultProtocol)
			sClose
			(\sock -> do
				bindSocket sock (SockAddrUnix p)
				Network.Socket.listen sock (socketTransportOptionBacklog opts)
				return (addr, sock))

listenTcp :: UUID -> Address -> TransportOptions SocketTransport -> IO (Address, Socket)
listenTcp uuid origAddr opts = go where
	params = addressParameters origAddr
	param key = Map.lookup key params
	
	unknownFamily x = "Unknown socket family for TCP transport: " ++ show x
	getFamily = case param "family" of
		Just "ipv4" -> Right AF_INET
		Just "ipv6" -> Right AF_INET6
		Nothing     -> Right AF_UNSPEC
		Just x      -> Left (unknownFamily x)
	
	badPort x = "Invalid socket port for TCP transport: " ++ show x
	getPort = case param "port" of
		Nothing -> Right 0
		Just x -> case readPortNumber x of
			Just port -> Right port
			Nothing -> Left (badPort x)
	
	paramBind = case param "bind" of
		Just "*" -> Nothing
		Just x -> Just x
		Nothing -> case param "host" of
			Just x -> Just x
			Nothing -> Just "localhost"
	
	getAddresses family_ = getAddrInfo (Just (defaultHints
		{ addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]
		, addrFamily = family_
		, addrSocketType = Stream
		})) paramBind Nothing
	
	bindAddrs _ [] = throwIO (transportError "listenTcp: no addresses")
		{ transportErrorAddress = Just origAddr
		}
	bindAddrs sock (addr:addrs) = do
		tried <- Control.Exception.try (bindSocket sock (addrAddress addr))
		case tried of
			Left err -> case addrs of
				[] -> throwIO (transportError (show (err :: IOException)))
					{ transportErrorAddress = Just origAddr
					}
				_ -> bindAddrs sock addrs
			Right _ -> return ()
	
	sockAddr (PortNum port) = address_ "tcp" p where
		p = baseParams ++ hostParam ++ familyParam
		baseParams =
			[ ("port", show port)
			, ("guid", formatUUID uuid)
			]
		hostParam = case param "host" of
			Just x -> [("host", x)]
			Nothing -> []
		familyParam = case param "family" of
			Just x -> [("family", x)]
			Nothing -> []
	
	go = case getPort of
		Left err -> throwIO (transportError err)
			{ transportErrorAddress = Just origAddr
			}
		Right port -> case getFamily of
			Left err -> throwIO (transportError err)
				{ transportErrorAddress = Just origAddr
				}
			Right family_ -> catchIOException (Just origAddr) $ do
				sockAddrs <- getAddresses family_
				bracketOnError
					(socket family_ Stream defaultProtocol)
					sClose
					(\sock -> do
						setSocketOption sock ReuseAddr 1
						bindAddrs sock (map (setPort port) sockAddrs)
						
						Network.Socket.listen sock (socketTransportOptionBacklog opts)
						sockPort <- socketPort sock
						return (sockAddr sockPort, sock))

catchIOException :: Maybe Address -> IO a -> IO a
catchIOException addr io = do
	tried <- try io
	case tried of
		Right a -> return a
		Left err -> throwIO (transportError (show (err :: IOException)))
			{ transportErrorAddress = addr
			}

address_ :: String -> [(String, String)] -> Address
address_ method params = addr where
	Just addr = address method (Map.fromList params)

setPort :: PortNumber -> AddrInfo -> AddrInfo
setPort port info = case addrAddress info of
	(SockAddrInet  _ x) -> info { addrAddress = SockAddrInet port x }
	(SockAddrInet6 _ x y z) -> info { addrAddress = SockAddrInet6 port x y z }
	_ -> info

readPortNumber :: String -> Maybe PortNumber
readPortNumber s = do
	case dropWhile (\c -> c >= '0' && c <= '9') s of
		[] -> return ()
		_ -> Nothing
	let word = read s :: Integer
	if word > 0 && word <= 65535
		then Just (fromInteger word)
		else Nothing
