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
	( Transport(..)
	, TransportOpen(..)
	, TransportListen(..)
	, TransportError(..)
	, SocketTransport
	) where

import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import qualified Network.Socket
import           Network.Socket (Socket)
import           Network.Socket.ByteString (sendAll, recv)

import           DBus
import           DBus.Util (readPortNumber)

-- | A Transport can exchange bytes with a remote peer.
class Transport t where
	-- | Additional options that this transport type may use when establishing
	-- a connection.
	data TransportOptions t :: *
	
	-- | Default values for this transport's options.
	transportDefaultOptions :: TransportOptions t
	
	-- | Send a 'ByteString' over the transport.
	transportPut :: t -> ByteString -> IO ()
	
	-- | Receive a 'ByteString' of the given size from the transport. The
	-- transport should block until sufficient bytes are available, and
	-- only return fewer than the requested amount if there will not be
	-- any more data.
	transportGet :: t -> Int -> IO ByteString
	
	-- | Close an open transport, and release any associated resources
	-- or handles.
	transportClose :: t -> IO ()

-- | A TransportOpen can open a connection to a remote peer.
class Transport t => TransportOpen t where
	-- | Open a connection to the given address, using the given options.
	--
	-- Returns a 'TransportError' if the connection could not be
	-- established.
	transportOpen :: TransportOptions t -> Address -> IO (Either TransportError t)

-- | A TransportListen can listen for and accept connections from remote peers.
class Transport t => TransportListen t where
	-- | Used for transports that listen on a port or address.
	data TransportListener t :: *
	
	-- | Begin listening for connections on the given address, using the
	-- given options.
	--
	-- Returns a 'TransportError' if it's not possible to listen at that
	-- address (for example, if the port is already in use).
	transportListen :: TransportOptions t -> Address -> IO (Either TransportError (TransportListener t))
	
	-- | Accept a new connection.
	--
	-- Returns a 'TransportError' if some error happens before the
	-- transport is ready to exchange bytes (for example, an authentication
	-- error).
	transportAccept :: TransportListener t -> IO (Either TransportError t)

data TransportError = TransportError String
	deriving (Eq, Show)

-- | Supports connecting over UNIX or TCP sockets.
--
-- UNIX sockets are similar to pipes, but exist as special files in the
-- filesystem. On Linux, /abstract sockets/ have a path-like address, but do
-- not actually have entries in the filesystem.
--
-- TCP sockets may use either IPv4 or IPv6.
newtype SocketTransport = SocketTransport Socket

instance Transport SocketTransport where
	data TransportOptions SocketTransport = SocketTransportOptions
	transportDefaultOptions = SocketTransportOptions
	transportPut (SocketTransport s) = sendAll s
	transportGet (SocketTransport s) = recv s
	transportClose (SocketTransport s) = Network.Socket.sClose s

instance TransportOpen SocketTransport where
	transportOpen _ a = case Char8.unpack (addressMethod a) of
		"unix" -> openUnix (addressParameters a)
		"tcp" -> openTcp a (addressParameters a)
		method -> return (Left (TransportError ("Unknown address method: " ++ show method)))

openUnix :: Map.Map ByteString ByteString -> IO (Either TransportError SocketTransport)
openUnix params = go where
	param key = Map.lookup (Char8.pack key) params
	
	tooMany = "Only one of 'path' or 'abstract' may be specified for the\
	          \ 'unix' transport."
	tooFew = "One of 'path' or 'abstract' must be specified for the\
	         \ 'unix' transport."
	
	path = case (param "path", param "abstract") of
		(Just _, Just _) -> Left tooMany
		(Nothing, Nothing) -> Left tooFew
		(Just x, Nothing) -> Right (Char8.unpack x)
		(Nothing, Just x) -> Right ('\x00' : Char8.unpack x)
	
	go = case path of
		Left err -> return (Left (TransportError err))
		Right p -> do
			sock <- Network.Socket.socket
				Network.Socket.AF_UNIX
				Network.Socket.Stream
				Network.Socket.defaultProtocol
			Network.Socket.connect sock (Network.Socket.SockAddrUnix p)
			return (Right (SocketTransport sock))

openTcp :: Address -> Map.Map ByteString ByteString -> IO (Either TransportError SocketTransport)
openTcp a params = go where
	param key = Map.lookup (Char8.pack key) params
	
	hostname = maybe "localhost" Char8.unpack (param "host")
	unknownFamily x = "Unknown socket family for TCP transport: " ++ show x
	getFamily = case fmap Char8.unpack (param "family") of
		Just "ipv4" -> Right Network.Socket.AF_INET
		Just "ipv6" -> Right Network.Socket.AF_INET6
		Nothing     -> Right Network.Socket.AF_UNSPEC
		Just x      -> Left (TransportError (unknownFamily x))
	missingPort = "TCP transport requires the `port' parameter."
	badPort x = "Invalid socket port for TCP transport: " ++ show x
	getPort = case param "port" of
		Nothing -> Left (TransportError missingPort)
		Just x -> case readPortNumber (Char8.unpack x) of
			Just port -> Right port
			Nothing -> Left (TransportError (badPort x))

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

	openSocket _ [] = return (Left (TransportError ("Failed to open socket to address " ++ show a)))
	openSocket port (addr:addrs) = do
		tried <- Control.Exception.try (openSocket' port addr)
		case tried :: Either IOException Network.Socket.Socket of
			Left err -> case err :: IOException of
				_ -> openSocket port addrs
			Right sock -> return (Right sock)
	openSocket' port addr = do
		sock <- Network.Socket.socket (Network.Socket.addrFamily addr)
		                  (Network.Socket.addrSocketType addr)
		                  (Network.Socket.addrProtocol addr)
		Network.Socket.connect sock (setPort port (Network.Socket.addrAddress addr))
		return sock
	
	go = case getPort of
		Left err -> return (Left err)
		Right port -> case getFamily of
			Left err -> return (Left err)
			Right family -> do
				addrs <- getAddresses family
				eSock <- openSocket port addrs
				case eSock of
					Left err -> return (Left err)
					Right sock -> return (Right (SocketTransport sock))
