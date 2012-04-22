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

module DBus.Connection.Transport
	( Transport
	, transport
	, Socket
	, socket
	, socketPut
	, socketGet
	, socketClose
	, tcp
	, unix
	, connectTransport
	) where

import qualified Control.Exception
import           Control.Monad (unless)
import qualified Data.Binary.Get
import qualified Data.Binary.Put
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map
import           Data.Word (Word32)
import qualified Network
import qualified Network.Socket
import qualified System.IO
import           Text.ParserCombinators.Parsec

import           DBus.Address
import           DBus.Connection.Error

-- | A 'Transport' is anything which can send and receive bytestrings,
-- typically via a socket.
data Transport = Transport ByteString (Address -> IO Socket)

data Socket = Socket
	(ByteString -> IO ())
	(Word32 -> IO ByteString)
	(IO ())

transport :: ByteString -> (Address -> IO Socket) -> Transport
transport = Transport

socket :: (ByteString -> IO ()) -> (Word32 -> IO ByteString) -> IO () -> Socket
socket = Socket

socketPut :: Socket -> ByteString -> IO ()
socketPut (Socket x _ _) = x

socketGet :: Socket -> Word32 -> IO ByteString
socketGet (Socket _ x _) = x

socketClose :: Socket -> IO ()
socketClose (Socket _ _ x) = x

connectTransport :: [Transport] -> Address -> IO (Maybe Socket)
connectTransport transports addr = loop transports where
	m = addressMethod addr
	loop [] = return Nothing
	loop ((Transport n io):ts) = if n == m
		then fmap Just (io addr)
		else loop ts

connectHandle :: System.IO.Handle -> IO Socket
connectHandle h = do
	System.IO.hSetBuffering h System.IO.NoBuffering
	System.IO.hSetBinaryMode h True
	return (Socket
		(Data.ByteString.hPut h)
		(Data.ByteString.hGet h . fromIntegral)
		(System.IO.hClose h))

unix :: Transport
unix = transport "unix" connectUNIX

connectUNIX :: Address -> IO Socket
connectUNIX a = getHandle >>= connectHandle where
	params = addressParameters a
	param key = Data.Map.lookup key params
	
	tooMany = "Only one of `path' or `abstract' may be specified for the\
	          \ `unix' transport."
	tooFew = "One of `path' or `abstract' must be specified for the\
	         \ `unix' transport."
	
	path = case (param "path", param "abstract") of
		(Just _, Just _) -> connectionError tooMany
		(Nothing, Nothing) -> connectionError tooFew
		(Just x, Nothing) -> return (Char8.unpack x)
		(Nothing, Just x) -> return ('\x00' : Char8.unpack x)
	
	getHandle = do
		port <- fmap Network.UnixSocket path
		Network.connectTo "localhost" port

tcp :: Transport
tcp = Transport "tcp" connectTCP

connectTCP :: Address -> IO Socket
connectTCP a = getHandle >>= connectHandle where
	params = addressParameters a
	param key = Data.Map.lookup key params
	
	getHandle = do
		port <- getPort
		family <- getFamily
		addrs <- getAddresses family
		sock<- openSocket port addrs
		Network.Socket.socketToHandle sock System.IO.ReadWriteMode
	hostname = maybe "localhost" Char8.unpack (param "host")
	unknownFamily x = concat ["Unknown socket family for TCP transport: ", show x]
	getFamily = case param "family" of
		Just "ipv4" -> return Network.Socket.AF_INET
		Just "ipv6" -> return Network.Socket.AF_INET6
		Nothing     -> return Network.Socket.AF_UNSPEC
		Just x      -> connectionError (unknownFamily x)
	missingPort = "TCP transport requires the `port' parameter."
	badPort x = concat ["Invalid socket port for TCP transport: ", show x]
	getPort = case param "port" of
		Nothing -> connectionError missingPort
		Just x -> case parse parseWord16 "" (Char8.unpack x) of
			Right x' -> return (Network.Socket.PortNum x')
			Left  _  -> connectionError (badPort x)

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
		return (Data.Binary.Get.runGet Data.Binary.Get.getWord16host (Data.Binary.Put.runPut (Data.Binary.Put.putWord16be word)))

	getAddresses family = do
		let hints = Network.Socket.defaultHints
			{ Network.Socket.addrFlags = [Network.Socket.AI_ADDRCONFIG]
			, Network.Socket.addrFamily = family
			, Network.Socket.addrSocketType = Network.Socket.Stream
			}
		Network.Socket.getAddrInfo (Just hints) (Just hostname) Nothing

	setPort port (Network.Socket.SockAddrInet  _ x)     = Network.Socket.SockAddrInet port x
	setPort port (Network.Socket.SockAddrInet6 _ x y z) = Network.Socket.SockAddrInet6 port x y z
	setPort _    addr                       = addr

	openSocket _ [] = connectionError ("Failed to open socket to address " ++ show a)
	openSocket port (addr:addrs) = Control.Exception.catch (openSocket' port addr) $
		\(Control.Exception.SomeException _) -> openSocket port addrs
	openSocket' port addr = do
		sock <- Network.Socket.socket (Network.Socket.addrFamily addr)
		                  (Network.Socket.addrSocketType addr)
		                  (Network.Socket.addrProtocol addr)
		Network.Socket.connect sock . setPort port . Network.Socket.addrAddress $ addr
		return sock
