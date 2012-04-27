{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
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

module DBusTests.Socket (test_Socket) where

import           Test.Chell

import           Control.Concurrent
import           Control.Exception (IOException, finally, try)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map
import qualified Data.Text as T
import qualified Network as N
import qualified Network.Socket as NS
import           System.IO
import           System.Random (randomIO)

import qualified Data.UUID as UUID

import           DBus
import           DBus.Socket
import           DBus.Util (readUntil)

test_Socket :: Suite
test_Socket = suite "Socket"
	[ test_ConnectUnix
	, test_ConnectTcp_IPv4
	, skipWhen noIPv6 test_ConnectTcp_IPv6
	]

test_ConnectUnix :: Suite
test_ConnectUnix = assertions "connect-unix" $ do
	(addr, networkSocket) <- listenRandomUnix
	connectResult <- withDummyServer networkSocket (DBus.Socket.connect addr)
	case connectResult of
		Left err -> $(Test.Chell.fail) (T.pack ("expected successful connection, got error: " ++ show err))
		Right s -> $assert (equal (socketAddress s) addr)

test_ConnectTcp_IPv4 :: Suite
test_ConnectTcp_IPv4 = assertions "connect-tcp-ipv4" $ do
	(addr, networkSocket) <- listenRandomIPv4
	connectResult <- withDummyServer networkSocket (DBus.Socket.connect addr)
	case connectResult of
		Left err -> $(Test.Chell.fail) (T.pack ("expected successful connection, got error: " ++ show err))
		Right s -> $assert (equal (socketAddress s) addr)

test_ConnectTcp_IPv6 :: Suite
test_ConnectTcp_IPv6 = assertions "connect-tcp-ipv6" $ do
	(addr, networkSocket) <- listenRandomIPv6
	connectResult <- withDummyServer networkSocket (DBus.Socket.connect addr)
	case connectResult of
		Left err -> $(Test.Chell.fail) (T.pack ("expected successful connection, got error: " ++ show err))
		Right s -> $assert (equal (socketAddress s) addr)

readChar8 :: Handle -> IO Char
readChar8 h = fmap (Char8.head) (Data.ByteString.hGet h 1)

newtype DummyServer = DummyServer (MVar ())

withDummyServer :: MonadIO m => N.Socket -> IO a -> m a
withDummyServer sock io = liftIO go where
	go = do
		mvar <- newEmptyMVar
		_ <- forkIO (run mvar)
		finally io (putMVar mvar ())
	
	run mvar = do
		(h, _, _) <- N.accept sock
		hSetBuffering h NoBuffering
		
		-- authentication sends '\x00', then AUTH EXTERNAL ...
		_ <- readChar8 h
		_ <- readUntil "\r\n" (readChar8 h)
		Data.ByteString.hPut h "OK blah blah blah\r\n"
		
		_ <- readMVar mvar
		hClose h
		N.sClose sock

listenRandomUnix :: MonadIO m => m (Address, N.Socket)
listenRandomUnix = liftIO $ do
	uuid <- liftIO randomIO
	let sockAddr = NS.SockAddrUnix ('\x00' : UUID.toString uuid)
	
	sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
	NS.bindSocket sock sockAddr
	NS.listen sock 1
	
	let Just addr = address "unix" (Data.Map.fromList
		[ ("abstract", Char8.pack (UUID.toString uuid))
		])
	return (addr, sock)

listenRandomIPv4 :: MonadIO m => m (Address, N.Socket)
listenRandomIPv4 = liftIO $ do
	hostAddr <- NS.inet_addr "127.0.0.1"
	let sockAddr = NS.SockAddrInet 0 hostAddr
	
	sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
	NS.bindSocket sock sockAddr
	NS.listen sock 1
	
	sockPort <- NS.socketPort sock
	let Just addr = address "tcp" (Data.Map.fromList
		[ ("family", "ipv4")
		, ("host", "localhost")
		, ("port", Char8.pack (show (toInteger sockPort)))
		])
	return (addr, sock)

listenRandomIPv6 :: MonadIO m => m (Address, N.Socket)
listenRandomIPv6 = liftIO $ do
	addrs <- NS.getAddrInfo Nothing (Just "::1") Nothing
	let sockAddr = case addrs of
		[] -> error "listenRandomIPv6: no address for localhost?"
		a:_ -> NS.addrAddress a
	
	sock <- NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol
	NS.bindSocket sock sockAddr
	NS.listen sock 1
	
	sockPort <- NS.socketPort sock
	let Just addr = address "tcp" (Data.Map.fromList
		[ ("family", "ipv6")
		, ("host", "localhost")
		, ("port", Char8.pack (show (toInteger sockPort)))
		])
	return (addr, sock)

noIPv6 :: IO Bool
noIPv6 = do
	tried <- try (NS.getAddrInfo Nothing (Just "::1") Nothing)
	case (tried :: Either IOException [NS.AddrInfo]) of
		Left _ -> return True
		Right addrs -> return (null addrs)
