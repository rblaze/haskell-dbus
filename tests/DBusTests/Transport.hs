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

module DBusTests.Transport (test_Transport) where

import           Test.Chell

import           Control.Concurrent
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString
import           Data.Function (fix)
import           Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified Network as N
import qualified Network.Socket as NS
import           Network.Socket.ByteString (sendAll, recv)
import           System.Directory (getTemporaryDirectory, removeFile)

import           DBus
import           DBus.Transport

import           DBusTests.Util

test_Transport :: Suite
test_Transport = suite "Transport"
	test_TransportOpen
	test_TransportListen
	test_TransportAccept
	test_TransportSendReceive
	test_HandleLostConnection

test_TransportOpen :: Suite
test_TransportOpen = suite "transportOpen"
	test_OpenUnknown
	test_OpenUnix
	test_OpenTcp

test_TransportListen :: Suite
test_TransportListen = suite "transportListen"
	test_ListenUnknown
	test_ListenUnix
	test_ListenTcp

test_TransportAccept :: Suite
test_TransportAccept = suite "transportAccept"
	test_AcceptSocket
	test_AcceptSocketClosed

test_OpenUnknown :: Test
test_OpenUnknown = assertions "unknown" $ do
	let Just addr = address "noexist" Map.empty
	$assert $ throwsEq
		((transportError "Unknown address method: \"noexist\"")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)

test_OpenUnix :: Suite
test_OpenUnix = suite "unix"
	test_OpenUnix_Path
	test_OpenUnix_Abstract
	test_OpenUnix_TooFew
	test_OpenUnix_TooMany
	test_OpenUnix_NotListening

test_OpenUnix_Path :: Test
test_OpenUnix_Path = assertions "path" $ do
	(addr, networkSocket) <- listenRandomUnixPath
	afterTest (N.sClose networkSocket)
	
	fdcountBefore <- countFileDescriptors
	
	t <- liftIO (transportOpen socketTransportOptions addr)
	liftIO (transportClose t)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenUnix_Abstract :: Test
test_OpenUnix_Abstract = assertions "abstract" $ do
	(addr, networkSocket) <- listenRandomUnixAbstract
	afterTest (N.sClose networkSocket)
	
	fdcountBefore <- countFileDescriptors
	
	t <- liftIO (transportOpen socketTransportOptions addr)
	liftIO (transportClose t)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenUnix_TooFew :: Test
test_OpenUnix_TooFew = assertions "too-few" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "unix" Map.empty
	$assert $ throwsEq
		((transportError "One of 'path' or 'abstract' must be specified for the 'unix' transport.")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenUnix_TooMany :: Test
test_OpenUnix_TooMany = assertions "too-many" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "unix" (Map.fromList
		[ ("path", "foo")
		, ("abstract", "bar")
		])
	$assert $ throwsEq
		((transportError "Only one of 'path' or 'abstract' may be specified for the 'unix' transport.")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenUnix_NotListening :: Test
test_OpenUnix_NotListening = assertions "not-listening" $ do
	fdcountBefore <- countFileDescriptors
	
	(addr, networkSocket) <- listenRandomUnixAbstract
	liftIO (NS.sClose networkSocket)
	$assert $ throwsEq
		((transportError "connect: does not exist (Connection refused)")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp :: Suite
test_OpenTcp = suite "tcp"
	test_OpenTcp_IPv4
	(skipWhen noIPv6 test_OpenTcp_IPv6)
	test_OpenTcp_Unknown
	test_OpenTcp_NoPort
	test_OpenTcp_InvalidPort
	test_OpenTcp_NoUsableAddresses
	test_OpenTcp_NotListening

test_OpenTcp_IPv4 :: Test
test_OpenTcp_IPv4 = assertions "ipv4" $ do
	(addr, networkSocket) <- listenRandomIPv4
	afterTest (N.sClose networkSocket)
	
	fdcountBefore <- countFileDescriptors
	
	t <- liftIO (transportOpen socketTransportOptions addr)
	liftIO (transportClose t)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp_IPv6 :: Test
test_OpenTcp_IPv6 = assertions "ipv6" $ do
	(addr, networkSocket) <- listenRandomIPv6
	afterTest (N.sClose networkSocket)
	
	fdcountBefore <- countFileDescriptors
	
	t <- liftIO (transportOpen socketTransportOptions addr)
	liftIO (transportClose t)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp_Unknown :: Test
test_OpenTcp_Unknown = assertions "unknown-family" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "noexist")
		, ("port", "1234")
		])
	$assert $ throwsEq
		((transportError "Unknown socket family for TCP transport: \"noexist\"")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp_NoPort :: Test
test_OpenTcp_NoPort = assertions "no-port" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		])
	$assert $ throwsEq
		((transportError "TCP transport requires the `port' parameter.")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp_InvalidPort :: Test
test_OpenTcp_InvalidPort = assertions "invalid-port" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		, ("port", "123456")
		])
	$assert $ throwsEq
		((transportError "Invalid socket port for TCP transport: \"123456\"")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp_NoUsableAddresses :: Test
test_OpenTcp_NoUsableAddresses = assertions "no-usable-addresses" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		, ("port", "1234")
		, ("host", "256.256.256.256")
		])
	$assert $ throws
		(\err -> and
			[ "getAddrInfo: does not exist" `isPrefixOf` transportErrorMessage err
			, transportErrorAddress err == Just addr
			])
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_OpenTcp_NotListening :: Test
test_OpenTcp_NotListening = assertions "too-many" $ do
	fdcountBefore <- countFileDescriptors
	
	(addr, networkSocket) <- listenRandomIPv4
	liftIO (NS.sClose networkSocket)
	$assert $ throwsEq
		((transportError "connect: does not exist (Connection refused)")
			{ transportErrorAddress = Just addr
			})
		(transportOpen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_TransportSendReceive :: Test
test_TransportSendReceive = assertions "send-receive" $ do
	(addr, networkSocket) <- listenRandomIPv4
	afterTest (N.sClose networkSocket)
	
	-- a simple echo server, which sends back anything it receives.
	_ <- liftIO $ forkIO $ do
		(s, _) <- NS.accept networkSocket
		fix $ \loop -> do
			bytes <- recv s 50
			if Data.ByteString.null bytes
				then NS.sClose s
				else do
					sendAll s bytes
					loop
	
	t <- liftIO (transportOpen socketTransportOptions addr)
	afterTest (transportClose t)
	
	-- small chunks of data are combined
	do
		var <- forkVar (transportGet t 3)
		liftIO (transportPut t "1")
		liftIO (transportPut t "2")
		liftIO (transportPut t "3")
		bytes <- liftIO (readMVar var)
		$assert (equal bytes "123")
	
	-- large chunks of data are read in full
	do
		let sentBytes = Data.ByteString.replicate (4096 * 100) 0
		var <- forkVar (transportGet t (4096 * 100))
		liftIO (transportPut t sentBytes)
		bytes <- liftIO (readMVar var)
		$assert (equal bytes sentBytes)

test_HandleLostConnection :: Test
test_HandleLostConnection = assertions "handle-lost-connection" $ do
	(addr, networkSocket) <- listenRandomIPv4
	afterTest (N.sClose networkSocket)
	
	_ <- liftIO $ forkIO $ do
		(s, _) <- NS.accept networkSocket
		sendAll s "123"
		NS.sClose s
	
	t <- liftIO (transportOpen socketTransportOptions addr)
	afterTest (transportClose t)
	
	bytes <- liftIO (transportGet t 4)
	$assert (equal bytes "123")

test_ListenUnknown :: Test
test_ListenUnknown = assertions "unknown" $ do
	let Just addr = address "noexist" Map.empty
	$assert $ throwsEq
		((transportError "Unknown address method: \"noexist\"")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)

test_ListenUnix :: Suite
test_ListenUnix = suite "unix"
	test_ListenUnix_Path
	test_ListenUnix_Abstract
	test_ListenUnix_Tmpdir
	test_ListenUnix_TooFew
	test_ListenUnix_TooMany
	test_ListenUnix_InvalidBind

test_ListenUnix_Path :: Test
test_ListenUnix_Path = assertions "path" $ do
	path <- liftIO getTempPath
	let Just addr = address "unix" (Map.fromList
		[ ("path", path)
		])
	l <- liftIO (transportListen socketTransportOptions addr)
	afterTest (transportListenerClose l)
	afterTest (removeFile path)
	
	-- listener address is random, so it can't be checked directly.
	let addrParams = addressParameters (transportListenerAddress l)
	$expect (sameItems (Map.keys addrParams) ["path", "guid"])
	$expect (equal (Map.lookup "path" addrParams) (Just path))

test_ListenUnix_Abstract :: Test
test_ListenUnix_Abstract = assertions "abstract" $ do
	path <- liftIO getTempPath
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", path)
		])
	l <- liftIO (transportListen socketTransportOptions addr)
	afterTest (transportListenerClose l)
	
	-- listener address is random, so it can't be checked directly.
	let addrParams = addressParameters (transportListenerAddress l)
	$expect (sameItems (Map.keys addrParams) ["abstract", "guid"])
	$expect (equal (Map.lookup "abstract" addrParams) (Just path))

test_ListenUnix_Tmpdir :: Test
test_ListenUnix_Tmpdir = assertions "tmpdir" $ do
	tmpdir <- liftIO getTemporaryDirectory
	let Just addr = address "unix" (Map.fromList
		[ ("tmpdir", tmpdir)
		])
	l <- liftIO (transportListen socketTransportOptions addr)
	afterTest (transportListenerClose l)
	
	-- listener address is random, so it can't be checked directly.
	let addrKeys = Map.keys (addressParameters (transportListenerAddress l))
	$expect ("path" `elem` addrKeys || "abstract" `elem` addrKeys)

test_ListenUnix_TooFew :: Test
test_ListenUnix_TooFew = assertions "too-few" $ do
	let Just addr = address "unix" Map.empty
	$assert $ throwsEq
		((transportError "One of 'abstract', 'path', or 'tmpdir' must be specified for the 'unix' transport.")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)

test_ListenUnix_TooMany :: Test
test_ListenUnix_TooMany = assertions "too-many" $ do
	let Just addr = address "unix" (Map.fromList
		[ ("path", "foo")
		, ("abstract", "bar")
		])
	$assert $ throwsEq
		((transportError "Only one of 'abstract', 'path', or 'tmpdir' may be specified for the 'unix' transport.")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)

test_ListenUnix_InvalidBind :: Test
test_ListenUnix_InvalidBind = assertions "invalid-bind" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "unix" (Map.fromList
		[ ("path", "/")
		])
	$assert $ throwsEq
		((transportError "bind: resource busy (Address already in use)")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_ListenTcp :: Suite
test_ListenTcp = suite "tcp"
	test_ListenTcp_IPv4
	(skipWhen noIPv6 test_ListenTcp_IPv6)
	test_ListenTcp_Unknown
	test_ListenTcp_InvalidPort
	test_ListenTcp_InvalidBind

test_ListenTcp_IPv4 :: Test
test_ListenTcp_IPv4 = assertions "ipv4" $ do
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		])
	l <- liftIO (transportListen socketTransportOptions addr)
	afterTest (transportListenerClose l)
	
	let params = addressParameters (transportListenerAddress l)
	$expect (equal (Map.lookup "family" params) (Just "ipv4"))
	$expect ("port" `elem` Map.keys params)

test_ListenTcp_IPv6 :: Test
test_ListenTcp_IPv6 = assertions "ipv6" $ do
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv6")
		])
	l <- liftIO (transportListen socketTransportOptions addr)
	afterTest (transportListenerClose l)
	
	let params = addressParameters (transportListenerAddress l)
	$expect (equal (Map.lookup "family" params) (Just "ipv6"))
	$expect ("port" `elem` Map.keys params)

test_ListenTcp_Unknown :: Test
test_ListenTcp_Unknown = assertions "unknown-family" $ do
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "noexist")
		, ("port", "1234")
		])
	$assert $ throwsEq
		((transportError "Unknown socket family for TCP transport: \"noexist\"")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)

test_ListenTcp_InvalidPort :: Test
test_ListenTcp_InvalidPort = assertions "invalid-port" $ do
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		, ("port", "123456")
		])
	$assert $ throwsEq
		((transportError "Invalid socket port for TCP transport: \"123456\"")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)

test_ListenTcp_InvalidBind :: Test
test_ListenTcp_InvalidBind = assertions "invalid-bind" $ do
	fdcountBefore <- countFileDescriptors
	
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		, ("port", "1")
		])
	$assert $ throwsEq
		((transportError "bind: permission denied (Permission denied)")
			{ transportErrorAddress = Just addr
			})
		(transportListen socketTransportOptions addr)
	
	fdcountAfter <- countFileDescriptors
	$assert (equal fdcountBefore fdcountAfter)

test_AcceptSocket :: Test
test_AcceptSocket = assertions "socket" $ do
	path <- liftIO getTempPath
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", path)
		])
	listener <- liftIO (transportListen socketTransportOptions addr)
	afterTest (transportListenerClose listener)
	
	acceptedVar <- forkVar (transportAccept listener)
	openedVar <- forkVar (transportOpen socketTransportOptions addr)
	
	accepted <- liftIO (readMVar acceptedVar)
	opened <- liftIO (readMVar openedVar)
	afterTest (transportClose accepted)
	afterTest (transportClose opened)
	
	liftIO (transportPut opened "testing")
	
	bytes <- liftIO (transportGet accepted 7)
	
	$expect (equal bytes "testing")

test_AcceptSocketClosed :: Test
test_AcceptSocketClosed = assertions "socket-closed" $ do
	path <- liftIO getTempPath
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", path)
		])
	listener <- liftIO (transportListen socketTransportOptions addr)
	let listeningAddr = transportListenerAddress listener
	liftIO (transportListenerClose listener)
	
	$assert $ throwsEq
		((transportError "user error (accept: can't perform accept on socket ((AF_UNIX,Stream,0)) in status Closed)")
			{ transportErrorAddress = Just listeningAddr
			})
		(transportAccept listener)

socketTransportOptions :: TransportOptions SocketTransport
socketTransportOptions = transportDefaultOptions
