{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2012 John Millikin <john@john-millikin.com>
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

import Control.Concurrent
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Data.Function (fix)
import Data.List (isInfixOf)
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (getTemporaryDirectory, removeFile)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString
import qualified Data.Map as Map
import qualified Network.Socket as NS

import DBus
import DBus.Transport

import DBusTests.Util

test_Transport :: TestTree
test_Transport = testGroup "Transport" $
    [ suite_TransportOpen
    , suite_TransportListen
    , suite_TransportAccept
    , test_TransportSendReceive
    , test_HandleLostConnection
    ]

suite_TransportOpen :: TestTree
suite_TransportOpen = testGroup "transportOpen" $
    [ test_OpenUnknown
    , suite_OpenUnix
    , suite_OpenTcp
    ]

suite_TransportListen :: TestTree
suite_TransportListen = testGroup "transportListen" $
    [ test_ListenUnknown
    , suite_ListenUnix
    , suite_ListenTcp
    ]

suite_TransportAccept :: TestTree
suite_TransportAccept = testGroup "transportAccept"
    [ test_AcceptSocket
    , test_AcceptSocketClosed
    ]

test_OpenUnknown :: TestTree
test_OpenUnknown = testCase "unknown" $ do
    let Just addr = address "noexist" Map.empty
    assertException
        ((transportError "Unknown address method: \"noexist\"")
            { transportErrorAddress = Just addr
            })
        (transportOpen socketTransportOptions addr)

suite_OpenUnix :: TestTree
suite_OpenUnix = testGroup "unix"
    [ test_OpenUnix_Path
    , test_OpenUnix_Abstract
    , test_OpenUnix_TooFew
    , test_OpenUnix_TooMany
    , test_OpenUnix_NotListening
    ]

test_OpenUnix_Path :: TestTree
test_OpenUnix_Path = testCase "path" $ runResourceT $ do
    (addr, networkSocket) <- listenRandomUnixPath
    fdcountBefore <- countFileDescriptors

    t <- liftIO (transportOpen socketTransportOptions addr)
    liftIO (transportClose t)

    fdcountAfter <- countFileDescriptors
    liftIO (fdcountBefore @=? fdcountAfter)

test_OpenUnix_Abstract :: TestTree
test_OpenUnix_Abstract = testCase "abstract" $ runResourceT $ do
    (addr, networkSocket, _) <- listenRandomUnixAbstract
    fdcountBefore <- countFileDescriptors

    t <- liftIO (transportOpen socketTransportOptions addr)
    liftIO (transportClose t)

    fdcountAfter <- countFileDescriptors
    liftIO (fdcountBefore @=? fdcountAfter)

test_OpenUnix_TooFew :: TestTree
test_OpenUnix_TooFew = testCase "too-few" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "unix" Map.empty
    assertException
        ((transportError "One of 'path' or 'abstract' must be specified for the 'unix' transport.")
            { transportErrorAddress = Just addr
            })
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_OpenUnix_TooMany :: TestTree
test_OpenUnix_TooMany = testCase "too-many" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "unix" (Map.fromList
            [ ("path", "foo")
            , ("abstract", "bar")
            ])
    assertException
        ((transportError "Only one of 'path' or 'abstract' may be specified for the 'unix' transport.")
            { transportErrorAddress = Just addr
            })
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_OpenUnix_NotListening :: TestTree
test_OpenUnix_NotListening = testCase "not-listening" $ runResourceT $ do
    fdcountBefore <- countFileDescriptors

    (addr, networkSocket, key) <- listenRandomUnixAbstract
    release key

    liftIO $ assertThrows
        (\err -> and
            [ "Connection refused" `isInfixOf` transportErrorMessage err
            , transportErrorAddress err == Just addr
            ])
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    liftIO (fdcountBefore @=? fdcountAfter)

suite_OpenTcp :: TestTree
suite_OpenTcp = testGroup "tcp"
    [ test_OpenTcp_IPv4
    , test_OpenTcp_IPv6
    , test_OpenTcp_Unknown
    , test_OpenTcp_NoPort
    , test_OpenTcp_InvalidPort
    , test_OpenTcp_NoUsableAddresses
    , test_OpenTcp_NotListening
    ]

test_OpenTcp_IPv4 :: TestTree
test_OpenTcp_IPv4 = testCase "ipv4" $ runResourceT $ do
    (addr, networkSocket, _) <- listenRandomIPv4
    fdcountBefore <- countFileDescriptors

    t <- liftIO (transportOpen socketTransportOptions addr)
    liftIO (transportClose t)

    fdcountAfter <- countFileDescriptors
    liftIO (fdcountBefore @=? fdcountAfter)

test_OpenTcp_IPv6 :: TestTree
test_OpenTcp_IPv6 = testCase "ipv6" $ unlessM noIPv6 $ runResourceT $ do
    (addr, networkSocket) <- listenRandomIPv6
    fdcountBefore <- countFileDescriptors

    t <- liftIO (transportOpen socketTransportOptions addr)
    liftIO (transportClose t)

    fdcountAfter <- countFileDescriptors
    liftIO (fdcountBefore @=? fdcountAfter)

test_OpenTcp_Unknown :: TestTree
test_OpenTcp_Unknown = testCase "unknown-family" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "tcp" (Map.fromList
            [ ("family", "noexist")
            , ("port", "1234")
            ])
    assertException
        ((transportError "Unknown socket family for TCP transport: \"noexist\"")
            { transportErrorAddress = Just addr
            })
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_OpenTcp_NoPort :: TestTree
test_OpenTcp_NoPort = testCase "no-port" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            ])
    assertException
        ((transportError "TCP transport requires the `port' parameter.")
            { transportErrorAddress = Just addr
            })
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_OpenTcp_InvalidPort :: TestTree
test_OpenTcp_InvalidPort = testCase "invalid-port" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            , ("port", "123456")
            ])
    assertException
        ((transportError "Invalid socket port for TCP transport: \"123456\"")
            { transportErrorAddress = Just addr
            })
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_OpenTcp_NoUsableAddresses :: TestTree
test_OpenTcp_NoUsableAddresses = testCase "no-usable-addresses" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            , ("port", "1234")
            , ("host", "256.256.256.256")
            ])
    assertThrows
        (\err -> and
            [ "getAddrInfo" `isInfixOf` transportErrorMessage err
            , "does not exist" `isInfixOf` transportErrorMessage err
            , transportErrorAddress err == Just addr
            ])
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_OpenTcp_NotListening :: TestTree
test_OpenTcp_NotListening = testCase "too-many" $ runResourceT $ do
    fdcountBefore <- countFileDescriptors

    (addr, networkSocket, key) <- listenRandomIPv4
    release key
    liftIO $ assertThrows
        (\err -> and
            [ "Connection refused" `isInfixOf` transportErrorMessage err
            , transportErrorAddress err == Just addr
            ])
        (transportOpen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    liftIO (fdcountBefore @=? fdcountAfter)

test_TransportSendReceive :: TestTree
test_TransportSendReceive = testCase "send-receive" $ runResourceT $ do
    (addr, networkSocket, _) <- listenRandomIPv4

    -- a simple echo server, which sends back anything it receives.
    _ <- liftIO $ forkIO $ do
        (s, _) <- NS.accept networkSocket
        fix $ \loop -> do
            bytes <- recv s 50
            if Data.ByteString.null bytes
                then NS.close s
                else do
                    sendAll s bytes
                    loop

    (_, t) <- allocate
        (transportOpen socketTransportOptions addr)
        transportClose

    -- small chunks of data are combined
    do
        var <- forkVar (transportGet t 3)
        liftIO (transportPut t "1")
        liftIO (transportPut t "2")
        liftIO (transportPut t "3")
        bytes <- liftIO (readMVar var)
        liftIO (bytes @?= "123")

    -- large chunks of data are read in full
    do
        let sentBytes = Data.ByteString.replicate (4096 * 100) 0
        var <- forkVar (transportGet t (4096 * 100))
        liftIO (transportPut t sentBytes)
        bytes <- liftIO (readMVar var)
        liftIO (bytes @?= sentBytes)

test_HandleLostConnection :: TestTree
test_HandleLostConnection = testCase "handle-lost-connection" $ runResourceT $ do
    (addr, networkSocket, _) <- listenRandomIPv4

    _ <- liftIO $ forkIO $ do
        (s, _) <- NS.accept networkSocket
        sendAll s "123"
        NS.close s

    (_, t) <- allocate
        (transportOpen socketTransportOptions addr)
        transportClose

    bytes <- liftIO (transportGet t 4)
    liftIO (bytes @?= "123")

test_ListenUnknown :: TestTree
test_ListenUnknown = testCase "unknown" $ do
    let Just addr = address "noexist" Map.empty
    assertException
        ((transportError "Unknown address method: \"noexist\"")
            { transportErrorAddress = Just addr
            })
        (transportListen socketTransportOptions addr)

suite_ListenUnix :: TestTree
suite_ListenUnix = testGroup "unix"
    [ test_ListenUnix_Path
    , test_ListenUnix_Abstract
    , test_ListenUnix_Tmpdir
    , test_ListenUnix_TooFew
    , test_ListenUnix_TooMany
    , test_ListenUnix_InvalidBind
    ]

test_ListenUnix_Path :: TestTree
test_ListenUnix_Path = testCase "path" $ runResourceT $ do
    (_, path) <- allocate getTempPath removeFile
    let Just addr = address "unix" (Map.fromList
            [ ("path", path)
            ])
    (_, l) <- allocate
        (transportListen socketTransportOptions addr)
        transportListenerClose

    -- listener address is random, so it can't be checked directly.
    let addrParams = addressParameters (transportListenerAddress l)
    liftIO (Map.keys addrParams @=? ["guid", "path"])
    liftIO (Map.lookup "path" addrParams @?= Just path)

test_ListenUnix_Abstract :: TestTree
test_ListenUnix_Abstract = testCase "abstract" $ runResourceT $ do
    path <- liftIO getTempPath
    let Just addr = address "unix" (Map.fromList
            [ ("abstract", path)
            ])
    (_, l) <- allocate
        (transportListen socketTransportOptions addr)
        transportListenerClose

    -- listener address is random, so it can't be checked directly.
    let addrParams = addressParameters (transportListenerAddress l)
    liftIO (Map.keys addrParams @?= ["abstract", "guid"])
    liftIO (Map.lookup "abstract" addrParams @?= Just path)

test_ListenUnix_Tmpdir :: TestTree
test_ListenUnix_Tmpdir = testCase "tmpdir" $ runResourceT $ do
    tmpdir <- liftIO getTemporaryDirectory
    let Just addr = address "unix" (Map.fromList
            [ ("tmpdir", tmpdir)
            ])
    (_, l) <- allocate
        (transportListen socketTransportOptions addr)
        transportListenerClose

    -- listener address is random, so it can't be checked directly.
    let addrKeys = Map.keys (addressParameters (transportListenerAddress l))
    liftIO $ assertBool "invalid keys"
        ("path" `elem` addrKeys || "abstract" `elem` addrKeys)

test_ListenUnix_TooFew :: TestTree
test_ListenUnix_TooFew = testCase "too-few" $ do
    let Just addr = address "unix" Map.empty
    assertException
        ((transportError "One of 'abstract', 'path', or 'tmpdir' must be specified for the 'unix' transport.")
            { transportErrorAddress = Just addr
            })
        (transportListen socketTransportOptions addr)

test_ListenUnix_TooMany :: TestTree
test_ListenUnix_TooMany = testCase "too-many" $ do
    let Just addr = address "unix" (Map.fromList
            [ ("path", "foo")
            , ("abstract", "bar")
            ])
    assertException
        ((transportError "Only one of 'abstract', 'path', or 'tmpdir' may be specified for the 'unix' transport.")
            { transportErrorAddress = Just addr
            })
        (transportListen socketTransportOptions addr)

test_ListenUnix_InvalidBind :: TestTree
test_ListenUnix_InvalidBind = testCase "invalid-bind" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "unix" (Map.fromList
            [ ("path", "/")
            ])
    assertThrows
        (\err -> and
            [ "Address already in use" `isInfixOf` transportErrorMessage err
            , transportErrorAddress err == Just addr
            ])
        (transportListen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

suite_ListenTcp :: TestTree
suite_ListenTcp = testGroup "tcp"
    [ test_ListenTcp_IPv4
    , test_ListenTcp_IPv6
    , test_ListenTcp_Unknown
    , test_ListenTcp_InvalidPort
    , test_ListenTcp_InvalidBind
    ]

test_ListenTcp_IPv4 :: TestTree
test_ListenTcp_IPv4 = testCase "ipv4" $ runResourceT $ do
    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            ])
    (_, l) <- allocate
        (transportListen socketTransportOptions addr)
        transportListenerClose

    let params = addressParameters (transportListenerAddress l)
    liftIO (Map.lookup "family" params @?= Just "ipv4")
    liftIO $ assertBool "no port" ("port" `elem` Map.keys params)

test_ListenTcp_IPv6 :: TestTree
test_ListenTcp_IPv6 = testCase "ipv6" $ unlessM noIPv6 $ runResourceT $ do
    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv6")
            ])
    (_, l) <- allocate
        (transportListen socketTransportOptions addr)
        transportListenerClose

    let params = addressParameters (transportListenerAddress l)
    liftIO (Map.lookup "family" params @?= Just "ipv6")
    liftIO $ assertBool "no port" ("port" `elem` Map.keys params)

test_ListenTcp_Unknown :: TestTree
test_ListenTcp_Unknown = testCase "unknown-family" $ do
    let Just addr = address "tcp" (Map.fromList
            [ ("family", "noexist")
            , ("port", "1234")
            ])
    assertException
        ((transportError "Unknown socket family for TCP transport: \"noexist\"")
            { transportErrorAddress = Just addr
            })
        (transportListen socketTransportOptions addr)

test_ListenTcp_InvalidPort :: TestTree
test_ListenTcp_InvalidPort = testCase "invalid-port" $ do
    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            , ("port", "123456")
            ])
    assertException
        ((transportError "Invalid socket port for TCP transport: \"123456\"")
            { transportErrorAddress = Just addr
            })
        (transportListen socketTransportOptions addr)

test_ListenTcp_InvalidBind :: TestTree
test_ListenTcp_InvalidBind = testCase "invalid-bind" $ do
    fdcountBefore <- countFileDescriptors

    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            , ("port", "1")
            ])
    assertThrows
        (\err -> and
            [ "Permission denied" `isInfixOf` transportErrorMessage err
            , transportErrorAddress err == Just addr
            ])
        (transportListen socketTransportOptions addr)

    fdcountAfter <- countFileDescriptors
    fdcountBefore @=? fdcountAfter

test_AcceptSocket :: TestTree
test_AcceptSocket = testCase "socket" $ runResourceT $ do
    path <- liftIO getTempPath
    let Just addr = address "unix" (Map.fromList
            [ ("abstract", path)
            ])
    (_, listener) <- allocate
        (transportListen socketTransportOptions addr)
        transportListenerClose

    acceptedVar <- forkVar (transportAccept listener)
    openedVar <- forkVar (transportOpen socketTransportOptions addr)

    (_, accepted) <- allocate (readMVar acceptedVar) transportClose
    (_, opened) <- allocate (readMVar openedVar) transportClose

    liftIO (transportPut opened "testing")

    bytes <- liftIO (transportGet accepted 7)

    liftIO (bytes @?= "testing")

test_AcceptSocketClosed :: TestTree
test_AcceptSocketClosed = testCase "socket-closed" $ do
    path <- getTempPath
    let Just addr = address "unix" (Map.fromList
            [ ("abstract", path)
            ])
    listener <- transportListen socketTransportOptions addr
    let listeningAddr = transportListenerAddress listener
    transportListenerClose listener

    assertThrows
        (\err -> and
            [ "accept" `isInfixOf` transportErrorMessage err
            , "socket" `isInfixOf` transportErrorMessage err
            , "Closed" `isInfixOf` transportErrorMessage err
            , transportErrorAddress err == Just listeningAddr
            ])
        (transportAccept listener)

socketTransportOptions :: TransportOptions SocketTransport
socketTransportOptions = transportDefaultOptions
