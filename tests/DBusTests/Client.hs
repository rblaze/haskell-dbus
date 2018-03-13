-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
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

module DBusTests.Client (test_Client) where

import Control.Concurrent
import Control.Exception (try)
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import DBus
import qualified DBus.Client
import qualified DBus.Socket

import DBusTests.Util

doExport client path name methods =
  DBus.Client.export client (objectPath_ path) DBus.Client.defaultInterface
        { DBus.Client.interfaceMethods = methods
        , DBus.Client.interfaceName = interfaceName_ name
        }

test_Client :: TestTree
test_Client = testGroup "Client" $
    [ test_RequestName
    , test_ReleaseName
    , test_Call
    , test_CallNoReply
    , test_AddMatch
    , test_AutoMethod
    , test_ExportIntrospection
    , suite_Connect
    ]

test_Connect :: String -> (Address -> IO DBus.Client.Client) -> TestTree
test_Connect name connect = testCase name $ do
    (addr, sockVar) <- startDummyBus
    clientVar <- forkVar (connect addr)

    -- TODO: verify that 'hello' contains expected data, and
    -- send a properly formatted reply.
    sock <- readMVar sockVar
    receivedHello <- DBus.Socket.receive sock
    let (ReceivedMethodCall helloSerial _) = receivedHello

    DBus.Socket.send sock (methodReturn helloSerial) (\_ -> return ())

    client <- readMVar clientVar
    DBus.Client.disconnect client

suite_Connect :: TestTree
suite_Connect = testGroup "connect"
    [ test_ConnectSystem
    , test_ConnectSystem_NoAddress
    , test_ConnectSession
    , test_ConnectSession_NoAddress
    , test_ConnectStarter
    , test_ConnectStarter_NoAddress
    ]

test_ConnectSystem :: TestTree
test_ConnectSystem = test_Connect "connectSystem" $ \addr ->
    withEnv "DBUS_SYSTEM_BUS_ADDRESS"
        (Just (formatAddress addr))
        DBus.Client.connectSystem

test_ConnectSystem_NoAddress :: TestTree
test_ConnectSystem_NoAddress = testCase "connectSystem-no-address" $
    assertException
        (DBus.Client.clientError "connectSystem: DBUS_SYSTEM_BUS_ADDRESS is invalid.")
        (withEnv "DBUS_SYSTEM_BUS_ADDRESS"
            (Just "invalid")
            DBus.Client.connectSystem)

test_ConnectSession :: TestTree
test_ConnectSession = test_Connect "connectSession" $ \addr ->
    withEnv "DBUS_SESSION_BUS_ADDRESS"
        (Just (formatAddress addr))
        DBus.Client.connectSession

test_ConnectSession_NoAddress :: TestTree
test_ConnectSession_NoAddress = testCase "connectSession-no-address" $
    assertException
        (DBus.Client.clientError "connectSession: DBUS_SESSION_BUS_ADDRESS is missing or invalid.")
        (withEnv "DBUS_SESSION_BUS_ADDRESS"
            (Just "invalid")
            DBus.Client.connectSession)

test_ConnectStarter :: TestTree
test_ConnectStarter = test_Connect "connectStarter" $ \addr ->
    withEnv "DBUS_STARTER_ADDRESS"
        (Just (formatAddress addr))
        DBus.Client.connectStarter

test_ConnectStarter_NoAddress :: TestTree
test_ConnectStarter_NoAddress = testCase "connectStarter-no-address" $
    assertException
        (DBus.Client.clientError "connectStarter: DBUS_STARTER_ADDRESS is missing or invalid.")
        (withEnv "DBUS_STARTER_ADDRESS"
            (Just "invalid")
            DBus.Client.connectStarter)

test_RequestName :: TestTree
test_RequestName = withConnectedClient $ \res -> testCase "requestName" $ do
    (sock, client) <- res
    let allFlags =
            [ DBus.Client.nameAllowReplacement
            , DBus.Client.nameReplaceExisting
            , DBus.Client.nameDoNotQueue
            ]

    let requestCall = (dbusCall "RequestName")
            { methodCallDestination = Just (busName_ "org.freedesktop.DBus")
            , methodCallBody = [toVariant "com.example.Foo", toVariant (7 :: Word32)]
            }

    let requestReply body serial = (methodReturn serial)
            { methodReturnBody = body
            }

    -- NamePrimaryOwner
    do
        reply <- stubMethodCall sock
            (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
            requestCall
            (requestReply [toVariant (1 :: Word32)])
        reply @?= DBus.Client.NamePrimaryOwner

    -- NameInQueue
    do
        reply <- stubMethodCall sock
            (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
            requestCall
            (requestReply [toVariant (2 :: Word32)])
        reply @?= DBus.Client.NameInQueue

    -- NameExists
    do
        reply <- stubMethodCall sock
            (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
            requestCall
            (requestReply [toVariant (3 :: Word32)])
        reply @?= DBus.Client.NameExists

    -- NameAlreadyOwner
    do
        reply <- stubMethodCall sock
            (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
            requestCall
            (requestReply [toVariant (4 :: Word32)])
        reply @?= DBus.Client.NameAlreadyOwner

    -- response with empty body
    do
        tried <- stubMethodCall sock
            (try (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags))
            requestCall
            (requestReply [])
        err <- requireLeft tried
        err @?= (DBus.Client.clientError "requestName: received empty response")
            { DBus.Client.clientErrorFatal = False
            }

    -- response with invalid body
    do
        tried <- stubMethodCall sock
            (try (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags))
            requestCall
            (requestReply [toVariant ""])
        err <- requireLeft tried
        err @?= (DBus.Client.clientError "requestName: received invalid response code (Variant \"\")")
            { DBus.Client.clientErrorFatal = False
            }

    -- response with unknown result code
    do
        reply <- stubMethodCall sock
            (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
            requestCall
            (requestReply [toVariant (5 :: Word32)])
        show reply @?= "UnknownRequestNameReply 5"

test_ReleaseName :: TestTree
test_ReleaseName = withConnectedClient $ \res -> testCase "releaseName" $ do
    (sock, client) <- res
    let requestCall = (dbusCall "ReleaseName")
            { methodCallDestination = Just (busName_ "org.freedesktop.DBus")
            , methodCallBody = [toVariant "com.example.Foo"]
            }

    let requestReply body serial = (methodReturn serial)
            { methodReturnBody = body
            }

    -- NameReleased
    do
        reply <- stubMethodCall sock
            (DBus.Client.releaseName client (busName_ "com.example.Foo"))
            requestCall
            (requestReply [toVariant (1 :: Word32)])
        reply @?= DBus.Client.NameReleased

    -- NameNonExistent
    do
        reply <- stubMethodCall sock
            (DBus.Client.releaseName client (busName_ "com.example.Foo"))
            requestCall
            (requestReply [toVariant (2 :: Word32)])
        reply @?= DBus.Client.NameNonExistent

    -- NameNotOwner
    do
        reply <- stubMethodCall sock
            (DBus.Client.releaseName client (busName_ "com.example.Foo"))
            requestCall
            (requestReply [toVariant (3 :: Word32)])
        reply @?= DBus.Client.NameNotOwner

    -- response with empty body
    do
        tried <- stubMethodCall sock
            (try (DBus.Client.releaseName client (busName_ "com.example.Foo")))
            requestCall
            (requestReply [])
        err <- requireLeft tried
        err @?= (DBus.Client.clientError "releaseName: received empty response")
            { DBus.Client.clientErrorFatal = False
            }

    -- response with invalid body
    do
        tried <- stubMethodCall sock
            (try (DBus.Client.releaseName client (busName_ "com.example.Foo")))
            requestCall
            (requestReply [toVariant ""])
        err <- requireLeft tried
        err @?= (DBus.Client.clientError "releaseName: received invalid response code (Variant \"\")")
            { DBus.Client.clientErrorFatal = False
            }

    -- response with unknown result code
    do
        reply <- stubMethodCall sock
            (DBus.Client.releaseName client (busName_ "com.example.Foo"))
            requestCall
            (requestReply [toVariant (5 :: Word32)])
        show reply @?= "UnknownReleaseNameReply 5"

test_Call :: TestTree
test_Call = withConnectedClient $ \res -> testCase "call" $ do
    (sock, client) <- res
    let requestCall = (dbusCall "Hello")
            { methodCallSender = Just (busName_ "com.example.Foo")
            , methodCallDestination = Just (busName_ "org.freedesktop.DBus")
            , methodCallReplyExpected = False
            , methodCallAutoStart = False
            , methodCallBody = [toVariant "com.example.Foo"]
            }

    -- methodCallReplyExpected is forced to True
    do
        response <- stubMethodCall sock
            (DBus.Client.call client requestCall)
            (requestCall
                { methodCallReplyExpected = True
                })
            methodReturn
        reply <- requireRight response

        reply @?= methodReturn (methodReturnSerial reply)

test_CallNoReply :: TestTree
test_CallNoReply = withConnectedClient $ \res -> testCase "callNoReply" $ do
    (sock, client) <- res
    let requestCall = (dbusCall "Hello")
            { methodCallSender = Just (busName_ "com.example.Foo")
            , methodCallDestination = Just (busName_ "org.freedesktop.DBus")
            , methodCallReplyExpected = True
            , methodCallAutoStart = False
            , methodCallBody = [toVariant "com.example.Foo"]
            }

    -- methodCallReplyExpected is forced to False
    do
        stubMethodCall sock
            (DBus.Client.callNoReply client requestCall)
            (requestCall
                { methodCallReplyExpected = False
                })
            methodReturn

test_AddMatch :: TestTree
test_AddMatch = withConnectedClient $ \res -> testCase "addMatch" $ do
    (sock, client) <- res
    let matchRule = DBus.Client.matchAny
            { DBus.Client.matchSender = Just (busName_ "com.example.Foo")
            , DBus.Client.matchDestination = Just (busName_ "com.example.Bar")
            , DBus.Client.matchPath = Just (objectPath_ "/")
            , DBus.Client.matchInterface = Just (interfaceName_ "com.example.Baz")
            , DBus.Client.matchMember = Just (memberName_ "Qux")
            }

    -- might as well test this while we're at it
    show matchRule @?= "MatchRule \"sender='com.example.Foo',destination='com.example.Bar',path='/',interface='com.example.Baz',member='Qux'\""

    let requestCall = (dbusCall "AddMatch")
            { methodCallDestination = Just (busName_ "org.freedesktop.DBus")
            , methodCallBody = [toVariant "type='signal',sender='com.example.Foo',destination='com.example.Bar',path='/',interface='com.example.Baz',member='Qux'"]
            }

    signalVar <- newEmptyMVar

    -- add a listener for the given signal
    _ <- stubMethodCall sock
        (DBus.Client.addMatch client matchRule (putMVar signalVar))
        requestCall
        methodReturn

    -- ignored signal
    DBus.Socket.send sock
        (signal (objectPath_ "/") (interfaceName_ "com.example.Baz") (memberName_ "Qux"))
        (\_ -> return ())
    isEmptyMVar signalVar >>= assertBool "signal not ignored"

    -- matched signal
    let matchedSignal = (signal (objectPath_ "/") (interfaceName_ "com.example.Baz") (memberName_ "Qux"))
            { signalSender = Just (busName_ "com.example.Foo")
            , signalDestination = Just (busName_ "com.example.Bar")
            }
    DBus.Socket.send sock matchedSignal (\_ -> return ())
    received <- takeMVar signalVar
    received @?= matchedSignal

test_AutoMethod :: TestTree
test_AutoMethod = withConnectedClient $ \res -> testCase "autoMethod" $ do
    (sock, client) <- res
    let methodMax = (\x y -> return (max x y)) :: Word32 -> Word32 -> IO Word32

    let methodPair = (\x y -> return (x, y)) :: String -> String -> IO (String, String)

    doExport client "/" "com.example.Foo"
        [ DBus.Client.autoMethod (memberName_ "Max") methodMax
        , DBus.Client.autoMethod (memberName_ "Pair") methodPair
        ]

    -- valid call to com.example.Foo.Max
    do
        (serial, response) <- callClientMethod sock "/" "com.example.Foo" "Max" [toVariant (2 :: Word32), toVariant (1 :: Word32)]
        response @?= Right (methodReturn serial)
            { methodReturnBody = [toVariant (2 :: Word32)]
            }

    -- valid call to com.example.Foo.Pair
    do
        (serial, response) <- callClientMethod sock "/" "com.example.Foo" "Pair" [toVariant "x", toVariant "y"]
        response @?= Right (methodReturn serial)
            { methodReturnBody = [toVariant "x", toVariant "y"]
            }

    -- invalid call to com.example.Foo.Max
    do
        (serial, response) <- callClientMethod sock "/" "com.example.Foo" "Max" [toVariant "x", toVariant "y"]
        response @?= Left (methodError serial (errorName_ "org.freedesktop.DBus.Error.InvalidParameters"))

test_ExportIntrospection :: TestTree
test_ExportIntrospection = withConnectedClient $ \res -> testCase "exportIntrospection" $ do
    (sock, client) <- res
    doExport client "/foo" "com.example.Echo"
               [ DBus.Client.autoMethod (memberName_ "Method1")
                 (undefined :: String -> IO ())
               , DBus.Client.autoMethod (memberName_ "Method2")
                 (undefined :: String -> IO String)
               , DBus.Client.autoMethod (memberName_ "Method3")
                 (undefined :: String -> IO (String, String))
               ]

    let introspect path = do
        (_, response) <- callClientMethod sock path "org.freedesktop.DBus.Introspectable" "Introspect" []
        ret <- requireRight response
        let body = methodReturnBody ret

        length body @?= 1
        let Just xml = fromVariant (head body)
        return xml

    root <- introspect "/"
    root @?=
         "<!DOCTYPE node PUBLIC '-//freedesktop//DTD D-BUS Object Introspection 1.0//EN' 'http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd'>\n<node name='/'><node name='foo'><interface name='org.freedesktop.DBus.Properties'><method name='Get'><arg name='interfaceName' type='s' direction='in'/><arg name='memberName' type='s' direction='in'/><arg name='value' type='v' direction='out'/></method><method name='Set'><arg name='interfaceName' type='s' direction='in'/><arg name='memberName' type='s' direction='in'/><arg name='value' type='v' direction='in'/></method><method name='GetAll'><arg name='interfaceName' type='s' direction='in'/><arg name='values' type='a{sv}' direction='out'/></method></interface><interface name='org.freedesktop.DBus.Introspectable'><method name='Introspect'><arg name='output' type='s' direction='out'/></method></interface><interface name='com.example.Echo'><method name='Method1'><arg name='a' type='s' direction='in'/></method><method name='Method2'><arg name='a' type='s' direction='in'/><arg name='b' type='s' direction='out'/></method><method name='Method3'><arg name='a' type='s' direction='in'/><arg name='b' type='s' direction='out'/><arg name='c' type='s' direction='out'/></method></interface></node></node>"
    foo <- introspect "/foo"
    foo @?=
        "<!DOCTYPE node PUBLIC '-//freedesktop//DTD D-BUS Object Introspection 1.0//EN' 'http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd'>\n<node name='/foo'><interface name='org.freedesktop.DBus.Properties'><method name='Get'><arg name='interfaceName' type='s' direction='in'/><arg name='memberName' type='s' direction='in'/><arg name='value' type='v' direction='out'/></method><method name='Set'><arg name='interfaceName' type='s' direction='in'/><arg name='memberName' type='s' direction='in'/><arg name='value' type='v' direction='in'/></method><method name='GetAll'><arg name='interfaceName' type='s' direction='in'/><arg name='values' type='a{sv}' direction='out'/></method></interface><interface name='org.freedesktop.DBus.Introspectable'><method name='Introspect'><arg name='output' type='s' direction='out'/></method></interface><interface name='com.example.Echo'><method name='Method1'><arg name='a' type='s' direction='in'/></method><method name='Method2'><arg name='a' type='s' direction='in'/><arg name='b' type='s' direction='out'/></method><method name='Method3'><arg name='a' type='s' direction='in'/><arg name='b' type='s' direction='out'/><arg name='c' type='s' direction='out'/></method></interface></node>"

startDummyBus :: IO (Address, MVar DBus.Socket.Socket)
startDummyBus = do
    uuid <- randomUUID
    let Just addr = address "unix" (Map.fromList [("abstract", formatUUID uuid)])
    listener <- DBus.Socket.listen addr
    sockVar <- forkVar (DBus.Socket.accept listener)
    return (DBus.Socket.socketListenerAddress listener, sockVar)

withConnectedClient :: (IO (DBus.Socket.Socket, DBus.Client.Client) -> TestTree) -> TestTree
withConnectedClient = withResource create remove
  where
    create = do
        (addr, sockVar) <- startDummyBus
        clientVar <- forkVar (DBus.Client.connect addr)

        -- TODO: verify that 'hello' contains expected data, and
        -- send a properly formatted reply.
        sock <- readMVar sockVar
        receivedHello <- DBus.Socket.receive sock
        let (ReceivedMethodCall helloSerial _) = receivedHello

        DBus.Socket.send sock (methodReturn helloSerial) (\_ -> return ())

        client <- readMVar clientVar
        return (sock, client)
    remove (_, client) = DBus.Client.disconnect client

stubMethodCall :: DBus.Socket.Socket -> IO a -> MethodCall -> (Serial -> MethodReturn) -> IO a
stubMethodCall sock io expectedCall respond = do
    var <- forkVar io

    receivedCall <- DBus.Socket.receive sock
    let ReceivedMethodCall callSerial call = receivedCall
    expectedCall @?= call

    DBus.Socket.send sock (respond callSerial) (\_ -> return ())

    takeMVar var

callClientMethod :: DBus.Socket.Socket -> String -> String -> String -> [Variant] -> IO (Serial, Either MethodError MethodReturn)
callClientMethod sock path iface name body = do
    let call = (methodCall (objectPath_ path) (interfaceName_ iface) (memberName_ name))
            { methodCallBody = body
            }
    serial <- DBus.Socket.send sock call return
    resp <- DBus.Socket.receive sock
    case resp of
        ReceivedMethodReturn _ ret -> return (serial, Right ret)
        ReceivedMethodError _ err -> return (serial, Left err)
        _ -> assertFailure "callClientMethod: unexpected response to method call" >> undefined

dbusCall :: String -> MethodCall
dbusCall member = methodCall (objectPath_ "/org/freedesktop/DBus") (interfaceName_ "org.freedesktop.DBus") (memberName_ member)
