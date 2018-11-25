-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module DBusTests.Client (test_Client) where

import Control.Concurrent
import Control.Exception (try)
import Data.Word
import Data.Int
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import DBus
import qualified DBus.Client
import qualified DBus.Socket
import DBus.Introspection.Parse
import DBus.Introspection.Types
import DBus.Internal.Types

import DBusTests.Util
import qualified DBusTests.TH as TH
import qualified DBusTests.Generation as G

doExport :: DBus.Client.Client -> String -> String -> [DBus.Client.Method] -> IO ()
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
    , test_CallWithGeneration
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

test_CallWithGeneration :: TestTree
test_CallWithGeneration = withConnectedClient $ \res -> testCase "callGeneration" $ do
    (sock, client) <- res
    let string = "test"
        busName = busName_ "org.freeDesktop.DBus"
        int = 32 :: Int32
        path = objectPath_ "/a/b/c"
        returnValue = Map.fromList [(string, int)]

    DBus.Client.export client path G.testInterface

    do
        response <- stubMethodCall sock
            (TH.sampleMethod1 client busName path string int)
            (methodCall path (DBus.Client.interfaceName G.testInterface) $
                        memberName_ "SampleMethod1")
             { methodCallDestination = Just busName
             , methodCallBody = [toVariant string, toVariant int]
             }
            (\x -> (methodReturn x)
                   { methodReturnBody = [toVariant returnValue]})
        reply <- requireRight response

        reply @?= returnValue

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
test_ExportIntrospection =
  withConnectedClient $ \res ->
    testCase "exportIntrospection" $ do
      (sock, client) <- res
      let interface =
            DBus.Client.defaultInterface
            { DBus.Client.interfaceMethods =
                [ DBus.Client.autoMethod
                    (memberName_ "Method1")
                    (undefined :: String -> IO ())
                , DBus.Client.autoMethod
                    (memberName_ "Method2")
                    (undefined :: String -> IO String)
                , DBus.Client.autoMethod
                    (memberName_ "Method3")
                    (undefined :: String -> IO (String, String))
                ]
            , DBus.Client.interfaceName = interfaceName_ "com.example.Echo"
            }
          expectedIntrospectionInterface =
            Object
            { objectPath = ObjectPath "/foo"
            , objectInterfaces =
                [ Interface
                  { interfaceName =
                      InterfaceName "org.freedesktop.DBus.Properties"
                  , interfaceMethods =
                      [ Method
                        { methodName = MemberName "Get"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "b"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "c"
                              , methodArgType = TypeVariant
                              , methodArgDirection = Out
                              }
                            ]
                        }
                      , Method
                        { methodName = MemberName "GetAll"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "b"
                              , methodArgType = TypeDictionary TypeString TypeVariant
                              , methodArgDirection = Out
                              }
                            ]
                        }
                      , Method
                        { methodName = MemberName "Set"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "b"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "c"
                              , methodArgType = TypeVariant
                              , methodArgDirection = In
                              }
                            ]
                        }
                      ]
                  , interfaceSignals =
                      [ Signal
                        { signalName = MemberName "PropertiesChanged"
                        , signalArgs =
                            [ SignalArg
                              { signalArgName = "interface_name"
                              , signalArgType = TypeString
                              }
                            , SignalArg
                              { signalArgName = "changed_properties"
                              , signalArgType = TypeDictionary TypeString TypeVariant
                              }
                            , SignalArg
                              { signalArgName = "invalidated_properties"
                              , signalArgType = TypeArray TypeString
                              }
                            ]
                        }
                      ]
                  , interfaceProperties = []
                  }
                , Interface
                  { interfaceName =
                      InterfaceName "org.freedesktop.DBus.Introspectable"
                  , interfaceMethods =
                      [ Method
                        { methodName = MemberName "Introspect"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = Out
                              }
                            ]
                        }
                      ]
                  , interfaceSignals = []
                  , interfaceProperties = []
                  }
                , Interface
                  { interfaceName = InterfaceName "com.example.Echo"
                  , interfaceMethods =
                      [ Method
                        { methodName = MemberName "Method1"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            ]
                        }
                      , Method
                        { methodName = MemberName "Method2"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "b"
                              , methodArgType = TypeString
                              , methodArgDirection = Out
                              }
                            ]
                        }
                      , Method
                        { methodName = MemberName "Method3"
                        , methodArgs =
                            [ MethodArg
                              { methodArgName = "a"
                              , methodArgType = TypeString
                              , methodArgDirection = In
                              }
                            , MethodArg
                              { methodArgName = "b"
                              , methodArgType = TypeString
                              , methodArgDirection = Out
                              }
                            , MethodArg
                              { methodArgName = "c"
                              , methodArgType = TypeString
                              , methodArgDirection = Out
                              }
                            ]
                        }
                      ]
                  , interfaceSignals = []
                  , interfaceProperties = []
                  }
                ]
            , objectChildren = []
            }
      DBus.Client.export client (objectPath_ "/foo") interface
      let introspect path = do
            (_, response) <-
              callClientMethod
                sock
                path
                "org.freedesktop.DBus.Introspectable"
                "Introspect"
                []
            ret <- requireRight response
            let body = methodReturnBody ret
            length body @?= 1
            let Just xml = fromVariant (head body)
            return $ parseXML (objectPath_ "/") xml
      root <- introspect "/"
      root @?=
        Just
          (Object
           { objectPath = ObjectPath "/"
           , objectInterfaces = []
           , objectChildren = [ expectedIntrospectionInterface ]
           })
      foo <- introspect "/foo"
      foo @?= Just expectedIntrospectionInterface

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
