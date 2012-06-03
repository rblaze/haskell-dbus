{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2012 John Millikin <jmillikin@gmail.com>
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

import           Control.Concurrent
import           Control.Exception (try)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import           Data.Word

import           Test.Chell

import           DBus
import qualified DBus.Client
import qualified DBus.Socket

import           DBusTests.Util (forkVar, withEnv)

test_Client :: Suite
test_Client = suite "Client"
	suite_Connect
	test_RequestName
	test_ReleaseName
	test_Call
	test_CallNoReply
	test_Listen
	test_AutoMethod

test_Connect :: String -> (Address -> IO DBus.Client.Client) -> Test
test_Connect name connect = assertions name $ do
	(addr, sockVar) <- startDummyBus
	clientVar <- forkVar (connect addr)
	
	-- TODO: verify that 'hello' contains expected data, and
	-- send a properly formatted reply.
	sock <- liftIO (readMVar sockVar)
	receivedHello <- liftIO (DBus.Socket.receive sock)
	let (ReceivedMethodCall helloSerial _) = receivedHello
	
	liftIO (DBus.Socket.send sock (MethodReturn
		{ methodReturnSerial = helloSerial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = []
		}) (\_ -> return ()))
	
	client <- liftIO (readMVar clientVar)
	liftIO (DBus.Client.disconnect client)

suite_Connect :: Suite
suite_Connect = suite "connect"
	test_ConnectSystem
	test_ConnectSystem_NoAddress
	test_ConnectSession
	test_ConnectSession_NoAddress
	test_ConnectStarter
	test_ConnectStarter_NoAddress

test_ConnectSystem :: Test
test_ConnectSystem = test_Connect "connectSystem" $ \addr -> do
	withEnv "DBUS_SYSTEM_BUS_ADDRESS"
		(Just (formatAddress addr))
		DBus.Client.connectSystem

test_ConnectSystem_NoAddress :: Test
test_ConnectSystem_NoAddress = assertions "connectSystem-no-address" $ do
	$expect $ throwsEq
		(DBus.Client.clientError "connectSystem: DBUS_SYSTEM_BUS_ADDRESS is invalid.")
		(withEnv "DBUS_SYSTEM_BUS_ADDRESS"
			(Just "invalid")
			DBus.Client.connectSystem)

test_ConnectSession :: Test
test_ConnectSession = test_Connect "connectSession" $ \addr -> do
	withEnv "DBUS_SESSION_BUS_ADDRESS"
		(Just (formatAddress addr))
		DBus.Client.connectSession

test_ConnectSession_NoAddress :: Test
test_ConnectSession_NoAddress = assertions "connectSession-no-address" $ do
	$expect $ throwsEq
		(DBus.Client.clientError "connectSession: DBUS_SESSION_BUS_ADDRESS is missing or invalid.")
		(withEnv "DBUS_SESSION_BUS_ADDRESS"
			(Just "invalid")
			DBus.Client.connectSession)

test_ConnectStarter :: Test
test_ConnectStarter = test_Connect "connectStarter" $ \addr -> do
	withEnv "DBUS_STARTER_ADDRESS"
		(Just (formatAddress addr))
		DBus.Client.connectStarter

test_ConnectStarter_NoAddress :: Test
test_ConnectStarter_NoAddress = assertions "connectStarter-no-address" $ do
	$expect $ throwsEq
		(DBus.Client.clientError "connectStarter: DBUS_STARTER_ADDRESS is missing or invalid.")
		(withEnv "DBUS_STARTER_ADDRESS"
			(Just "invalid")
			DBus.Client.connectStarter)

test_RequestName :: Test
test_RequestName = assertions "requestName" $ do
	(sock, client) <- startConnectedClient
	let allFlags =
		[ DBus.Client.AllowReplacement
		, DBus.Client.ReplaceExisting
		, DBus.Client.DoNotQueue
		]
	
	let requestCall = MethodCall
		{ methodCallPath = objectPath_ "/org/freedesktop/DBus"
		, methodCallInterface = Just (interfaceName_ "org.freedesktop.DBus")
		, methodCallMember = memberName_ "RequestName"
		, methodCallSender = Nothing
		, methodCallDestination = Just (busName_ "org.freedesktop.DBus")
		, methodCallFlags = []
		, methodCallBody = [toVariant "com.example.Foo", toVariant (7 :: Word32)]
		}
	
	let requestReply body serial = MethodReturn
		{ methodReturnSerial = serial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = body
		}
	
	-- PrimaryOwner
	do
		reply <- stubMethodCall sock
			(DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
			requestCall
			(requestReply [toVariant (1 :: Word32)])
		$expect (equal reply DBus.Client.PrimaryOwner)
	
	-- InQueue
	do
		reply <- stubMethodCall sock
			(DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
			requestCall
			(requestReply [toVariant (2 :: Word32)])
		$expect (equal reply DBus.Client.InQueue)
	
	-- Exists
	do
		reply <- stubMethodCall sock
			(DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
			requestCall
			(requestReply [toVariant (3 :: Word32)])
		$expect (equal reply DBus.Client.Exists)
	
	-- AlreadyOwner
	do
		reply <- stubMethodCall sock
			(DBus.Client.requestName client (busName_ "com.example.Foo") allFlags)
			requestCall
			(requestReply [toVariant (4 :: Word32)])
		$expect (equal reply DBus.Client.AlreadyOwner)
	
	-- response with empty body
	do
		tried <- stubMethodCall sock
			(try (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags))
			requestCall
			(requestReply [])
		err <- $requireLeft tried
		$expect (equal err (DBus.Client.clientError "requestName: received empty response")
			{ DBus.Client.clientErrorFatal = False
			})
	
	-- response with invalid body
	do
		tried <- stubMethodCall sock
			(try (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags))
			requestCall
			(requestReply [toVariant ""])
		err <- $requireLeft tried
		$expect (equal err (DBus.Client.clientError "requestName: received invalid response code (Variant \"\")")
			{ DBus.Client.clientErrorFatal = False
			})
	
	-- response with unknown result code
	do
		tried <- stubMethodCall sock
			(try (DBus.Client.requestName client (busName_ "com.example.Foo") allFlags))
			requestCall
			(requestReply [toVariant (5 :: Word32)])
		err <- $requireLeft tried
		$expect (equal err (DBus.Client.clientError "requestName: received unknown response code 5")
			{ DBus.Client.clientErrorFatal = False
			})

test_ReleaseName :: Test
test_ReleaseName = assertions "releaseName" $ do
	(sock, client) <- startConnectedClient
	
	let requestCall = MethodCall
		{ methodCallPath = objectPath_ "/org/freedesktop/DBus"
		, methodCallInterface = Just (interfaceName_ "org.freedesktop.DBus")
		, methodCallMember = memberName_ "ReleaseName"
		, methodCallSender = Nothing
		, methodCallDestination = Just (busName_ "org.freedesktop.DBus")
		, methodCallFlags = []
		, methodCallBody = [toVariant "com.example.Foo"]
		}
	
	let requestReply body serial = MethodReturn
		{ methodReturnSerial = serial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = body
		}
	
	-- Released
	do
		reply <- stubMethodCall sock
			(DBus.Client.releaseName client (busName_ "com.example.Foo"))
			requestCall
			(requestReply [toVariant (1 :: Word32)])
		$expect (equal reply DBus.Client.Released)
	
	-- NonExistent
	do
		reply <- stubMethodCall sock
			(DBus.Client.releaseName client (busName_ "com.example.Foo"))
			requestCall
			(requestReply [toVariant (2 :: Word32)])
		$expect (equal reply DBus.Client.NonExistent)
	
	-- NotOwner
	do
		reply <- stubMethodCall sock
			(DBus.Client.releaseName client (busName_ "com.example.Foo"))
			requestCall
			(requestReply [toVariant (3 :: Word32)])
		$expect (equal reply DBus.Client.NotOwner)
	
	-- response with empty body
	do
		tried <- stubMethodCall sock
			(try (DBus.Client.releaseName client (busName_ "com.example.Foo")))
			requestCall
			(requestReply [])
		err <- $requireLeft tried
		$expect (equal err (DBus.Client.clientError "releaseName: received empty response")
			{ DBus.Client.clientErrorFatal = False
			})
	
	-- response with invalid body
	do
		tried <- stubMethodCall sock
			(try (DBus.Client.releaseName client (busName_ "com.example.Foo")))
			requestCall
			(requestReply [toVariant ""])
		err <- $requireLeft tried
		$expect (equal err (DBus.Client.clientError "releaseName: received invalid response code (Variant \"\")")
			{ DBus.Client.clientErrorFatal = False
			})
	
	-- response with unknown result code
	do
		tried <- stubMethodCall sock
			(try (DBus.Client.releaseName client (busName_ "com.example.Foo")))
			requestCall
			(requestReply [toVariant (5 :: Word32)])
		err <- $requireLeft tried
		$expect (equal err (DBus.Client.clientError "releaseName: received unknown response code 5")
			{ DBus.Client.clientErrorFatal = False
			})

test_Call :: Test
test_Call = assertions "call" $ do
	(sock, client) <- startConnectedClient
	
	let requestCall = MethodCall
		{ methodCallPath = objectPath_ "/org/freedesktop/DBus"
		, methodCallInterface = Just (interfaceName_ "org.freedesktop.DBus")
		, methodCallMember = memberName_ "Hello"
		, methodCallSender = Just (busName_ "com.example.Foo")
		, methodCallDestination = Just (busName_ "org.freedesktop.DBus")
		, methodCallFlags = [NoReplyExpected, NoAutoStart]
		, methodCallBody = [toVariant "com.example.Foo"]
		}
	
	let requestReply serial = MethodReturn
		{ methodReturnSerial = serial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = []
		}
	
	-- NoReplyExpected and methodCallSender are removed
	do
		response <- stubMethodCall sock
			(DBus.Client.call client requestCall)
			(requestCall
				{ methodCallSender = Nothing
				, methodCallFlags = [NoAutoStart]
				})
			requestReply
		reply <- $requireRight response
		
		$expect (equal
			(reply { methodReturnSerial = firstSerial })
			(requestReply firstSerial))

test_CallNoReply :: Test
test_CallNoReply = assertions "callNoReply" $ do
	(sock, client) <- startConnectedClient
	
	let requestCall = MethodCall
		{ methodCallPath = objectPath_ "/org/freedesktop/DBus"
		, methodCallInterface = Just (interfaceName_ "org.freedesktop.DBus")
		, methodCallMember = memberName_ "Hello"
		, methodCallSender = Just (busName_ "com.example.Foo")
		, methodCallDestination = Just (busName_ "org.freedesktop.DBus")
		, methodCallFlags = [NoAutoStart]
		, methodCallBody = [toVariant "com.example.Foo"]
		}
	
	let requestReply serial = MethodReturn
		{ methodReturnSerial = serial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = []
		}
	
	-- NoReplyExpected is added, methodCallSender is removed
	do
		stubMethodCall sock
			(DBus.Client.callNoReply client requestCall)
			(requestCall
				{ methodCallSender = Nothing
				, methodCallFlags = [NoAutoStart, NoReplyExpected]
				})
			requestReply

test_Listen :: Test
test_Listen = assertions "listen" $ do
	(sock, client) <- startConnectedClient
	
	let matchRule = DBus.Client.matchAny
		{ DBus.Client.matchSender = Just (busName_ "com.example.Foo")
		, DBus.Client.matchDestination = Just (busName_ "com.example.Bar")
		, DBus.Client.matchPath = Just (objectPath_ "/")
		, DBus.Client.matchInterface = Just (interfaceName_ "com.example.Baz")
		, DBus.Client.matchMember = Just (memberName_ "Qux")
		}
	
	-- might as well test this while we're at it
	$expect (equal (show matchRule) "MatchRule \"sender='com.example.Foo',destination='com.example.Bar',path='/',interface='com.example.Baz',member='Qux'\"")
	
	let requestCall = MethodCall
		{ methodCallPath = objectPath_ "/org/freedesktop/DBus"
		, methodCallInterface = Just (interfaceName_ "org.freedesktop.DBus")
		, methodCallMember = memberName_ "AddMatch"
		, methodCallSender = Nothing
		, methodCallDestination = Just (busName_ "org.freedesktop.DBus")
		, methodCallFlags = [NoReplyExpected]
		, methodCallBody = [toVariant "sender='com.example.Foo',destination='com.example.Bar',path='/',interface='com.example.Baz',member='Qux'"]
		}
	
	let requestReply serial = MethodReturn
		{ methodReturnSerial = serial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = []
		}
	
	signalVar <- liftIO newEmptyMVar
	
	-- add a listener for the given signal
	stubMethodCall sock
		(DBus.Client.listen client matchRule (\sender sig -> putMVar signalVar (sender, sig)))
		requestCall
		requestReply
	
	-- ignored signal
	liftIO (DBus.Socket.send sock (Signal
		{ signalPath = objectPath_ "/"
		, signalMember = memberName_ "Qux"
		, signalInterface = interfaceName_ "com.example.Baz"
		, signalSender = Nothing
		, signalDestination = Nothing
		, signalBody = []
		}) (\_ -> return ()))
	$assert (isEmptyMVar signalVar)
	
	-- matched signal
	let matchedSignal = Signal
		{ signalPath = objectPath_ "/"
		, signalMember = memberName_ "Qux"
		, signalInterface = interfaceName_ "com.example.Baz"
		, signalSender = Just (busName_ "com.example.Foo")
		, signalDestination = Just (busName_ "com.example.Bar")
		, signalBody = []
		}
	liftIO (DBus.Socket.send sock matchedSignal (\_ -> return ()))
	received <- liftIO (takeMVar signalVar)
	$expect (equal received (busName_ "com.example.Foo", matchedSignal))

test_AutoMethod :: Test
test_AutoMethod = assertions "autoMethod" $ do
	(sock, client) <- startConnectedClient
	
	let methodMax = (\x y -> return (max x y)) :: Word32 -> Word32 -> IO Word32
	
	let methodPair = (\x y -> return (x, y)) :: String -> String -> IO (String, String)
	
	liftIO (DBus.Client.export client (objectPath_ "/")
		[ DBus.Client.autoMethod (interfaceName_ "com.example.Foo") (memberName_ "Max") methodMax
		, DBus.Client.autoMethod (interfaceName_ "com.example.Foo") (memberName_ "Pair") methodPair
		])
	
	-- valid call to com.example.Foo.Max
	do
		(serial, response) <- callClientMethod sock "/" "com.example.Foo" "Max" [toVariant (2 :: Word32), toVariant (1 :: Word32)]
		$expect (equal response (Right (MethodReturn
			{ methodReturnSerial = serial
			, methodReturnSender = Nothing
			, methodReturnDestination = Nothing
			, methodReturnBody = [toVariant (2 :: Word32)]
			})))
	
	-- valid call to com.example.Foo.Pair
	do
		(serial, response) <- callClientMethod sock "/" "com.example.Foo" "Pair" [toVariant "x", toVariant "y"]
		$expect (equal response (Right (MethodReturn
			{ methodReturnSerial = serial
			, methodReturnSender = Nothing
			, methodReturnDestination = Nothing
			, methodReturnBody = [toVariant "x", toVariant "y"]
			})))
	
	-- invalid call to com.example.Foo.Max
	do
		(serial, response) <- callClientMethod sock "/" "com.example.Foo" "Max" [toVariant "x", toVariant "y"]
		$expect (equal response (Left (MethodError
			{ methodErrorName = errorName_ "org.freedesktop.DBus.Error.InvalidParameters"
			, methodErrorSerial = serial
			, methodErrorSender = Nothing
			, methodErrorDestination = Nothing
			, methodErrorBody = []
			})))

startDummyBus :: Assertions (Address, MVar DBus.Socket.Socket)
startDummyBus = do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList [("abstract", formatUUID uuid)])
	listener <- liftIO (DBus.Socket.listen addr)
	sockVar <- forkVar (DBus.Socket.accept listener)
	return (DBus.Socket.socketListenerAddress listener, sockVar)

startConnectedClient :: Assertions (DBus.Socket.Socket, DBus.Client.Client)
startConnectedClient = do
	(addr, sockVar) <- startDummyBus
	clientVar <- forkVar (DBus.Client.connect addr)
	
	-- TODO: verify that 'hello' contains expected data, and
	-- send a properly formatted reply.
	sock <- liftIO (readMVar sockVar)
	receivedHello <- liftIO (DBus.Socket.receive sock)
	let (ReceivedMethodCall helloSerial _) = receivedHello
	
	liftIO (DBus.Socket.send sock (MethodReturn
		{ methodReturnSerial = helloSerial
		, methodReturnSender = Nothing
		, methodReturnDestination = Nothing
		, methodReturnBody = []
		}) (\_ -> return ()))
	
	client <- liftIO (readMVar clientVar)
	afterTest (DBus.Client.disconnect client)
	
	return (sock, client)

stubMethodCall :: DBus.Socket.Socket -> IO a -> MethodCall -> (Serial -> MethodReturn) -> Assertions a
stubMethodCall sock io expectedCall respond = do
	var <- forkVar io
	
	receivedCall <- liftIO (DBus.Socket.receive sock)
	let ReceivedMethodCall callSerial call = receivedCall
	$expect (equal expectedCall call)
	
	liftIO (DBus.Socket.send sock (respond callSerial) (\_ -> return ()))
	
	liftIO (takeMVar var)

callClientMethod :: DBus.Socket.Socket -> String -> String -> String -> [Variant] -> Assertions (Serial, Either MethodError MethodReturn)
callClientMethod sock path iface name body = do
	let call = MethodCall
		{ methodCallPath = objectPath_ path
		, methodCallInterface = Just (interfaceName_ iface)
		, methodCallMember = memberName_ name
		, methodCallSender = Nothing
		, methodCallDestination = Nothing
		, methodCallFlags = []
		, methodCallBody = body
		}
	serial <- liftIO (DBus.Socket.send sock call return)
	resp <- liftIO (DBus.Socket.receive sock)
	case resp of
		ReceivedMethodReturn _ ret -> return (serial, Right ret)
		ReceivedMethodError _ err -> return (serial, Left err)
		_ -> $die "callClientMethod: unexpected response to method call"
