{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map

import           Test.Chell

import           DBus
import qualified DBus.Client
import qualified DBus.Socket

import           DBusTests.Util (forkVar, withEnv)

test_Client :: Suite
test_Client = suite "Client"
	test_ConnectSystem
	test_ConnectSession
	test_ConnectStarter

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

test_ConnectSystem :: Test
test_ConnectSystem = test_Connect "connectSystem" $ \addr -> do
	let addrEnv = Char8.unpack (formatAddress addr)
	withEnv "DBUS_SYSTEM_BUS_ADDRESS"
		(Just addrEnv)
		DBus.Client.connectSystem

test_ConnectSession :: Test
test_ConnectSession = test_Connect "connectSession" $ \addr -> do
	let addrEnv = Char8.unpack (formatAddress addr)
	withEnv "DBUS_SESSION_BUS_ADDRESS"
		(Just addrEnv)
		DBus.Client.connectSession

test_ConnectStarter :: Test
test_ConnectStarter = test_Connect "connectStarter" $ \addr -> do
	let addrEnv = Char8.unpack (formatAddress addr)
	withEnv "DBUS_STARTER_ADDRESS"
		(Just addrEnv)
		DBus.Client.connectStarter

startDummyBus :: Assertions (Address, MVar DBus.Socket.Socket)
startDummyBus = do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList [("abstract", formatUUID uuid)])
	listener <- liftIO (DBus.Socket.listen addr)
	sockVar <- forkVar (DBus.Socket.accept listener)
	return (DBus.Socket.socketListenerAddress listener, sockVar)
