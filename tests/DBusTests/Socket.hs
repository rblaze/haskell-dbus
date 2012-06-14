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
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Map as Map

import           DBus
import           DBus.Socket
import           DBus.Transport

import           DBusTests.Util (forkVar)

test_Socket :: Suite
test_Socket = suite "Socket"
	test_Listen
	test_ListenWith_CustomAuth
	test_SendReceive

test_Listen :: Test
test_Listen = assertions "listen" $ do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", formatUUID uuid)
		])
	
	listener <- liftIO (listen addr)
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (open addr)
	
	sock1 <- liftIO (takeMVar acceptedVar)
	afterTest (close sock1)
	
	sock2 <- liftIO (takeMVar openedVar)
	afterTest (close sock2)

test_ListenWith_CustomAuth :: Test
test_ListenWith_CustomAuth = assertions "listenWith-custom-auth" $ do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", formatUUID uuid)
		])
	
	listener <- liftIO (listenWith (defaultSocketOptions
		{ socketAuthenticator = dummyAuth
		}) addr)
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (openWith (defaultSocketOptions
		{ socketAuthenticator = dummyAuth
		}) addr)
	
	sock1 <- liftIO (takeMVar acceptedVar)
	afterTest (close sock1)
	
	sock2 <- liftIO (takeMVar openedVar)
	afterTest (close sock2)

test_SendReceive :: Test
test_SendReceive = assertions "send-receive" $ do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", formatUUID uuid)
		])
	
	let msg = (methodCall "/" "org.example.iface" "Foo")
		{ methodCallSender = Just "org.example.src"
		, methodCallDestination = Just "org.example.dst"
		, methodCallAutoStart = False
		, methodCallReplyExpected = False
		, methodCallBody = [toVariant True]
		}
	
	listener <- liftIO (listen addr)
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (open addr)
	
	sock1 <- liftIO (takeMVar acceptedVar)
	afterTest (close sock1)
	
	sock2 <- liftIO (takeMVar openedVar)
	afterTest (close sock2)
	
	-- client -> server
	do
		serialVar <- liftIO newEmptyMVar
		sentVar <- forkVar (send sock2 msg (putMVar serialVar))
		receivedVar <- forkVar (receive sock1)
		
		serial <- liftIO (takeMVar serialVar)
		sent <- liftIO (takeMVar sentVar)
		received <- liftIO (takeMVar receivedVar)
		
		$assert (equal sent ())
		$assert (equal received (ReceivedMethodCall serial msg))
	
	-- server -> client
	do
		serialVar <- liftIO newEmptyMVar
		sentVar <- forkVar (send sock1 msg (putMVar serialVar))
		receivedVar <- forkVar (receive sock2)
		
		serial <- liftIO (takeMVar serialVar)
		sent <- liftIO (takeMVar sentVar)
		received <- liftIO (takeMVar receivedVar)
		
		$assert (equal sent ())
		$assert (equal received (ReceivedMethodCall serial msg))

dummyAuth :: Transport t => Authenticator t
dummyAuth = authenticator
	{ authenticatorClient = dummyAuthClient
	, authenticatorServer = dummyAuthServer
	}

dummyAuthClient :: Transport t => t -> IO Bool
dummyAuthClient t = do
	transportPut t "\x00"
	resp <- transportGet t 4
	return (resp == "OK\r\n")

dummyAuthServer :: Transport t => t -> UUID -> IO Bool
dummyAuthServer t _ = do
	c <- transportGet t 1
	if c == "\x00"
		then do
			transportPut t "OK\r\n"
			return True
		else return False
