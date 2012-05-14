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
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import qualified Data.Set as Set

import           DBus
import           DBus.Socket
import           DBus.Transport
import           DBus.Util (readUntil, randomUUID)

import           DBusTests.Util (forkVar)

test_Socket :: Suite
test_Socket = suite "Socket"
	[ test_Listen
	, test_ListenWith_CustomAuth
	, test_SendReceive
	]

test_Listen :: Suite
test_Listen = assertions "listen" $ do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", Char8.pack uuid)
		])
	
	listened <- liftIO (listen addr)
	$assert (right listened)
	let Right listener = listened
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (open addr)
	
	accepted <- liftIO (takeMVar acceptedVar)
	$assert (right accepted)
	let Right sock = accepted
	afterTest (close sock)

test_ListenWith_CustomAuth :: Suite
test_ListenWith_CustomAuth = assertions "listenWith-custom-auth" $ do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", Char8.pack uuid)
		])
	
	listened <- liftIO (listenWith (defaultSocketOptions
		{ socketAuthenticator = dummyAuth
		}) addr)
	$assert (right listened)
	let Right listener = listened
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (openWith (defaultSocketOptions
		{ socketAuthenticator = dummyAuth
		}) addr)
	
	accepted <- liftIO (takeMVar acceptedVar)
	$assert (right accepted)
	let Right sock = accepted
	afterTest (close sock)
	return ()

test_SendReceive :: Suite
test_SendReceive = assertions "send-receive" $ do
	uuid <- liftIO randomUUID
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", Char8.pack uuid)
		])
	
	let msg = MethodCall
		{ methodCallPath = "/"
		, methodCallMember = "Foo"
		, methodCallInterface = Just "org.example.iface"
		, methodCallSender = Just "org.example.src"
		, methodCallDestination = Just "org.example.dst"
		, methodCallFlags = Set.fromList [NoReplyExpected, NoAutoStart]
		, methodCallBody = [toVariant True]
		}
	
	Right listener <- liftIO (listen addr)
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (open addr)
	
	Right sock1 <- liftIO (takeMVar acceptedVar)
	afterTest (close sock1)
	
	Right sock2 <- liftIO (takeMVar openedVar)
	afterTest (close sock2)
	
	serialVar <- liftIO newEmptyMVar
	sentVar <- forkVar (send sock1 msg (putMVar serialVar))
	receivedVar <- forkVar (receive sock2)
	
	serial <- liftIO (takeMVar serialVar)
	sent <- liftIO (takeMVar sentVar)
	received <- liftIO (takeMVar receivedVar)
	
	$assert (equal sent (Right ()))
	$assert (equal received (Right (ReceivedMethodCall serial msg)))

dummyAuth :: Transport t => Authenticator t
dummyAuth = authenticator
	{ authenticatorClient = dummyAuthClient
	, authenticatorServer = dummyAuthServer
	}

dummyAuthClient :: Transport t => t -> IO Bool
dummyAuthClient t = do
	transportPut t "\x00"
	resp <- readUntil "\r\n" (readChar8 t)
	return (take 3 resp == "OK ")

dummyAuthServer :: Transport t => t -> String -> IO Bool
dummyAuthServer t uuid = do
	c <- transportGet t 1
	if c == "\x00"
		then do
			transportPut t "OK "
			transportPut t (Char8.pack uuid)
			transportPut t "\r\n"
			return True
		else return False

readChar8 :: Transport t => t -> IO Char
readChar8 t = do
	c <- transportGet t 1
	return (Char8.head c)
