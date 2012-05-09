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
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Network as N
import           System.IO
import           System.Random (randomIO)

import qualified Data.UUID as UUID

import           DBus
import           DBus.Socket
import           DBus.Transport
import           DBus.Util (readUntil)

import           DBusTests.Util (listenRandomIPv4)

test_Socket :: Suite
test_Socket = assertions "Socket" $ do
	uuid <- liftIO randomIO
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", Char8.pack (UUID.toString uuid))
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
	
	listened <- liftIO (listenWith (defaultSocketOptions
		{ socketAuthenticators = [dummyAuth]
		}) addr)
	$assert (right listened)
	let Right listener = listened
	afterTest (closeListener listener)
	
	acceptedVar <- forkVar (accept listener)
	openedVar <- forkVar (openWith (defaultSocketOptions
		{ socketAuthenticators = [dummyAuth]
		}) addr)
	
	accepted <- liftIO (takeMVar acceptedVar)
	$assert (right accepted)
	let Right sock1 = accepted
	afterTest (close sock1)
	
	opened <- liftIO (takeMVar openedVar)
	$assert (right opened)
	let Right sock2 = opened
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
	transportPut t "AUTH DUMMY\r\n"
	resp <- readUntil "\r\n" (readChar8 t)
	return $ case takeWhile (/= ' ') resp of
		"OK" -> True
		_ -> False

dummyAuthServer :: Transport t => t -> IO Bool
dummyAuthServer t = do
	req <- readUntil "\r\n" (readChar8 t)
	return $ case req of
		"AUTH DUMMY\r\n" -> True
		_ -> False

readChar8 :: Transport t => t -> IO Char
readChar8 t = do
	c <- transportGet t 1
	return (Char8.head c)

forkVar :: MonadIO m => IO a -> m (MVar a)
forkVar io = liftIO $ do
	var <- newEmptyMVar
	_ <- forkIO (io >>= putMVar var)
	return var
