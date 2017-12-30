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

module DBusTests.Socket (test_Socket) where

import Control.Concurrent
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import DBus
import DBus.Socket
import DBus.Transport

import DBusTests.Util (forkVar)

test_Socket :: TestTree
test_Socket = testGroup "Socket"
    [ test_Listen
    , test_ListenWith_CustomAuth
    , test_SendReceive
    ]

test_Listen :: TestTree
test_Listen = testCase "listen" $ do
    uuid <- randomUUID
    let Just addr = address "unix" (Map.fromList
            [ ("abstract", formatUUID uuid)
            ])

    bracket (listen addr) closeListener $ \listener -> do
        acceptedVar <- forkVar (accept listener)
        openedVar <- forkVar (open addr)

        sock1 <- takeMVar acceptedVar
        sock2 <- takeMVar openedVar
        close sock1
        close sock2

test_ListenWith_CustomAuth :: TestTree
test_ListenWith_CustomAuth = testCase "listenWith-custom-auth" $ do
    uuid <- randomUUID
    let Just addr = address "unix" (Map.fromList
            [ ("abstract", formatUUID uuid)
            ])

    bracket (listenWith (defaultSocketOptions
            { socketAuthenticator = dummyAuth
            }) addr) closeListener $ \listener -> do
        acceptedVar <- forkVar (accept listener)
        openedVar <- forkVar (openWith (defaultSocketOptions
            { socketAuthenticator = dummyAuth
            }) addr)

        sock1 <- takeMVar acceptedVar
        sock2 <- takeMVar openedVar
        close sock1
        close sock2

test_SendReceive :: TestTree
test_SendReceive = testCase "send-receive" $ do
    uuid <- randomUUID
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

    bracket (listen addr) closeListener $ \listener -> do
        acceptedVar <- forkVar (accept listener)
        openedVar <- forkVar (open addr)

        bracket (takeMVar acceptedVar) close $ \sock1 -> do
        bracket (takeMVar openedVar) close $ \sock2 -> do
            -- client -> server
            do
                serialVar <- newEmptyMVar
                sentVar <- forkVar (send sock2 msg (putMVar serialVar))
                receivedVar <- forkVar (receive sock1)

                serial <- takeMVar serialVar
                sent <- takeMVar sentVar
                received <- takeMVar receivedVar

                sent @?= ()
                received @?= ReceivedMethodCall serial msg

            -- server -> client
            do
                serialVar <- newEmptyMVar
                sentVar <- forkVar (send sock1 msg (putMVar serialVar))
                receivedVar <- forkVar (receive sock2)

                serial <- takeMVar serialVar
                sent <- takeMVar sentVar
                received <- takeMVar receivedVar

                sent @?= ()
                received @?= ReceivedMethodCall serial msg

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
