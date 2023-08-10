{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- Copyright (C) 2012 John Millikin <john@john-millikin.com>
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

module DBusTests.Socket (test_Socket) where

import Control.Concurrent
import Control.Exception
import Control.Monad (void)
import System.IO (SeekMode(..))
import System.Posix (Fd, fdRead, fdSeek, fdWrite)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map

import DBus
import DBus.Socket
import DBus.Transport

import DBusTests.Util (forkVar, nonWindowsTestCase, withTempFd)

test_Socket :: TestTree
test_Socket = testGroup "Socket"
    [ test_Listen
    , test_ListenWith_CustomAuth
    , test_SendReceive
    , test_SendReceive_FileDescriptors
    ]

test_Listen :: TestTree
test_Listen = testCase "listen" $ do
    uuid <- randomUUID
    let Just addr = address "unix" (Map.singleton "abstract" (formatUUID uuid))

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
    let Just addr = address "unix" (Map.singleton "abstract" (formatUUID uuid))

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
    let Just addr = address "unix" (Map.singleton "abstract" (formatUUID uuid))

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

test_SendReceive_FileDescriptors :: TestTree
test_SendReceive_FileDescriptors = nonWindowsTestCase "send-receive-file-descriptors" $ do
    uuid <- randomUUID
    let Just addr = address "unix" (Map.singleton "abstract" (formatUUID uuid))
    
    withTempFd $ \tmpFd -> do

        -- in order to know that the file descriptor received by the server points to
        -- the same file as the file descriptor that was sent, we write "hello" on the
        -- client and read it back on the server.
        void $ fdWrite tmpFd "hello"
        void $ fdSeek tmpFd AbsoluteSeek 0

        let msg = (methodCall "/" "org.example.iface" "Foo")
              { methodCallBody = [toVariant tmpFd] }

        bracket (listen addr) closeListener $ \listener -> do
            acceptedVar <- forkVar (accept listener)
            openedVar <- forkVar (open addr)

            bracket (takeMVar acceptedVar) close $ \sock1 -> do
                bracket (takeMVar openedVar) close $ \sock2 -> do
                    receivedVar <- forkVar (receive sock1)
                    send sock2 msg (const (pure ()))

                    received <- takeMVar receivedVar
                    case receivedMessageBody received of
                        [fromVariant -> Just (fd :: Fd)] -> do
                            assertBool ("Expected a different Fd, not " <> show tmpFd) (fd /= tmpFd)
                            (fdMsg, _) <- fdRead fd 5
                            fdMsg @?= "hello"
                        body -> do
                            assertFailure ("Expected a single Fd, not: " <> show body)

dummyAuth :: Transport t => Authenticator t
dummyAuth = authenticator
    { authenticatorClient = dummyAuthClient
    , authenticatorServer = dummyAuthServer
    }

dummyAuthClient :: Transport t => t -> IO Bool
dummyAuthClient t = do
    transportPut t "\x00" []
    resp <- transportGet t 4
    return (resp == ("OK\r\n", []))

dummyAuthServer :: Transport t => t -> UUID -> IO Bool
dummyAuthServer t _ = do
    (c, _fds) <- transportGet t 1
    if c == "\x00"
        then do
            transportPut t "OK\r\n" []
            return True
        else return False
