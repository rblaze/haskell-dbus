{-# LANGUAGE OverloadedStrings #-}

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

module DBusTests.Integration (test_Integration) where

import Control.Exception (finally)
import System.Directory (removeFile)
import System.Exit
import System.IO (hGetLine)
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import DBus
import DBus.Socket
import DBus.Client
import DBusTests.Util

test_Integration :: TestTree
test_Integration = testGroup "Integration"
    [ test_Socket
    , test_Client
    ]

test_Socket :: TestTree
test_Socket = withDaemon "socket" $ \addr -> do
    let hello = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "Hello")
            { methodCallDestination = Just "org.freedesktop.DBus"
            }

    sock <- open addr
    serial <- send sock hello return
    assertBool "invalid serial" $ serialValue serial >= 1

    received <- receive sock
    let ReceivedMethodReturn _ ret = received
    methodReturnSerial ret @?= serial
    methodReturnSender ret @?= Just "org.freedesktop.DBus"

    close sock

test_Client :: TestTree
test_Client = withDaemon "client" $ \addr -> do
    clientA <- connect addr
    clientB <- connect addr

    export clientA "/"
           defaultInterface
           { interfaceName = "com.example.Echo"
           , interfaceMethods =
               [ Method "Echo" (signature_ [TypeString]) (signature_ []) (
                 \msg -> if map variantType (methodCallBody msg) == [TypeString]
                         then return (ReplyReturn (methodCallBody msg))
                         else
                           return $ ReplyError
                                    "com.example.Error"
                                    [toVariant ("bad body: " ++ show (methodCallBody msg))])
               ]
           }

    -- TODO: get bus address of clientA with a function
    let busAddrA = ":1.0"

    -- Successful call
    let bodyGood = [toVariant ("test" :: String)]
    retGood <- call clientB (methodCall "/" "com.example.Echo" "Echo")
        { methodCallDestination = Just busAddrA
        , methodCallBody = bodyGood
        }
    ret <- requireRight retGood
    methodReturnBody ret @?= bodyGood

    -- Failed call
    let bodyBad = [toVariant True]
    retBad <- call clientB (methodCall "/" "com.example.Echo" "Echo")
        { methodCallDestination = Just busAddrA
        , methodCallBody = bodyBad
        }
    err <- requireLeft retBad
    methodErrorName err @?= "com.example.Error"
    methodErrorBody err @?= [toVariant ("bad body: [Variant True]" :: String)]

    disconnect clientA
    disconnect clientB

configFileContent :: String
configFileContent = "\
\<!DOCTYPE busconfig PUBLIC \"-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN\"\
\ \"http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd\">\
\<busconfig>\
\  <type>session</type>\
\  <keep_umask/>\
\  <listen>unix:tmpdir=/tmp</listen>\
\  <policy context=\"default\">\
\    <!-- Allow everything to be sent -->\
\    <allow send_destination=\"*\" eavesdrop=\"true\"/>\
\    <!-- Allow everything to be received -->\
\    <allow eavesdrop=\"true\"/>\
\    <!-- Allow anyone to own anything -->\
\    <allow own=\"*\"/>\
\  </policy>\
\</busconfig>"

withDaemon :: String -> (Address -> Assertion) -> TestTree
withDaemon name io = testCase name $ do
    (versionExit, _, _) <- readProcessWithExitCode "dbus-daemon" ["--version"] ""
    case versionExit of
        ExitFailure _ -> assertFailure $ "dbus-daemon failed: " ++ show versionExit
        ExitSuccess -> do
            configFilePath <- getTempPath
            writeFile configFilePath configFileContent
            daemon <- createProcess (proc "dbus-daemon" ["--config-file=" ++ configFilePath, "--print-address"])
                { std_out = CreatePipe
                , close_fds = True
                }
            let (_, Just daemonStdout, _, daemonProc) = daemon
            finally
                (do
                    addrString <- hGetLine daemonStdout
                    case parseAddress addrString of
                        Nothing -> assertFailure $ "dbus-daemon returned invalid address: " ++ show addrString
                        Just addr -> io addr)
                (do
                    terminateProcess daemonProc
                    _ <- waitForProcess daemonProc
                    removeFile configFilePath
                    return ())
