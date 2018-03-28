{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

import           Test.Chell

import           Control.Exception (finally)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory (removeFile)
import           System.Exit
import           System.IO (hGetLine, writeFile)
import           System.Process

import           DBus
import           DBus.Socket
import           DBus.Client
import           DBusTests.Util (getTempPath)

test_Integration :: Suite
test_Integration = suite "integration"
	[ test_Socket
	, test_Client
	]

test_Socket :: Test
test_Socket = withDaemon "socket" $ \addr -> do
	let hello = (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "Hello")
		{ methodCallDestination = Just "org.freedesktop.DBus"
		}
	
	sock <- liftIO (open addr)
	serial <- liftIO (send sock hello return)
	$expect (greaterEqual (serialValue serial) 1)
	
	received <- liftIO (receive sock)
	let ReceivedMethodReturn _ ret = received
	$expect (equal (methodReturnSerial ret) serial)
	$expect (equal (methodReturnSender ret) (Just "org.freedesktop.DBus"))
	
	liftIO (close sock)

test_Client :: Test
test_Client = withDaemon "client" $ \addr -> do
	clientA <- liftIO (connect addr)
	clientB <- liftIO (connect addr)
	
	liftIO (export clientA "/"
		[ method "com.example.Echo" "Echo" (signature_ [TypeString]) (signature_ []) (
			\msg -> if map variantType (methodCallBody msg) == [TypeString]
				then return (replyReturn (methodCallBody msg))
				else return (replyError "com.example.Error" [toVariant ("bad body: " ++ show (methodCallBody msg))]))
		])
	
	-- TODO: get bus address of clientA with a function
	let busAddrA = ":1.0"
	
	-- Successful call
	let bodyGood = [toVariant ("test" :: String)]
	retGood <- liftIO (call clientB (methodCall "/" "com.example.Echo" "Echo")
		{ methodCallDestination = Just busAddrA
		, methodCallBody = bodyGood
		})
	ret <- $requireRight retGood
	$expect (equal (methodReturnBody ret) bodyGood)
	
	-- Failed call
	let bodyBad = [toVariant True]
	retBad <- liftIO (call clientB (methodCall "/" "com.example.Echo" "Echo")
		{ methodCallDestination = Just busAddrA
		, methodCallBody = bodyBad
		})
	err <- $requireLeft retBad
	$expect (equal (methodErrorName err) "com.example.Error")
	$expect (equal (methodErrorBody err) [toVariant ("bad body: [Variant True]" :: String)])
	
	liftIO (disconnect clientA)
	liftIO (disconnect clientB)

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

withDaemon :: String -> (Address -> Assertions ()) -> Test
withDaemon name io = test name $ \opts -> do
	(versionExit, _, _) <- readProcessWithExitCode "dbus-daemon" ["--version"] ""
	case versionExit of
		ExitFailure _ -> return TestSkipped
		ExitSuccess -> do
			configFilePath <- liftIO getTempPath
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
						Nothing -> return (TestAborted [] ("dbus-daemon returned invalid address: " ++ show addrString))
						Just addr -> runTest (assertions name (io addr)) opts)
				(do
					terminateProcess daemonProc
					_ <- waitForProcess daemonProc
					removeFile configFilePath
					return ())
