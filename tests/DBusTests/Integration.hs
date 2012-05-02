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

module DBusTests.Integration (test_Integration) where

import           Test.Chell

import           Control.Exception (finally)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as ByteString
import qualified Data.Set
import qualified Data.Text as T
import           System.Exit
import           System.Process

import           DBus
import           DBus.Socket
import           DBus.Client

import           Paths_haskell_dbus_tests (getDataFileName)

test_Integration :: Suite
test_Integration = suite "integration"
	[ test_Socket
	, test_Client
	]

test_Socket :: Suite
test_Socket = withDaemon "socket" $ \addr -> do
	let hello = MethodCall
		{ methodCallPath = "/org/freedesktop/DBus"
		, methodCallMember = "Hello"
		, methodCallInterface = Just "org.freedesktop.DBus"
		, methodCallSender = Nothing
		, methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallFlags = Data.Set.empty
		, methodCallBody = []
		}
	
	Right sock <- liftIO (open addr)
	Right serial <- liftIO (send sock hello return)
	$expect (greaterEqual (serialValue serial) 1)
	
	Right received <- liftIO (receive sock)
	let ReceivedMethodReturn _ ret = received
	$expect (equal (methodReturnSerial ret) serial)
	$expect (equal (methodReturnSender ret) (Just "org.freedesktop.DBus"))
	
	liftIO (close sock)

test_Client :: Suite
test_Client = withDaemon "client" $ \addr -> do
	Right clientA <- liftIO (connect addr)
	Right clientB <- liftIO (connect addr)
	
	liftIO (export clientA "/"
		[ method "com.example.Echo" "Echo" (signature_ [TypeString]) (signature_ []) (\vs -> if map variantType vs == [TypeString]
			then return (ReplyReturn vs)
			else return (ReplyError "com.example.Error" [toVariant ("bad body: " ++ show vs)]))
		])
	
	-- TODO: get bus address of clientA with a function
	let busAddrA = ":1.0"
	
	-- Successful call
	let bodyGood = [toVariant ("test" :: String)]
	retGood <- liftIO (call clientB (MethodCall
		{ methodCallPath = "/"
		, methodCallMember = "Echo"
		, methodCallInterface = Just "com.example.Echo"
		, methodCallSender = Nothing
		, methodCallDestination = Just busAddrA
		, methodCallFlags = Data.Set.empty
		, methodCallBody = bodyGood
		}))
	$assert (right retGood)
	let Right ret = retGood
	$expect (equal (methodReturnBody ret) bodyGood)
	
	-- Failed call
	let bodyBad = [toVariant True]
	retBad <- liftIO (call clientB (MethodCall
		{ methodCallPath = "/"
		, methodCallMember = "Echo"
		, methodCallInterface = Just "com.example.Echo"
		, methodCallSender = Nothing
		, methodCallDestination = Just busAddrA
		, methodCallFlags = Data.Set.empty
		, methodCallBody = bodyBad
		}))
	$assert (left retBad)
	let Left err = retBad
	$expect (equal (methodErrorName err) "com.example.Error")
	$expect (equal (methodErrorBody err) [toVariant ("bad body: [Variant True]" :: String)])
	
	liftIO (disconnect clientA)
	liftIO (disconnect clientB)

withDaemon :: T.Text -> (Address -> Assertions ()) -> Suite
withDaemon name io = test $ Test name $ \opts -> do
	(versionExit, _, _) <- readProcessWithExitCode "dbus-daemon" ["--version"] ""
	case versionExit of
		ExitFailure _ -> return TestSkipped
		ExitSuccess -> do
			configFilePath <- getDataFileName "data/dbus-daemon.xml"
			daemon <- createProcess (proc "dbus-daemon" ["--config-file=" ++ configFilePath, "--print-address"])
				{ std_out = CreatePipe
				, close_fds = True
				}
			let (_, Just daemonStdout, _, daemonProc) = daemon
			finally
				(do
					addrString <- ByteString.hGetLine daemonStdout
					case parseAddress addrString of
						Nothing -> return (TestAborted [] (T.pack ("dbus-daemon returned invalid address: " ++ show addrString)))
						Just addr -> case suiteTests (assertions name (io addr)) of
							[t] -> runTest t opts
							_ -> return (TestAborted [] (T.pack ("withDaemon: only one test expected" ++ show addrString))))
				(do
					terminateProcess daemonProc
					_ <- waitForProcess daemonProc
					return ())
