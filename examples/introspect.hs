{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2009-2011 John Millikin <john@john-millikin.com>
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

module Main (main) where

import           Control.Monad (when)
import           Data.String (fromString)
import           System.Environment (getArgs, getProgName)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

import           DBus
import           DBus.Client
import qualified DBus.Introspection as I

main :: IO ()
main = do
	args <- getArgs
	(service, path) <- case args of
		a1:a2:_ -> return (fromString a1, fromString a2)
		_ -> do
			name <- getProgName
			hPutStrLn stderr ("Usage: " ++ name ++ " <service> <path>")
			exitFailure
	client <- connectSession
	printObj (introspect client service) path

introspect :: Client -> BusName -> ObjectPath -> IO I.Object
introspect client service path = do
	reply <- call_ client (methodCall path "org.freedesktop.DBus.Introspectable" "Introspect")
		{ methodCallDestination = Just service
		}
	let Just xml = fromVariant (methodReturnBody reply !! 0)
	case I.parseXML path xml of
		Just info -> return info
		Nothing -> error ("Invalid introspection XML: " ++ show xml)

-- most of this stuff is just boring text formatting

printObj :: (ObjectPath -> IO I.Object) -> ObjectPath -> IO ()
printObj get path = do
	obj <- get path
	putStrLn (formatObjectPath path)
	mapM_ printIface (I.objectInterfaces obj)
	putStrLn ""
	mapM_ (printObj get) [I.objectPath x | x <- I.objectChildren obj]

printIface :: I.Interface -> IO ()
printIface iface = do
	putStr "    "
	putStrLn (formatInterfaceName (I.interfaceName iface))
	
	mapM_ printMethod (I.interfaceMethods iface)
	mapM_ printSignal (I.interfaceSignals iface)
	mapM_ printProperty (I.interfaceProperties iface)
	putStrLn ""

printMethod :: I.Method -> IO ()
printMethod method = do
	putStr "        method "
	putStrLn (formatMemberName (I.methodName method))
	mapM_ printMethodArg (I.methodArgs method)

printMethodArg :: I.MethodArg -> IO ()
printMethodArg arg = do
	let dir = case I.methodArgDirection arg of
		d | d == I.directionIn -> "IN "
		d | d == I.directionOut -> "OUT"
		_ -> "   "
	putStr ("            [" ++ dir ++ " ")
	putStr (show (formatSignature (signature_ [I.methodArgType arg])) ++ "] ")
	putStrLn (I.methodArgName arg)

printSignal :: I.Signal -> IO ()
printSignal sig = do
	putStr "        signal "
	putStrLn (formatMemberName (I.signalName sig))
	mapM_ printSignalArg (I.signalArgs sig)

printSignalArg :: I.SignalArg -> IO ()
printSignalArg arg = do
	putStr "            ["
	putStr (show (formatSignature (signature_ [I.signalArgType arg])) ++ "] ")
	putStrLn (I.signalArgName arg)

printProperty :: I.Property -> IO ()
printProperty prop = do
	putStr "        property "
	putStr (show (formatSignature (signature_ [I.propertyType prop])) ++ " ")
	putStrLn (I.propertyName prop)
	
	putStr "            "
	when (I.propertyRead prop) (putStr "Read")
	when (I.propertyWrite prop) (putStr "Write")
	putStrLn ""
