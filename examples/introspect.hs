{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2009-2011 John Millikin <jmillikin@gmail.com>
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

module Main (main) where
import           Data.String (fromString)
import qualified Data.Text
import           System (getArgs, getProgName, exitFailure)
import           System.IO (hPutStrLn, stderr)

import           DBus.Client.Simple
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
	obj <- proxy client service path
	reply <- call obj "org.freedesktop.DBus.Introspectable" "Introspect" []
	let Just xml = fromVariant (reply !! 0)
	case I.fromXML path xml of
		Just info -> return info
		Nothing -> error ("Invalid introspection XML: " ++ show xml)

-- most of this stuff is just boring text formatting

printObj :: (ObjectPath -> IO I.Object) -> ObjectPath -> IO ()
printObj get path = do
	(I.Object _ interfaces children) <- get path
	putStrLn (Data.Text.unpack (objectPathText path))
	mapM_ printIface interfaces
	putStrLn ""
	mapM_ (printObj get) [x | (I.Object x _ _) <- children]

printIface :: I.Interface -> IO ()
printIface (I.Interface name methods signals properties) = do
	putStr "    "
	putStrLn (Data.Text.unpack (interfaceNameText name))
	
	mapM_ printMethod methods
	mapM_ printSignal signals
	mapM_ printProperty properties
	putStrLn ""

printMethod :: I.Method -> IO ()
printMethod (I.Method name inParams outParams) = do
	putStr "        method "
	putStrLn (Data.Text.unpack (memberNameText name))
	
	mapM_ (printParam "IN") inParams
	mapM_ (printParam "OUT") outParams

printSignal :: I.Signal -> IO ()
printSignal (I.Signal name params) = do
	putStr "        signal "
	putStrLn (Data.Text.unpack (memberNameText name))
	
	mapM_ (printParam "OUT") params

printProperty :: I.Property -> IO ()
printProperty (I.Property name sig access) = do
	putStr "        property "
	putStr (show (signatureText sig) ++ " ")
	putStrLn (Data.Text.unpack name)
	
	putStr "            "
	putStrLn (show access)
	
printParam :: String -> I.Parameter -> IO ()
printParam label (I.Parameter name sig) = do
	putStr ("            [" ++ label ++ " ")
	putStr (show (signatureText sig) ++ "] ")
	putStrLn (Data.Text.unpack name)
