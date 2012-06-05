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

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           System.Exit

import           DBus
import           DBus.Client

onFoo :: String -> String -> IO (String, String)
onFoo x y = do
	putStrLn ("Foo " ++ show x ++ " " ++ show y)
	return (x, y)

onBar :: String -> String -> IO (String, String)
onBar x y = do
	putStrLn ("Bar " ++ show x ++ " " ++ show y)
	throwError "com.example.ErrorBar" "Bar failed" []

main :: IO ()
main = do
	-- Connect to the bus
	client <- connectSession
	
	-- Request a unique name on the bus.
	requestResult <- requestName client "com.example.exporting" []
	when (requestResult /= NamePrimaryOwner) $ do
		putStrLn "Another service owns the \"com.example.exporting\" bus name"
		exitFailure
	
	-- Export two example objects
	export client "/a"
		[ autoMethod "test.iface_1" "Foo" (onFoo "hello" "a")
		, autoMethod "test.iface_1" "Bar" (onBar "hello" "a")
		]
	export client "/b"
		[ autoMethod "test.iface_1" "Foo" (onFoo "hello")
		, autoMethod "test.iface_1" "Bar" (onBar "hello")
		]
	
	putStrLn "Exported objects /a and /b to bus name com.example.exporting"
	
	-- Wait forever for method calls
	forever (threadDelay 50000)
