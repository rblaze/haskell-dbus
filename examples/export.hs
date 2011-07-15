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
import           Control.Monad (forever)
import           Data.Text
import           DBus.Client.Simple

onFoo :: Text -> Text -> IO (Text, Text)
onFoo x y = do
	putStrLn ("Foo " ++ unpack x ++ " " ++ unpack y)
	return (x, y)

onBar :: Text -> Text -> IO (Text, Text)
onBar x y = do
	putStrLn ("Bar " ++ unpack x ++ " " ++ unpack y)
	throwError "com.example.ErrorBar" "Bar failed" []

main :: IO ()
main = do
	-- Connect to the bus
	client <- connectSession
	
	-- Request a unique name on the bus. If the name is already
	-- in use, continue without it.
	_ <- requestName client "com.example.exporting" []
	
	-- Export two example objects
	export client "/a"
		[ method "test.iface_1" "Foo" (onFoo "hello" "a")
		, method "test.iface_1" "Bar" (onBar "hello" "a")
		]
	export client "/b"
		[ method "test.iface_1" "Foo" (onFoo "hello")
		, method "test.iface_1" "Bar" (onBar "hello")
		]
	
	-- Wait forever for method calls
	forever (threadDelay 50000)
