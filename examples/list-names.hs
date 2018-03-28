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

import           Data.List (sort)
import           DBus
import           DBus.Client

main :: IO ()
main = do
	client <- connectSession
	
	-- Request a list of connected clients from the bus
	reply <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
		{ methodCallDestination = Just "org.freedesktop.DBus"
		}
	
	-- org.freedesktop.DBus.ListNames returns a single value, which is
	-- a list of names (here represented as [String])
	let Just names = fromVariant (methodReturnBody reply !! 0)
	
	-- Print each name on a line, sorted so reserved names are below
	-- temporary names.
	mapM_ putStrLn (sort names)
