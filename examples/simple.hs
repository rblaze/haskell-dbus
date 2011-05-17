-- Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
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

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import DBus.Bus
import DBus.Connection
import DBus.Message
import DBus.Types
import qualified Data.Set as Set
import Data.List (sort)

main :: IO ()
main = do
	-- Connect to the bus, and print which name was assigned to this
	-- connection.
	(bus, name) <- getSessionBus
	putStrLn $ "Connected as: " ++ show name
	
	-- Request a list of connected clients from the bus
	Right serial <- send bus return $ MethodCall
		{ methodCallPath = "/org/freedesktop/DBus"
		, methodCallMember = "ListNames"
		, methodCallInterface = Just "org.freedesktop.DBus"
		, methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallFlags = Set.empty
		, methodCallBody = []
		}
	
	-- Wait for the reply
	reply <- waitForReply bus serial
	
	-- Pull out the body, and convert it to [String]
	let Just names = fromArray =<< fromVariant (messageBody reply !! 0)
	
	-- Print each name on a line, sorted so reserved names are below
	-- temporary names.
	mapM_ putStrLn $ sort names

waitForReply :: Connection -> Serial -> IO MethodReturn
waitForReply bus serial = wait where
	wait = do
		received <- receive bus
		case received of
			Right (ReceivedMethodReturn _ _ ret) ->
				if methodReturnSerial ret == serial
					then return ret
					else wait
			Right _ -> wait
			Left err -> error $ show err

