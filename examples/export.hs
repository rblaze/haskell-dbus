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

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           System.Exit

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
    export client "/a" defaultInterface
             { interfaceName = "test.iface_1"
             , interfaceMethods =
               [ autoMethod "Foo" (onFoo "hello" "a")
               , autoMethod "Bar" (onBar "hello" "a")
               ]
             }
    export client "/b" defaultInterface
             { interfaceName = "test.iface_2"
             , interfaceMethods =
               [ autoMethod "Foo" (onFoo "hello")
               , autoMethod "Bar" (onBar "hello")
               ]
             }

    putStrLn "Exported objects /a and /b to bus name com.example.exporting"

    -- Wait forever for method calls
    forever (threadDelay 50000)
