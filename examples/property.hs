{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2025 Kevin Buhr <buhr@asaurus.net>
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

import           System.Environment
import           System.IO

import           DBus
import qualified DBus.Client as DBus

wirelessPropertyMethod :: MethodCall
wirelessPropertyMethod =
    (methodCall "/org/freedesktop/NetworkManager" "org.freedesktop.NetworkManager" "WirelessEnabled")
    { methodCallDestination = Just "org.freedesktop.NetworkManager" }

getWirelessProperty :: IO ()
getWirelessProperty = do
    client <- DBus.connectSystem
    res <- DBus.getPropertyValue client wirelessPropertyMethod
    case res of
        Left err -> print err
        Right True  -> putStrLn "Wireless enabled"
        Right False -> putStrLn "Wireless disabled"

setWirelessProperty :: Bool -> IO ()
setWirelessProperty b = do
    client <- DBus.connectSystem
    res <- DBus.setPropertyValue client wirelessPropertyMethod b
    case res of
        Just err -> print err
        Nothing -> getWirelessProperty

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["enable"]  -> setWirelessProperty True
        ["disable"] -> setWirelessProperty False
        ["query"]   -> getWirelessProperty
        _ -> do
          cmd <- getProgName
          hPutStrLn stderr $ "syntax: " ++ cmd ++ " [enable|disable|query]"
