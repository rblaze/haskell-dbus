-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
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

module DBusTests.Address (test_Address) where

import Data.Char (ord)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Printf (printf)
import qualified Data.Map

import           DBus

import           DBusTests.Util (smallListOf, smallListOf1, withEnv)

test_Address :: TestTree
test_Address = testGroup "Address"
    [ test_BuildAddress
    , test_ParseAddress
    , test_ParseAddresses
    , test_ParseInvalid
    , test_FormatAddress
    , test_FormatAddresses
    , test_GetSystemAddress
    , test_GetSessionAddress
    , test_GetStarterAddress
    ]

test_BuildAddress :: TestTree
test_BuildAddress = testProperty "address" prop where
    prop = forAll gen_Address check
    check (method, params) = case address method params of
        Nothing -> False
        Just addr -> and
            [ addressMethod addr == method
            , addressParameters addr == params
            ]

test_ParseAddress :: TestTree
test_ParseAddress = testProperty "parseAddress" prop where
    prop = forAll gen_AddressBytes check
    check (bytes, method, params) = case parseAddress bytes of
        Nothing -> False
        Just addr -> and
            [ addressMethod addr == method
            , addressParameters addr == params
            ]

test_ParseAddresses :: TestTree
test_ParseAddresses = testProperty "parseAddresses" prop where
    prop = forAll gen_AddressesBytes checkMany
    checkMany (bytes, expectedAddrs) = case parseAddresses bytes of
        Nothing -> False
        Just addrs -> and
            [ length addrs == length expectedAddrs
            , and (map checkOne (zip addrs expectedAddrs))
            ]
    checkOne (addr, (method, params)) = and
        [ addressMethod addr == method
        , addressParameters addr == params
        ]

test_ParseInvalid :: TestTree
test_ParseInvalid = testCase "parse-invalid" $ do
    -- empty
    Nothing @=? address "" Data.Map.empty
    Nothing @=? parseAddress ""

    -- no colon
    Nothing @=? parseAddress "a"

    -- no equals sign
    Nothing @=? parseAddress "a:b"

    -- no parameter
    -- TODO: should this be OK? what about the trailing comma rule?
    Nothing @=? parseAddress "a:,"

    -- no key
    Nothing @=? address "" (Data.Map.fromList [("", "c")])
    Nothing @=? parseAddress "a:=c"

    -- no value
    Nothing @=? address "" (Data.Map.fromList [("b", "")])
    Nothing @=? parseAddress "a:b="

test_FormatAddress :: TestTree
test_FormatAddress = testProperty "formatAddress" prop where
    prop = forAll gen_Address check where
    check (method, params) = let
        Just addr = address method params
        bytes = formatAddress addr
        parsed = parseAddress bytes
        shown = show addr
        in and
            [ parsed == Just addr
            , shown == "Address " ++ show bytes
            ]

test_FormatAddresses :: TestTree
test_FormatAddresses = testProperty "formatAddresses" prop where
    prop = forAll (smallListOf1 gen_Address) check where
    check pairs = let
        addrs = do
            (method, params) <- pairs
            let Just addr = address method params
            return addr
        bytes = formatAddresses addrs
        parsed = parseAddresses bytes
        in parsed == Just addrs

test_GetSystemAddress :: TestTree
test_GetSystemAddress = testCase "getSystemAddress" $ do
    do
        addr <- withEnv "DBUS_SYSTEM_BUS_ADDRESS" Nothing getSystemAddress
        assertBool "can't get system address" $ isJust addr
        addr @?= address "unix" (Data.Map.fromList [("path", "/var/run/dbus/system_bus_socket")])
    do
        addr <- withEnv "DBUS_SYSTEM_BUS_ADDRESS" (Just "a:b=c") getSystemAddress
        assertBool "can't get system address" $ isJust addr
        addr @?= address "a" (Data.Map.fromList [("b", "c")])

test_GetSessionAddress :: TestTree
test_GetSessionAddress = testCase "getSessionAddress" $ do
    addr <- withEnv "DBUS_SESSION_BUS_ADDRESS" (Just "a:b=c") getSessionAddress
    assertBool "can't get session address" $ isJust addr
    addr @?= address "a" (Data.Map.fromList [("b", "c")])

test_GetStarterAddress :: TestTree
test_GetStarterAddress = testCase "getStarterAddress" $ do
    addr <- withEnv "DBUS_STARTER_ADDRESS" (Just "a:b=c") getStarterAddress
    assertBool "can't get starter address" $ isJust addr
    addr @?= address "a" (Data.Map.fromList [("b", "c")])

gen_Address :: Gen (String, Map String String)
gen_Address = gen where
    methodChars = filter (`notElem` ":;") ['!'..'~']
    keyChars = filter (`notElem` "=;,") ['!'..'~']

    param = do
        key <- smallListOf1 (elements keyChars)
        value <- smallListOf1 (elements ['\x00'..'\xFF'])
        return (key, value)

    gen = do
        params <- smallListOf param
        method <- if null params
            then smallListOf1 (elements methodChars)
            else smallListOf (elements methodChars)

        return (method, Data.Map.fromList params)

gen_AddressBytes :: Gen (String, String, Map String String)
gen_AddressBytes = gen where
    methodChars = filter (`notElem` ":;") ['!'..'~']
    keyChars = filter (`notElem` "=;,") ['!'..'~']

    plainChars = concat
        [ ['0'..'9']
        , ['a'..'z']
        , ['A'..'Z']
        , "-_/\\*."
        ]

    encodedChars = [(printf "%%%02X" (ord x), x) | x <- ['\x00'..'\xFF']]

    plainChar = do
        x <- elements plainChars
        return ([x], x)
    encodedChar = elements encodedChars

    param = do
        key <- smallListOf1 (elements keyChars)
        value <- smallListOf1 (oneof [plainChar, encodedChar])
        let (valueChunks, valueChars) = unzip value

        let str = key ++ "=" ++ concat (valueChunks)
        return (str, key, valueChars)

    gen = do
        params <- smallListOf param
        method <- if null params
            then smallListOf1 (elements methodChars)
            else smallListOf (elements methodChars)

        let paramStrs = [s | (s, _, _) <- params]
        let mapItems = [(k, v) | (_, k, v) <- params]

        let str = method ++ ":" ++ (intercalate "," paramStrs)

        return (str, method, Data.Map.fromList mapItems)

gen_AddressesBytes :: Gen (String, [(String, Map String String)])
gen_AddressesBytes = do
    addrs <- smallListOf1 gen_AddressBytes
    let bytes = [b | (b, _, _) <- addrs]
    let expected = [(m, p) | (_, m, p) <- addrs]
    return (intercalate ";" bytes, expected)
