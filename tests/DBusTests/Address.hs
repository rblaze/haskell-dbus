{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2012 John Millikin <jmillikin@gmail.com>
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

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.Char (ord)
import           Data.List (intercalate)
import           Data.Map (Map)
import qualified Data.Map
import           Text.Printf (printf)

import           DBus

import           DBusTests.Util (smallListOf, smallListOf1, withEnv)

test_Address :: Suite
test_Address = suite "Address"
	test_BuildAddress
	test_ParseAddress
	test_ParseAddresses
	test_ParseInvalid
	test_FormatAddress
	test_FormatAddresses
	test_GetSystemAddress
	test_GetSessionAddress
	test_GetStarterAddress

test_BuildAddress :: Test
test_BuildAddress = property "address" prop where
	prop = forAll gen_Address check
	check (method, params) = case address method params of
		Nothing -> False
		Just addr -> and
			[ addressMethod addr == method
			, addressParameters addr == params
			]

test_ParseAddress :: Test
test_ParseAddress = property "parseAddress" prop where
	prop = forAll gen_AddressBytes check
	check (bytes, method, params) = case parseAddress bytes of
		Nothing -> False
		Just addr -> and
			[ addressMethod addr == method
			, addressParameters addr == params
			]

test_ParseAddresses :: Test
test_ParseAddresses = property "parseAddresses" prop where
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

test_ParseInvalid :: Test
test_ParseInvalid = assertions "parse-invalid" $ do
	-- empty
	$expect (nothing (address "" Data.Map.empty))
	$expect (nothing (parseAddress ""))
	
	-- no colon
	$expect (nothing (parseAddress "a"))
	
	-- no equals sign
	$expect (nothing (parseAddress "a:b"))
	
	-- no parameter
	-- TODO: should this be OK? what about the trailing comma rule?
	$expect (nothing (parseAddress "a:,"))
	
	-- no key
	$expect (nothing (address "" (Data.Map.fromList [("", "c")])))
	$expect (nothing (parseAddress "a:=c"))
	
	-- no value
	$expect (nothing (address "" (Data.Map.fromList [("b", "")])))
	$expect (nothing (parseAddress "a:b="))

test_FormatAddress :: Test
test_FormatAddress = property "formatAddress" prop where
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

test_FormatAddresses :: Test
test_FormatAddresses = property "formatAddresses" prop where
	prop = forAll (smallListOf1 gen_Address) check where
	check pairs = let
		addrs = do
			(method, params) <- pairs
			let Just addr = address method params
			return addr
		bytes = formatAddresses addrs
		parsed = parseAddresses bytes
		in parsed == Just addrs

test_GetSystemAddress :: Test
test_GetSystemAddress = assertions "getSystemAddress" $ do
	do
		addr <- withEnv "DBUS_SYSTEM_BUS_ADDRESS" Nothing getSystemAddress
		$expect (just addr)
		$assert (equal addr (address "unix" (Data.Map.fromList [("path", "/var/run/dbus/system_bus_socket")])))
	do
		addr <- withEnv "DBUS_SYSTEM_BUS_ADDRESS" (Just "a:b=c") getSystemAddress
		$expect (just addr)
		$assert (equal addr (address "a" (Data.Map.fromList [("b", "c")])))

test_GetSessionAddress :: Test
test_GetSessionAddress = assertions "getSessionAddress" $ do
	addr <- withEnv "DBUS_SESSION_BUS_ADDRESS" (Just "a:b=c") getSessionAddress
	$expect (just addr)
	$assert (equal addr (address "a" (Data.Map.fromList [("b", "c")])))

test_GetStarterAddress :: Test
test_GetStarterAddress = assertions "getStarterAddress" $ do
	addr <- withEnv "DBUS_STARTER_ADDRESS" (Just "a:b=c") getStarterAddress
	$expect (just addr)
	$assert (equal addr (address "a" (Data.Map.fromList [("b", "c")])))

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
