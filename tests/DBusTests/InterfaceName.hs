{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
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

module DBusTests.InterfaceName (test_InterfaceName) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.List (intercalate)

import           DBus

import           DBusTests.Util

test_InterfaceName :: Suite
test_InterfaceName = suite "InterfaceName"
	[ test_Parse
	, test_ParseInvalid
	, test_IsVariant
	]

test_Parse :: Test
test_Parse = property "parse" prop where
	prop = forAll gen_InterfaceName check
	check x = case parseInterfaceName x of
		Nothing -> False
		Just parsed -> formatInterfaceName parsed == x

test_ParseInvalid :: Test
test_ParseInvalid = assertions "parse-invalid" $ do
	-- empty
	$expect (nothing (parseInterfaceName ""))
	
	-- one element
	$expect (nothing (parseInterfaceName "foo"))
	
	-- element starting with a digit
	$expect (nothing (parseInterfaceName "foo.0bar"))
	
	-- trailing characters
	$expect (nothing (parseInterfaceName "foo.bar!"))
	
	-- at most 255 characters
	$expect (just (parseInterfaceName ("f." ++ replicate 252 'y')))
	$expect (just (parseInterfaceName ("f." ++ replicate 253 'y')))
	$expect (nothing (parseInterfaceName ("f." ++ replicate 254 'y')))

test_IsVariant :: Test
test_IsVariant = assertions "IsVariant" $ do
	assertVariant TypeString (interfaceName_ "foo.bar")

gen_InterfaceName :: Gen String
gen_InterfaceName = trim chunks where
	alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
	alphanum = alpha ++ ['0'..'9']
	
	trim gen = do
		x <- gen
		if length x > 255
			then return (dropWhileEnd (== '.') (take 255 x))
			else return x
	
	chunks = do
		x <- chunk
		xs <- listOf1 chunk
		return (intercalate "." (x:xs))
	chunk = do
		x <- elements alpha
		xs <- listOf (elements alphanum)
		return (x:xs)

instance Arbitrary InterfaceName where
	arbitrary = fmap interfaceName_ gen_InterfaceName
