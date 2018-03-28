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

module DBusTests.ObjectPath (test_ObjectPath) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.List (intercalate)

import           DBus

test_ObjectPath :: Suite
test_ObjectPath = suite "ObjectPath"
	[ test_Parse
	, test_ParseInvalid
	]

test_Parse :: Test
test_Parse = property "parse" prop where
	prop = forAll gen_ObjectPath check
	check x = case parseObjectPath x of
		Nothing -> False
		Just parsed -> formatObjectPath parsed == x

test_ParseInvalid :: Test
test_ParseInvalid = assertions "parse-invalid" $ do
	-- empty
	$expect (nothing (parseObjectPath ""))
	
	-- bad char
	$expect (nothing (parseObjectPath "/f!oo"))
	
	-- ends with a slash
	$expect (nothing (parseObjectPath "/foo/"))
	
	-- empty element
	$expect (nothing (parseObjectPath "/foo//bar"))
	
	-- trailing chars
	$expect (nothing (parseObjectPath "/foo!"))

gen_ObjectPath :: Gen String
gen_ObjectPath = gen where
	chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
	
	gen = do
		xs <- listOf (listOf1 (elements chars))
		return ("/" ++ intercalate "/" xs)

instance Arbitrary ObjectPath where
	arbitrary = fmap objectPath_ gen_ObjectPath
