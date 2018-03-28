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

module DBusTests.MemberName (test_MemberName) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           DBus

import           DBusTests.Util

test_MemberName :: Suite
test_MemberName = suite "MemberName"
	[ test_Parse
	, test_ParseInvalid
	, test_IsVariant
	]

test_Parse :: Test
test_Parse = property "parse" prop where
	prop = forAll gen_MemberName check
	check x = case parseMemberName x of
		Nothing -> False
		Just parsed -> formatMemberName parsed == x

test_ParseInvalid :: Test
test_ParseInvalid = assertions "parse-invalid" $ do
	-- empty
	$expect (nothing (parseMemberName ""))
	
	-- starts with a digit
	$expect (nothing (parseMemberName "@foo"))
	
	-- trailing chars
	$expect (nothing (parseMemberName "foo!"))
	
	-- at most 255 characters
	$expect (just (parseMemberName (replicate 254 'y')))
	$expect (just (parseMemberName (replicate 255 'y')))
	$expect (nothing (parseMemberName (replicate 256 'y')))

test_IsVariant :: Test
test_IsVariant = assertions "IsVariant" $ do
	assertVariant TypeString (memberName_ "foo")

gen_MemberName :: Gen String
gen_MemberName = gen where
	alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
	alphanum = alpha ++ ['0'..'9']
	
	gen = do
		x <- elements alpha
		xs <- listOf (elements alphanum)
		return (x:xs)

instance Arbitrary MemberName where
	arbitrary = fmap memberName_ gen_MemberName
