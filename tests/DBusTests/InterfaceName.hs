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

module DBusTests.InterfaceName (test_InterfaceName) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.List (intercalate)

import           DBus

import           DBusTests.Util

test_InterfaceName :: Suite
test_InterfaceName = suite "InterfaceName"
	test_Parse
	test_ParseInvalid
	test_IsVariant

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
