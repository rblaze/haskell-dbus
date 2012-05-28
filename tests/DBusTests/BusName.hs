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

module DBusTests.BusName (test_BusName) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.List (intercalate)

import           DBus

import           DBusTests.Util

test_BusName :: Suite
test_BusName = suite "BusName"
	test_Parse
	test_ParseInvalid
	test_IsVariant

test_Parse :: Test
test_Parse = property "parse" prop where
	prop = forAll gen_BusName check
	check x = case parseBusName x of
		Nothing -> False
		Just parsed -> formatBusName parsed == x

test_ParseInvalid :: Test
test_ParseInvalid = assertions "parse-invalid" $ do
	-- empty
	$expect (nothing (parseBusName ""))
	
	-- well-known starting with a digit
	$expect (nothing (parseBusName "foo.0bar"))
	
	-- well-known with one element
	$expect (nothing (parseBusName "foo"))
	
	-- unique with one element
	$expect (nothing (parseBusName ":foo"))
	
	-- trailing characters
	$expect (nothing (parseBusName "foo.bar!"))
	
	-- at most 255 characters
	$expect (just (parseBusName (":0." ++ replicate 251 'y')))
	$expect (just (parseBusName (":0." ++ replicate 252 'y')))
	$expect (nothing (parseBusName (":0." ++ replicate 253 'y')))

test_IsVariant :: Test
test_IsVariant = assertions "IsVariant" $ do
	assertVariant TypeString (busName_ "foo.bar")

gen_BusName :: Gen String
gen_BusName = oneof [unique, wellKnown] where
	alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_-"
	alphanum = alpha ++ ['0'..'9']
	
	unique = trim $ do
		x <- chunks alphanum
		return (":" ++ x)
	wellKnown = trim (chunks alpha)
	
	trim gen = do
		x <- gen
		if length x > 255
			then return (dropWhileEnd (== '.') (take 255 x))
			else return x
	
	chunks start = do
		x <- chunk start
		xs <- listOf1 (chunk start)
		return (intercalate "." (x:xs))
	chunk start = do
		x <- elements start
		xs <- listOf (elements alphanum)
		return (x:xs)

instance Arbitrary BusName where
	arbitrary = fmap busName_ gen_BusName
