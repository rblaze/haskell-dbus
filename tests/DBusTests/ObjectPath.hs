{-# LANGUAGE OverloadedStrings #-}
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

module DBusTests.ObjectPath (test_ObjectPath) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T

import           DBus

test_ObjectPath :: Suite
test_ObjectPath = suite "ObjectPath"
	test_Parse
	test_ParseInvalid

test_Parse :: Test
test_Parse = property "parse" prop where
	prop = forAll gen_ObjectPath check
	check x = case objectPath x of
		Nothing -> False
		Just parsed -> objectPathText parsed == x

test_ParseInvalid :: Test
test_ParseInvalid = assertions "parse-invalid" $ do
	-- empty
	$expect (nothing (objectPath ""))
	
	-- bad char
	$expect (nothing (objectPath "/f!oo"))
	
	-- ends with a slash
	$expect (nothing (objectPath "/foo/"))
	
	-- empty element
	$expect (nothing (objectPath "/foo//bar"))
	
	-- trailing chars
	$expect (nothing (objectPath "/foo!"))

gen_ObjectPath :: Gen Text
gen_ObjectPath = gen where
	chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
	
	gen = do
		xs <- listOf (listOf1 (elements chars))
		return (T.pack ("/" ++ intercalate "/" xs))

instance Arbitrary ObjectPath where
	arbitrary = fmap (objectPath_ . T.unpack) gen_ObjectPath
