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

module DBusTests.ErrorName (test_ErrorName) where

import Data.List (intercalate)
import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DBus

import DBusTests.Util

test_ErrorName :: TestTree
test_ErrorName = testGroup "ErrorName"
    [ test_Parse
    , test_ParseInvalid
    , test_IsVariant
    ]

test_Parse :: TestTree
test_Parse = testProperty "parse" prop where
    prop = forAll gen_ErrorName check
    check x = case parseErrorName x of
        Nothing -> False
        Just parsed -> formatErrorName parsed == x

test_ParseInvalid :: TestTree
test_ParseInvalid = testCase "parse-invalid" $ do
    -- empty
    Nothing @=? parseErrorName ""

    -- one element
    Nothing @=? parseErrorName "foo"

    -- element starting with a digit
    Nothing @=? parseErrorName "foo.0bar"

    -- trailing characters
    Nothing @=? parseErrorName "foo.bar!"

    -- at most 255 characters
    assertBool "valid parse failed"
        $ isJust (parseErrorName ("f." ++ replicate 252 'y'))
    assertBool "valid parse failed"
        $ isJust (parseErrorName ("f." ++ replicate 253 'y'))
    Nothing @=? parseErrorName ("f." ++ replicate 254 'y')

test_IsVariant :: TestTree
test_IsVariant = testCase "IsVariant" $
    assertVariant TypeString (errorName_ "foo.bar")

gen_ErrorName :: Gen String
gen_ErrorName = trim chunks where
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

instance Arbitrary ErrorName where
    arbitrary = fmap errorName_ gen_ErrorName
