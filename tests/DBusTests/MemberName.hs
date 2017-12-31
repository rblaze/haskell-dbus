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

module DBusTests.MemberName (test_MemberName) where

import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DBus

import DBusTests.Util

test_MemberName :: TestTree
test_MemberName = testGroup "MemberName"
    [ test_Parse
    , test_ParseInvalid
    , test_IsVariant
    ]

test_Parse :: TestTree
test_Parse = testProperty "parse" prop where
    prop = forAll gen_MemberName check
    check x = case parseMemberName x of
        Nothing -> False
        Just parsed -> formatMemberName parsed == x

test_ParseInvalid :: TestTree
test_ParseInvalid = testCase "parse-invalid" $ do
    -- empty
    Nothing @=? parseMemberName ""

    -- starts with a digit
    Nothing @=? parseMemberName "@foo"

    -- trailing chars
    Nothing @=? parseMemberName "foo!"

    -- at most 255 characters
    assertBool "valid parse failed"
        $ isJust (parseMemberName (replicate 254 'y'))
    assertBool "valid parse failed"
        $ isJust (parseMemberName (replicate 255 'y'))
    Nothing @=? parseMemberName (replicate 256 'y')

test_IsVariant :: TestTree
test_IsVariant = testCase "IsVariant" $
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
