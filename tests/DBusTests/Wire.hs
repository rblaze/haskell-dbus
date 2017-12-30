{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2012 John Millikin <john@john-millikin.com>
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

module DBusTests.Wire (test_Wire) where

import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Char8 ()

import DBus

test_Wire :: TestTree
test_Wire = testGroup "Wire" $
    [ test_Unmarshal
    ]

test_Unmarshal :: TestTree
test_Unmarshal = testGroup "unmarshal"
    [ test_UnmarshalUnexpectedEof
    ]

test_UnmarshalUnexpectedEof :: TestTree
test_UnmarshalUnexpectedEof = testCase "unexpected-eof" $ do
    let unmarshaled = unmarshal "0"
    assertBool "invalid unmarshalled parse" (isLeft unmarshaled)

    let Left err = unmarshaled
    unmarshalErrorMessage err
        @=? "Unexpected end of input while parsing message header."
