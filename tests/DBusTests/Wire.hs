{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2012 John Millikin <john@john-millikin.com>
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
