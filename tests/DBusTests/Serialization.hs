{-# LANGUAGE OverloadedStrings #-}

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

module DBusTests.Serialization (test_Serialization) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32, Int64)
import Data.Map (Map)
import Data.Text (Text)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.C.Types (CInt)
import System.Posix.Types (Fd)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import qualified Data.Map
import qualified Data.Vector

import DBus
import qualified DBus.Internal.Types

import DBusTests.BusName ()
import DBusTests.ErrorName ()
import DBusTests.InterfaceName ()
import DBusTests.MemberName ()
import DBusTests.ObjectPath ()
import DBusTests.Signature ()
import DBusTests.Util (smallListOf)

test_Serialization :: TestTree
test_Serialization = testGroup "Serialization"
    [ test_MethodCall
    , test_MethodReturn
    , test_MethodError
    , test_Signal
    ]

test_MethodCall :: TestTree
test_MethodCall = testProperty "MethodCall" prop where
    prop = forAll gen_MethodCall check
    check msg endianness serial = let
        Right (bytes, fds) = marshal endianness serial msg
        Right received = unmarshal bytes fds
        in ReceivedMethodCall serial msg == received

test_MethodReturn :: TestTree
test_MethodReturn = testProperty "MethodReturn" prop where
    prop = forAll gen_MethodReturn check
    check msg endianness serial = let
        Right (bytes, fds) = marshal endianness serial msg
        Right received = unmarshal bytes fds
        in ReceivedMethodReturn serial msg == received

test_MethodError :: TestTree
test_MethodError = testProperty "MethodError" prop where
    prop = forAll gen_MethodError check
    check msg endianness serial = let
        Right (bytes, fds) = marshal endianness serial msg
        Right received = unmarshal bytes fds
        in ReceivedMethodError serial msg == received

test_Signal :: TestTree
test_Signal = testProperty "Signal" prop where
    prop = forAll gen_Signal check
    check msg endianness serial = let
        Right (bytes, fds) = marshal endianness serial msg
        Right received = unmarshal bytes fds
        in ReceivedSignal serial msg == received

gen_Atom :: Gen Variant
gen_Atom = oneof
    [ fmap toVariant (arbitrary :: Gen Word8)
    , fmap toVariant (arbitrary :: Gen Word16)
    , fmap toVariant (arbitrary :: Gen Word32)
    , fmap toVariant (arbitrary :: Gen Word64)
    , fmap toVariant (arbitrary :: Gen Int16)
    , fmap toVariant (arbitrary :: Gen Int32)
    , fmap toVariant (arbitrary :: Gen Int64)
    , fmap toVariant (arbitrary :: Gen Bool)
    , fmap toVariant (arbitrary :: Gen Double)
    , fmap toVariant gen_UnixFd
    , fmap toVariant (arbitrary :: Gen Text)
    , fmap toVariant (arbitrary :: Gen ObjectPath)
    , fmap toVariant (arbitrary :: Gen Signature)
    ]

gen_UnixFd :: Gen Fd
gen_UnixFd = do
    let maxWord32 = toInteger (maxBound :: Word32)
    let maxCInt = toInteger (maxBound :: CInt)
    x <- choose (0, toInteger (min maxWord32 maxCInt))
    return (fromInteger x)

gen_Variant :: Gen Variant
gen_Variant = oneof
    [ gen_Atom
    , fmap toVariant (arbitrary :: Gen ByteString)

    -- TODO: proper arbitrary vectors
    , elements
      [ toVariant (Data.Vector.fromList ([] :: [Word8]))
      , toVariant (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word8]))
      , toVariant (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word16]))
      , toVariant (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word32]))
      , toVariant (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word64]))
      , toVariant (Data.Vector.fromList (["foo", "bar", "baz"] :: [Text]))
      ]

    -- TODO: proper arbitrary maps
    , elements
      [ toVariant (Data.Map.fromList [] :: Map Text Text)
      , toVariant (Data.Map.fromList [("foo", "bar"), ("baz", "qux")] :: Map Text Text)
      ]

    -- TODO: proper arbitrary structures
    , elements
      [ toVariant (True, "foo" :: Text, ["bar" :: Text])
      , toVariant (1 :: Word8, 1 :: Word16, 1 :: Word32, 1 :: Word64)
      ]
    , fmap toVariant gen_Variant
    ]

gen_MethodCall :: Gen MethodCall
gen_MethodCall = do
    path <- arbitrary
    iface <- arbitrary
    member <- arbitrary
    sender <- arbitrary
    dest <- arbitrary

    flagReplyExpected <- arbitrary
    flagAutoStart <- arbitrary

    body <- smallListOf gen_Variant
    return (methodCall path "com.example.ignored" member)
        { methodCallInterface = iface
        , methodCallSender = sender
        , methodCallDestination = dest
        , methodCallReplyExpected = flagReplyExpected
        , methodCallAutoStart = flagAutoStart
        , methodCallBody = body
        }

gen_MethodReturn :: Gen MethodReturn
gen_MethodReturn = do
    serial <- arbitrary
    sender <- arbitrary
    dest <- arbitrary
    body <- smallListOf gen_Variant
    return (methodReturn serial)
        { methodReturnSender = sender
        , methodReturnDestination = dest
        , methodReturnBody = body
        }

gen_MethodError :: Gen MethodError
gen_MethodError = do
    serial <- arbitrary
    name <- arbitrary
    sender <- arbitrary
    dest <- arbitrary
    body <- smallListOf gen_Variant
    return (methodError serial name)
        { methodErrorSender = sender
        , methodErrorDestination = dest
        , methodErrorBody = body
        }

gen_Signal :: Gen Signal
gen_Signal = do
    path <- arbitrary
    iface <- arbitrary
    member <- arbitrary
    sender <- arbitrary
    dest <- arbitrary
    body <- smallListOf gen_Variant
    return (signal path iface member)
        { signalSender = sender
        , signalDestination = dest
        , signalBody = body
        }

instance Arbitrary Endianness where
    arbitrary = elements [BigEndian, LittleEndian]

instance Arbitrary Serial where
    arbitrary = fmap DBus.Internal.Types.Serial arbitrary
