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

module DBusTests.Serialization (test_Serialization) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding ((.&.), property)

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Int (Int16, Int32, Int64)
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Map (Map)
import qualified Data.Map
import qualified Data.Vector

import           DBus
import qualified DBus.Types

import           DBusTests.BusName ()
import           DBusTests.ErrorName ()
import           DBusTests.InterfaceName ()
import           DBusTests.MemberName ()
import           DBusTests.ObjectPath ()
import           DBusTests.Signature ()
import           DBusTests.Util (smallListOf)

test_Serialization :: Suite
test_Serialization = suite "Serialization"
	test_MethodCall
	test_MethodReturn
	test_MethodError
	test_Signal

test_MethodCall :: Test
test_MethodCall = property "MethodCall" prop where
	prop = forAll gen_MethodCall check
	check msg endianness serial = let
		Right bytes = marshal endianness serial msg
		Right received = unmarshal bytes
		in ReceivedMethodCall serial msg == received

test_MethodReturn :: Test
test_MethodReturn = property "MethodReturn" prop where
	prop = forAll gen_MethodReturn check
	check msg endianness serial = let
		Right bytes = marshal endianness serial msg
		Right received = unmarshal bytes
		in ReceivedMethodReturn serial msg == received

test_MethodError :: Test
test_MethodError = property "MethodError" prop where
	prop = forAll gen_MethodError check
	check msg endianness serial = let
		Right bytes = marshal endianness serial msg
		Right received = unmarshal bytes
		in ReceivedMethodError serial msg == received

test_Signal :: Test
test_Signal = property "Signal" prop where
	prop = forAll gen_Signal check
	check msg endianness serial = let
		Right bytes = marshal endianness serial msg
		Right received = unmarshal bytes
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
	, fmap toVariant (arbitrary :: Gen Text)
	, fmap toVariant (arbitrary :: Gen ObjectPath)
	, fmap toVariant (arbitrary :: Gen Signature)
	]

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
	arbitrary = fmap DBus.Types.Serial arbitrary
