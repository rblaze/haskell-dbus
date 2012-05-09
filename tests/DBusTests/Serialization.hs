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

import           Control.Applicative ((<*>))
import           Data.Text (Text)
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
import           DBusTests.Util (halfSized, smallListOf)

test_Serialization :: Suite
test_Serialization = suite "Serialization"
	[ test_MethodCall
	, test_MethodReturn
	, test_MethodError
	, test_Signal
	]

test_MethodCall :: Suite
test_MethodCall = property "MethodCall" prop where
	prop = forAll gen_MethodCall check
	check msg endianness serial = let
		Right bytes = marshalMessage endianness serial msg
		Right received = unmarshalMessage bytes
		in ReceivedMethodCall serial msg == received

test_MethodReturn :: Suite
test_MethodReturn = property "MethodReturn" prop where
	prop = forAll gen_MethodReturn check
	check msg endianness serial = let
		Right bytes = marshalMessage endianness serial msg
		Right received = unmarshalMessage bytes
		in ReceivedMethodReturn serial msg == received

test_MethodError :: Suite
test_MethodError = property "MethodError" prop where
	prop = forAll gen_MethodError check
	check msg endianness serial = let
		Right bytes = marshalMessage endianness serial msg
		Right received = unmarshalMessage bytes
		in ReceivedMethodError serial msg == received

test_Signal :: Suite
test_Signal = property "Signal" prop where
	prop = forAll gen_Signal check
	check msg endianness serial = let
		Right bytes = marshalMessage endianness serial msg
		Right received = unmarshalMessage bytes
		in ReceivedSignal serial msg == received

gen_Atom :: Gen DBus.Types.Atom
gen_Atom = oneof
	[ fmap DBus.Types.AtomWord8 arbitrary
	, fmap DBus.Types.AtomWord16 arbitrary
	, fmap DBus.Types.AtomWord32 arbitrary
	, fmap DBus.Types.AtomWord64 arbitrary
	, fmap DBus.Types.AtomInt16 arbitrary
	, fmap DBus.Types.AtomInt32 arbitrary
	, fmap DBus.Types.AtomInt64 arbitrary
	, fmap DBus.Types.AtomBool arbitrary
	, fmap DBus.Types.AtomDouble arbitrary
	, fmap DBus.Types.AtomText arbitrary
	, fmap DBus.Types.AtomObjectPath arbitrary
	, fmap DBus.Types.AtomSignature arbitrary
	]

gen_Value :: Gen DBus.Types.Value
gen_Value = oneof
	[ fmap DBus.Types.ValueAtom gen_Atom
	, fmap DBus.Types.ValueBytes arbitrary
	
	-- TODO: proper arbitrary ValueVector
	, elements
	  [ DBus.Types.toValue (Data.Vector.fromList ([] :: [Word8]))
	  , DBus.Types.toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word8]))
	  , DBus.Types.toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word16]))
	  , DBus.Types.toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word32]))
	  , DBus.Types.toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word64]))
	  , DBus.Types.toValue (Data.Vector.fromList (["foo", "bar", "baz"] :: [Text]))
	  ]
	
	-- TODO: proper arbitrary ValueMap
	, elements
	  [ DBus.Types.toValue (Data.Map.fromList [] :: Map Text Text)
	  , DBus.Types.toValue (Data.Map.fromList [("foo", "bar"), ("baz", "qux")] :: Map Text Text)
	  ]
	
	, fmap DBus.Types.ValueStructure (listOf1 (halfSized gen_Value))
	, fmap DBus.Types.ValueVariant gen_Variant
	]

gen_Variant :: Gen Variant
gen_Variant = do
	val <- gen_Value
	case signature [DBus.Types.valueType val] of
		Just _ -> return (DBus.Types.Variant val)
		Nothing -> halfSized gen_Variant

gen_MethodCall :: Gen MethodCall
gen_MethodCall = return MethodCall
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> smallListOf gen_Variant

gen_MethodReturn :: Gen MethodReturn
gen_MethodReturn = return MethodReturn
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> smallListOf gen_Variant

gen_MethodError :: Gen MethodError
gen_MethodError = return MethodError
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> smallListOf gen_Variant

gen_Signal :: Gen Signal
gen_Signal = return Signal
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> arbitrary
	<*> smallListOf gen_Variant

instance Arbitrary Endianness where
	arbitrary = elements [BigEndian, LittleEndian]

instance Arbitrary Serial where
	arbitrary = fmap DBus.Types.Serial arbitrary

instance Arbitrary Flag where
	arbitrary = elements [NoReplyExpected, NoAutoStart]
