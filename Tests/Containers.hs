{-
  Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE RankNTypes #-}
module Tests.Containers (containerProperties) where

import Data.Maybe (isJust, fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import Test.QuickCheck

import Tests.Instances ()

import DBus.Types

containerProperties = concat
	[ forAllAtomic prop_AtomEquality
	, forAllVariable prop_VariantEquality
	
	, forAllAtomic prop_AtomWrapping0
	, forAllAtomic prop_AtomWrapping1
	, forAllVariable prop_VariantWrapping
	
	, forAllAtomic prop_AtomSignature
	, variantSignatureProperties
	, [ property prop_VariantSignatureLength ]
	, [ property prop_ArrayHomogeneous
	  , property prop_DictHomogeneous0
	  , property prop_DictHomogeneous1
	  ]
	]

-- Basic equality

prop_AtomEquality gen = forAll gen $ \x y ->
	(x == y) == (toAtom x == toAtom y)

prop_VariantEquality gen = forAll gen $ \x y ->
	(x == y) == (toVariant x == toVariant y)

-- Conversion to/from/between Atom and Variant

prop_AtomWrapping0 gen = forAll gen $ \x ->
	fromAtom (toAtom x) == Just x

prop_AtomWrapping1 gen = forAll gen $ \x ->
	toVariant (toAtom x) == toVariant x

prop_VariantWrapping gen = forAll gen $ \x ->
	fromVariant (toVariant x) == Just x

prop_AtomSignature gen = forAll gen $ \x ->
	atomSignature (toAtom x) == variantSignature (toVariant x)

-- Test that signatures are set properly
sameSig :: Variable a => a -> Type -> Bool
sameSig x t = [t] == t' where
	t' = signatureTypes . variantSignature . toVariant $ x

sameSig' :: Variable a => (a -> Signature) -> a -> Bool
sameSig' f x = f x == (variantSignature . toVariant) x

variantSignatureProperties =
	[ property $ \x -> sameSig (x :: Bool) BooleanT
	, property $ \x -> sameSig (x :: Word8) ByteT
	, property $ \x -> sameSig (x :: Word16) UInt16T
	, property $ \x -> sameSig (x :: Word32) UInt32T
	, property $ \x -> sameSig (x :: Word64) UInt64T
	, property $ \x -> sameSig (x :: Int16) Int16T
	, property $ \x -> sameSig (x :: Int32) Int32T
	, property $ \x -> sameSig (x :: Int64) Int64T
	, property $ \x -> sameSig (x :: Double) DoubleT
	, property $ \x -> sameSig (x :: String) StringT
	, property $ \x -> sameSig (x :: ObjectPath) ObjectPathT
	, property $ \x -> sameSig (x :: Signature) SignatureT
	, property $ sameSig' arraySignature
	, property $ sameSig' dictionarySignature
	, property $ sameSig' structureSignature
	, property $ \x -> sameSig (x :: Variant) VariantT
	]

-- All variant signatures have one entry
prop_VariantSignatureLength x = length sig == 1 where
	sig = signatureTypes . variantSignature $ x

-- All items in an array have the same signature. If items do not have
-- the same signature, the array can't be constructed
prop_ArrayHomogeneous vs = isJust array == homogeneousTypes where
	array = arrayFromItems vs
	homogeneousTypes = if length vs > 0
		then all (== firstType) types
		else True
	types = map (signatureTypes . variantSignature) vs
	firstType = head types

-- A dictionary must have homogeneous key and value types
prop_DictHomogeneous0 = homogeneousDict

prop_DictHomogeneous1 ks = forAll (vector (length ks)) $ \vs -> let
	dict = dictionaryFromItems (zip ks vs)
	in isJust dict ==> homogeneousDict (fromJust dict)

homogeneousDict :: Dictionary -> Property
homogeneousDict x = length items > 0 ==> homogeneous where
	items = dictionaryItems x
	keys = [k | (k,_) <- items]
	values = [v | (_,v) <- items]
	
	kTypes = map (signatureTypes . atomSignature) keys
	kFirst = head kTypes
	
	vTypes = map (signatureTypes . variantSignature) values
	vFirst = head vTypes
	
	homogeneous = all (== kFirst) kTypes && all (== vFirst) vTypes

-- TODO Conversion to/from array and dictionary

-- TODO: container signatures

-- Helper functions

forAllAtomic :: (forall a. (Show a, Arbitrary a, Atomic a, Eq a) =>
                (Gen a -> Property)) -> [Property]
forAllAtomic t =
	[ t (arbitrary :: Gen Bool)
	, t (arbitrary :: Gen Word8)
	, t (arbitrary :: Gen Word16)
	, t (arbitrary :: Gen Word32)
	, t (arbitrary :: Gen Word64)
	, t (arbitrary :: Gen Int16)
	, t (arbitrary :: Gen Int32)
	, t (arbitrary :: Gen Int64)
	, t (arbitrary :: Gen Double)
	, t (arbitrary :: Gen String)
	, t (arbitrary :: Gen ObjectPath)
	, t (arbitrary :: Gen Signature)
	]

forAllVariable :: (forall a. (Show a, Arbitrary a, Variable a, Eq a) =>
                  (Gen a -> Property)) -> [Property]
forAllVariable t = forAllAtomic t ++
	[ t (arbitrary :: Gen Array)
	, t (arbitrary :: Gen Dictionary)
	, t (arbitrary :: Gen Structure)
	, t (arbitrary :: Gen Variant)
	]
