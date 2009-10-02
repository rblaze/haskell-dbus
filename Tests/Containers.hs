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

import Control.Arrow ((&&&))
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
	, forAllAtomic prop_AtomDefaultSignature
	, forAllVariable prop_DefaultSignature
	, variantSignatureProperties
	
	, [ property prop_VariantSignatureLength
	  , property prop_VariantType
	  , property prop_AtomType
	  ]
	
	
	, [ property prop_ArrayHomogeneous
	  , property prop_DictHomogeneous0
	  , property prop_DictHomogeneous1
	  ]
	, props_EmptyArraySignature
	, props_EmptyDictionarySignature
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
	atomToVariant (toAtom x) == toVariant x

prop_VariantWrapping gen = forAll gen $ \x ->
	fromVariant (toVariant x) == Just x

prop_AtomSignature gen = forAll gen $ \x ->
	atomSignature (toAtom x) == variantSignature (toVariant x)

prop_AtomDefaultSignature gen = forAll gen $ \x ->
	variantSignature (toVariant x) == defaultSignature x

prop_DefaultSignature gen = forAll gen $ \x -> let
	sig = strSignature . defaultSignature $ y
	y = head [y, x]
	in not . null $ sig


-- Test that signatures are set properly
sameSig :: Variable a => a -> Type -> Bool
sameSig x t = t == (variantType . toVariant) x

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

-- variantType is a shortcut for getting the type in a variant's
-- signature.
prop_VariantType x = variantType x == t where
	t = head . signatureTypes . variantSignature $ x

-- Ditto atomType
prop_AtomType x = atomType x == t where
	t = head . signatureTypes . atomSignature $ x

-- All items in an array have the same signature. If items do not have
-- the same signature, the array can't be constructed
prop_ArrayHomogeneous vs = isJust array == homogeneousTypes where
	array = arrayFromItems sig vs
	homogeneousTypes = if length vs > 0
		then all (== firstType) types
		else True
	types = map variantType vs
	firstType = head types
	sig = if length vs > 0
		then variantSignature (head vs)
		else mkSignature' "y"

-- A dictionary must have homogeneous key and value types
prop_DictHomogeneous0 = homogeneousDict

prop_DictHomogeneous1 ks = forAll (vector (length ks)) $ \vs -> let
	dict = dictionaryFromItems kSig vSig (zip ks vs)
	(kSig, vSig) = if null ks
		then (id &&& id) . mkSignature' $ "y"
		else (atomSignature (head ks), variantSignature (head vs))
	in isJust dict ==> homogeneousDict (fromJust dict)

homogeneousDict :: Dictionary -> Property
homogeneousDict x = length items > 0 ==> homogeneous where
	items = dictionaryItems x
	keys = [k | (k,_) <- items]
	values = [v | (_,v) <- items]
	
	kTypes = map atomType keys
	kFirst = head kTypes
	
	vTypes = map variantType values
	vFirst = head vTypes
	
	homogeneous = all (== kFirst) kTypes && all (== vFirst) vTypes

props_EmptyArraySignature =
	[ check ([] :: [Word8]) "ay"
	, check ([] :: [Bool]) "ab"
	, check ([] :: [Array]) "aay"
	, check ([] :: [Dictionary]) "aa{yy}"
	, check ([] :: [Structure]) "a()"
	, check ([] :: [Variant]) "av"
	]
	where check xs sig = forAll (return sig) $ checkEmptyArray xs

checkEmptyArray xs sig = arraySignature a == sig' where
	sig' = mkSignature' sig
	a = fromJust . toArray $ xs

props_EmptyDictionarySignature =
	[ check ([] :: [(Word8, Word8)]) "a{yy}"
	, check ([] :: [(Bool, Word8)]) "a{by}"
	, check ([] :: [(Bool, Array)]) "a{bay}"
	, check ([] :: [(Bool, Dictionary)]) "a{ba{yy}}"
	, check ([] :: [(Bool, Structure)]) "a{b()}"
	, check ([] :: [(Bool, Variant)]) "a{bv}"
	]
	where check xs sig = forAll (return sig) $ checkEmptyDict xs

checkEmptyDict xs sig = dictionarySignature a == sig' where
	sig' = mkSignature' sig
	a = fromJust . toDictionary $ xs

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
