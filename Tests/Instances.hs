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

module Tests.Instances (sized') where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import Test.QuickCheck
import DBus.Types

instance Arbitrary Char where
	coarbitrary = undefined
	arbitrary = choose ('!', '~') -- TODO: unicode?

instance Arbitrary Word8 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 8 - 1

instance Arbitrary Word16 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 16 - 1

instance Arbitrary Word32 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 32 - 1

instance Arbitrary Word64 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 64 - 1

instance Arbitrary Int16 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 16 - 1

instance Arbitrary Int32 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 32 - 1

instance Arbitrary Int64 where
	coarbitrary = undefined
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max') :: Gen Integer)
		max' = iexp 2 64 - 1

sized' :: Int -> Gen a -> Gen [a]
sized' atLeast g = sized $ \n -> do
	n' <- choose (atLeast, max atLeast n)
	replicateM n' g

clampedSize :: Arbitrary a => Int -> Gen String -> (String -> Maybe a) -> Gen a
clampedSize maxSize gen f = do
	s <- gen
	if length s > maxSize
		then sized (\n -> resize (n `div` 2) arbitrary)
		else return . fromJust . f $ s

instance Arbitrary ObjectPath where
	coarbitrary = undefined
	arbitrary = fmap (fromJust . mkObjectPath) path' where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
		path = fmap (intercalate "/" . ([] :)) genElements
		path' = frequency [(1, return "/"), (9, path)]
		genElements = sized' 1 (sized' 1 (elements c))

instance Arbitrary InterfaceName where
	coarbitrary = undefined
	arbitrary = clampedSize 255 genName mkInterfaceName where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
		c' = c ++ ['0'..'9']
		
		genName = fmap (intercalate ".") genElements
		genElements = sized' 2 genElement
		genElement = do
			x <- elements c
			xs <- sized' 0 (elements c')
			return (x:xs)
		

instance Arbitrary BusName where
	coarbitrary = undefined
	arbitrary = clampedSize 255 (oneof [unique, wellKnown]) mkBusName where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ "_-"
		c' = c ++ ['0'..'9']
		
		unique = do
			elems' <- sized' 2 $ elems c'
			return $ ':' : intercalate "." elems'
		
		wellKnown = do
			elems' <- sized' 2 $ elems c
			return $ intercalate "." elems'
		
		elems start = do
			x <- elements start
			xs <- sized' 0 (elements c')
			return (x:xs)

instance Arbitrary MemberName where
	coarbitrary = undefined
	arbitrary = clampedSize 255 genName mkMemberName where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
		c' = c ++ ['0'..'9']
		
		genName = do
			x <- elements c
			xs <- sized' 0 (elements c')
			return (x:xs)

instance Arbitrary ErrorName where
	coarbitrary = undefined
	arbitrary = fmap (fromJust . mkErrorName . strInterfaceName) arbitrary

instance Arbitrary Type where
	coarbitrary = undefined
	arbitrary = oneof [atomicType, containerType]

instance Arbitrary Signature where
	coarbitrary = undefined
	arbitrary = clampedSize 255 genSig mkSignature where
		genSig = fmap (concatMap typeString) arbitrary

atomicType = elements
	[ BooleanT
	, ByteT
	, UInt16T
	, UInt32T
	, UInt64T
	, Int16T
	, Int32T
	, Int64T
	, DoubleT
	, StringT
	, ObjectPathT
	, SignatureT
	]

containerType = do
	c <- choose (0,3) :: Gen Int
	case c of
		0 -> fmap ArrayT arbitrary
		1 -> do
			kt <- atomicType
			vt <- arbitrary
			return $ DictionaryT kt vt
		2 -> sized structType
		3 -> return VariantT

structType n | n >= 0 = fmap StructureT $ resize (n `div` 2) arbitrary

instance Arbitrary Atom where
	coarbitrary = undefined
	arbitrary = atomicType >>= \t -> case t of
		BooleanT    -> fmap toAtom (arbitrary :: Gen Bool)
		ByteT       -> fmap toAtom (arbitrary :: Gen Word8)
		UInt16T     -> fmap toAtom (arbitrary :: Gen Word16)
		UInt32T     -> fmap toAtom (arbitrary :: Gen Word32)
		UInt64T     -> fmap toAtom (arbitrary :: Gen Word64)
		Int16T      -> fmap toAtom (arbitrary :: Gen Int16)
		Int32T      -> fmap toAtom (arbitrary :: Gen Int32)
		Int64T      -> fmap toAtom (arbitrary :: Gen Int64)
		DoubleT     -> fmap toAtom (arbitrary :: Gen Double)
		StringT     -> fmap toAtom (arbitrary :: Gen String)
		ObjectPathT -> fmap toAtom (arbitrary :: Gen ObjectPath)
		SignatureT  -> fmap toAtom (arbitrary :: Gen Signature)

instance Arbitrary Array where
	coarbitrary = undefined
	arbitrary = do
		-- Only generate arrays of atomic values, as generating
		-- containers randomly almost never results in a valid
		-- array.
		x <- atomicType >>= \t -> case t of
			BooleanT    -> fmap toArray (arbitrary :: Gen [Bool])
			ByteT       -> fmap toArray (arbitrary :: Gen [Word8])
			UInt16T     -> fmap toArray (arbitrary :: Gen [Word16])
			UInt32T     -> fmap toArray (arbitrary :: Gen [Word32])
			UInt64T     -> fmap toArray (arbitrary :: Gen [Word64])
			Int16T      -> fmap toArray (arbitrary :: Gen [Int16])
			Int32T      -> fmap toArray (arbitrary :: Gen [Int32])
			Int64T      -> fmap toArray (arbitrary :: Gen [Int64])
			DoubleT     -> fmap toArray (arbitrary :: Gen [Double])
			StringT     -> fmap toArray (arbitrary :: Gen [String])
			ObjectPathT -> fmap toArray (arbitrary :: Gen [ObjectPath])
			SignatureT  -> fmap toArray (arbitrary :: Gen [Signature])
		
		maybe arbitrary return x

instance Arbitrary Dictionary where
	coarbitrary = undefined
	arbitrary = do
		-- Only generate dictionaries of atomic values, as generating
		-- containers randomly almost never results in a valid
		-- array.
		kt <- atomicType
		vt <- atomicType
		ks <- case kt of
			BooleanT    -> fmap (map toAtom) (arbitrary :: Gen [Bool])
			ByteT       -> fmap (map toAtom) (arbitrary :: Gen [Word8])
			UInt16T     -> fmap (map toAtom) (arbitrary :: Gen [Word16])
			UInt32T     -> fmap (map toAtom) (arbitrary :: Gen [Word32])
			UInt64T     -> fmap (map toAtom) (arbitrary :: Gen [Word64])
			Int16T      -> fmap (map toAtom) (arbitrary :: Gen [Int16])
			Int32T      -> fmap (map toAtom) (arbitrary :: Gen [Int32])
			Int64T      -> fmap (map toAtom) (arbitrary :: Gen [Int64])
			DoubleT     -> fmap (map toAtom) (arbitrary :: Gen [Double])
			StringT     -> fmap (map toAtom) (arbitrary :: Gen [String])
			ObjectPathT -> fmap (map toAtom) (arbitrary :: Gen [ObjectPath])
			SignatureT  -> fmap (map toAtom) (arbitrary :: Gen [Signature])
		vs <- case vt of
			BooleanT    -> fmap (map toVariant) (arbitrary :: Gen [Bool])
			ByteT       -> fmap (map toVariant) (arbitrary :: Gen [Word8])
			UInt16T     -> fmap (map toVariant) (arbitrary :: Gen [Word16])
			UInt32T     -> fmap (map toVariant) (arbitrary :: Gen [Word32])
			UInt64T     -> fmap (map toVariant) (arbitrary :: Gen [Word64])
			Int16T      -> fmap (map toVariant) (arbitrary :: Gen [Int16])
			Int32T      -> fmap (map toVariant) (arbitrary :: Gen [Int32])
			Int64T      -> fmap (map toVariant) (arbitrary :: Gen [Int64])
			DoubleT     -> fmap (map toVariant) (arbitrary :: Gen [Double])
			StringT     -> fmap (map toVariant) (arbitrary :: Gen [String])
			ObjectPathT -> fmap (map toVariant) (arbitrary :: Gen [ObjectPath])
			SignatureT  -> fmap (map toVariant) (arbitrary :: Gen [Signature])
		
		let kSig = fromJust . mkSignature . typeString $ kt
		let vSig = fromJust . mkSignature . typeString $ vt
		maybe arbitrary return (dictionaryFromItems kSig vSig (zip ks vs))

instance Arbitrary Structure where
	coarbitrary = undefined
	arbitrary = sized $ \n ->
		fmap Structure $ resize (n `div` 2) arbitrary

instance Arbitrary Variant where
	coarbitrary = undefined
	arbitrary = arbitrary >>= \t -> case t of
		BooleanT          -> fmap toVariant (arbitrary :: Gen Bool)
		ByteT             -> fmap toVariant (arbitrary :: Gen Word8)
		UInt16T           -> fmap toVariant (arbitrary :: Gen Word16)
		UInt32T           -> fmap toVariant (arbitrary :: Gen Word32)
		UInt64T           -> fmap toVariant (arbitrary :: Gen Word64)
		Int16T            -> fmap toVariant (arbitrary :: Gen Int16)
		Int32T            -> fmap toVariant (arbitrary :: Gen Int32)
		Int64T            -> fmap toVariant (arbitrary :: Gen Int64)
		DoubleT           -> fmap toVariant (arbitrary :: Gen Double)
		StringT           -> fmap toVariant (arbitrary :: Gen String)
		ObjectPathT       -> fmap toVariant (arbitrary :: Gen ObjectPath)
		SignatureT        -> fmap toVariant (arbitrary :: Gen Signature)
		ArrayT _          -> fmap toVariant (arbitrary :: Gen Array)
		DictionaryT _ _   -> fmap toVariant (arbitrary :: Gen Dictionary)
		StructureT _      -> fmap toVariant (arbitrary :: Gen Structure)
		VariantT          -> fmap toVariant (arbitrary :: Gen Variant)

instance Arbitrary Endianness where
	coarbitrary = undefined
	arbitrary = elements [LittleEndian, BigEndian]

instance Arbitrary Serial where
	coarbitrary = undefined
	arbitrary = fmap Serial arbitrary

iexp :: Integral a => a -> a -> a
iexp x y = floor $ fromIntegral x ** fromIntegral y
