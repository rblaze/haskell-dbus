module Tests.Instances () where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import Test.QuickCheck
import DBus.Types.ObjectPath
import DBus.Types.Signature

instance Arbitrary Word8 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 8) - 1

instance Arbitrary Word16 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 16) - 1

instance Arbitrary Word32 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 32) - 1

instance Arbitrary Word64 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 64) - 1

instance Arbitrary Int16 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 16) - 1

instance Arbitrary Int32 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 32) - 1

instance Arbitrary Int64 where
	arbitrary = gen where
		gen = fmap fromIntegral (choose (0, max) :: Gen Integer)
		max = (iexp 2 64) - 1

instance Arbitrary ObjectPath where
	arbitrary = do
		let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
		let genElement = sized $ \n -> do
			n' <- choose (1, max 1 n)
			replicateM n' (elements chars)
		let genElements = sized $ \n -> do
			n' <- choose (1, max 1 n)
			replicateM n' genElement
		
		useRoot <- frequency [(1, return True), (9, return False)]
		path <- if useRoot
			then return "/"
			else fmap (intercalate "/" . ([] :)) genElements
		return . fromJust . mkObjectPath $ path

instance Arbitrary Type where
	arbitrary = oneof [atomicType, containerType]

instance Arbitrary Signature where
	arbitrary = do
		ts <- arbitrary
		let str = concatMap typeString ts
		if length str <= 255
			then return . fromJust . mkSignature $ str
			else arbitrary

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
			return $ DictT kt vt
		2 -> sized structType
		3 -> return VariantT

structType n | n >= 0 = do
	ts <- resize (n `div` 2) arbitrary
	return . StructT $ ts

iexp :: Integral a => a -> a -> a
iexp x y = floor $ (fromIntegral x) ** (fromIntegral y)
