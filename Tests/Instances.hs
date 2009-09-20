module Tests.Instances () where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import Test.QuickCheck
import DBus.Types.ObjectPath
import DBus.Types.Signature
import DBus.Types.Atom
import DBus.Types.Containers

instance Arbitrary Char where
	coarbitrary = undefined
	arbitrary = choose ('!', '~') -- TODO: unicode?

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
		ks <- atomicType >>= \t -> case t of
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
		vs <- atomicType >>= \t -> case t of
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
		
		maybe arbitrary return (dictionaryFromItems (zip ks vs))

instance Arbitrary Structure where
	coarbitrary = undefined
	arbitrary = sized $ \n ->
		fmap Structure $ resize (n `div` 2) arbitrary

instance Arbitrary Variant where
	coarbitrary = undefined
	arbitrary = arbitrary >>= \t -> case t of
		BooleanT    -> fmap toVariant (arbitrary :: Gen Bool)
		ByteT       -> fmap toVariant (arbitrary :: Gen Word8)
		UInt16T     -> fmap toVariant (arbitrary :: Gen Word16)
		UInt32T     -> fmap toVariant (arbitrary :: Gen Word32)
		UInt64T     -> fmap toVariant (arbitrary :: Gen Word64)
		Int16T      -> fmap toVariant (arbitrary :: Gen Int16)
		Int32T      -> fmap toVariant (arbitrary :: Gen Int32)
		Int64T      -> fmap toVariant (arbitrary :: Gen Int64)
		DoubleT     -> fmap toVariant (arbitrary :: Gen Double)
		StringT     -> fmap toVariant (arbitrary :: Gen String)
		ObjectPathT -> fmap toVariant (arbitrary :: Gen ObjectPath)
		SignatureT  -> fmap toVariant (arbitrary :: Gen Signature)
		ArrayT _    -> fmap toVariant (arbitrary :: Gen Array)
		DictT _ _   -> fmap toVariant (arbitrary :: Gen Dictionary)
		StructT _   -> fmap toVariant (arbitrary :: Gen Structure)
		VariantT    -> fmap toVariant (arbitrary :: Gen Variant)

iexp :: Integral a => a -> a -> a
iexp x y = floor $ (fromIntegral x) ** (fromIntegral y)
