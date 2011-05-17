{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
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

module Main (tests, main) where

import           Test.QuickCheck
import qualified Test.Framework as F
import           Test.Framework.Providers.QuickCheck2 (testProperty)

import           Control.Arrow ((&&&))
import           Control.Monad (replicateM)
import qualified Data.Binary.Get as G
import           Data.Char (isPrint)
import           Data.String
import           Data.List (intercalate, isInfixOf)
import           Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Int (Int16, Int32, Int64)

import           DBus.Address
import           DBus.Message.Internal
import           DBus.Types
import           DBus.Wire.Internal
import           DBus.Wire.Marshal
import           DBus.Wire.Unmarshal
import qualified DBus.Introspection as I

tests :: [F.Test]
tests = [ test_Addresses
	, test_Introspection
	, test_StringVariants
	, test_Types
	, test_Wire
	]

main :: IO ()
main = F.defaultMain tests

test_Addresses :: F.Test
test_Addresses = F.testGroup "Addresses"
	[ testProperty "Address identity"
		$ \x -> mkAddresses (strAddress x) == Just [x]
	, testProperty "Multiple addresses"
		$ \x y -> let
		joined = TL.concat [strAddress x, ";", strAddress y]
		in mkAddresses joined == Just [x, y]
	, testProperty "Ignore trailing semicolon"
		$ \x -> mkAddresses (TL.append (strAddress x) ";") == Just [x]
	, testProperty "Ignore trailing comma"
		$ \x -> let
		hasParams = not . Map.null . addressParameters $ x
		parsed = mkAddresses (TL.append (strAddress x) ",")
		in hasParams ==> parsed == Just [x]
	, F.testGroup "Valid addresses"
		[ test "colon" $ isJust . mkAddresses $ ":"
		, test "just scheme" $ isJust . mkAddresses $ "a:"
		, test "param" $ isJust . mkAddresses $ "a:b=c"
		, test "trailing semicolon" $ isJust . mkAddresses $ "a:;"
		, test "two schemes" $ isJust . mkAddresses $ "a:;b:"
		, test "trailing comma" $ isJust . mkAddresses $ "a:b=c,"
		]
	, F.testGroup "Invalid addresses"
		[ test "empty" $ isNothing . mkAddresses $ ""
		, test "no colon" $ isNothing . mkAddresses $ "a"
		, test "no equals" $ isNothing . mkAddresses $ "a:b"
		, test "no param" $ isNothing . mkAddresses $ "a:b="
		, test "no param" $ isNothing . mkAddresses $ "a:,"
		]
	]

test_Introspection :: F.Test
test_Introspection = F.testGroup "Introspection"
	[ testProperty "Generate -> Parse"
		$ \x@(I.Object path _ _) -> let
		xml = I.toXML x
		Just xml' = xml
		parsed = I.fromXML path xml'
		in isJust xml ==> I.fromXML path xml' == Just x
	]

test_StringVariants :: F.Test
test_StringVariants = F.testGroup "String"
	[ testProperty "String -> strict Text"
		$ funEq (fromVariant . toVariant) (Just . T.pack)
	, testProperty "String <- strict Text"
		$ funEq (fromVariant . toVariant) (Just . T.unpack)
	, testProperty "String -> lazy Text"
		$ funEq (fromVariant . toVariant) (Just . TL.pack)
	, testProperty "String <- lazy Text"
		$ funEq (fromVariant . toVariant) (Just . TL.unpack)
	, testProperty "Strict Text -> lazy Text"
		$ funEq (fromVariant . toVariant) (Just . TL.pack . T.unpack)
	, testProperty "Strict Text <- lazy Text"
		$ funEq (fromVariant . toVariant) (Just . T.pack . TL.unpack)
	]

test_Wire :: F.Test
test_Wire = F.testGroup "Wire format"
	[ testProperty "Marshal -> Ummarshal" prop_Unmarshal
	, F.testGroup "Messages"
		[ testProperty "Method calls" prop_WireMethodCall
		, testProperty "Method returns" prop_WireMethodReturn
		, testProperty "Errors" prop_WireError
		, testProperty "Signals" prop_WireSignal
		]
	]

prop_WireMethodCall e serial msg = prop_MarshalMessage e serial msg
	$ ReceivedMethodCall serial Nothing msg

prop_WireMethodReturn e serial msg = prop_MarshalMessage e serial msg
	$ ReceivedMethodReturn serial Nothing msg

prop_WireError e serial msg = prop_MarshalMessage e serial msg
	$ ReceivedError serial Nothing msg

prop_WireSignal e serial msg = prop_MarshalMessage e serial msg
	$ ReceivedSignal serial Nothing msg


prop_ArrayHomogeneous vs = isJust array == homogeneousTypes where
	array = arrayFromItems firstType vs
	homogeneousTypes = all (== firstType) types
	types = map variantType vs
	firstType = if null types
		then DBusByte
		else head types


prop_DictionaryHomogeneous x = all correctType pairs where
	pairs = dictionaryItems x
	kType = dictionaryKeyType x
	vType = dictionaryValueType x
	correctType (k, v) = variantType k == kType &&
	                     variantType v == vType

prop_Unmarshal :: Endianness -> Variant -> Property
prop_Unmarshal e x = valid ==> unmarshaled == Right [x] where
	sig = mkSignature . typeCode . variantType $ x
	Just sig' = sig
	
	bytes = runMarshal (marshal x) e
	Right bytes' = bytes
	
	valid = isJust sig && isRight bytes
	unmarshaled = runUnmarshal (unmarshal sig') e bytes'

prop_MarshalMessage e serial msg expected = valid ==> correct where
	bytes = marshalMessage e serial msg
	Right bytes' = bytes
	
	getBytes = G.getLazyByteString . fromIntegral
	unmarshaled = G.runGet (unmarshalMessage getBytes) bytes'
	
	valid = isRight bytes
	correct = unmarshaled == Right expected

test_Types :: F.Test
test_Types = F.testGroup "types"
	[ testProperty "Signature identity"
		$ funEq (mkSignature . strSignature) Just
	, testProperty "ObjectPath identity"
		$ funEq (mkObjectPath . strObjectPath) Just
	, testProperty "Array identity"
		$ \x -> Just x == arrayFromItems (arrayType x) (arrayItems x)
	, testProperty "Array homogeneity" prop_ArrayHomogeneous
	, testProperty "Dictionary identity"
		$ \x -> Just x == dictionaryFromItems
			(dictionaryKeyType x)
			(dictionaryValueType x)
			(dictionaryItems x)
	, testProperty "Dictionary homogeneity" prop_DictionaryHomogeneous
	, testProperty "Dictionary must have atomic keys" 
		$ \vt -> forAll containerType $ \kt ->
			isNothing (dictionaryFromItems kt vt [])
	, testProperty "Dictionary <-> Array conversion"
		$ funEq (arrayToDictionary . dictionaryToArray) Just
	, testProperty "BusName identity"
		$ funEq (mkBusName . strBusName) Just
	, testProperty "InterfaceName identity"
		$ funEq (mkInterfaceName . strInterfaceName) Just
	, testProperty "ErrorName identity"
		$ funEq (mkErrorName . strErrorName) Just
	, testProperty "MemberName identity"
		$ funEq (mkMemberName . strMemberName) Just
	]

-- Arbitrary instances {{{

instance Arbitrary T.Text where
	arbitrary = fmap T.pack arbitrary

instance Arbitrary TL.Text where
	arbitrary = fmap TL.pack arbitrary

instance Arbitrary Endianness where
	arbitrary = elements [LittleEndian, BigEndian]

instance Arbitrary Address where
	arbitrary = genAddress where
		optional = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_/\\*."
		methodChars = filter (flip notElem ":;") ['!'..'~']
		keyChars = filter (flip notElem "=;,") ['!'..'~']
		
		genMethod = atLeast 0 $ elements methodChars
		genParam = do
			key <- genKey
			value <- genValue
			return . concat $ [key, "=", value]
		
		genKey = atLeast 1 $ elements keyChars
		genValue = oneof [encodedValue, plainValue]
		genHex = elements $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
		encodedValue = do
			x1 <- genHex
			x2 <- genHex
			return ['%', x1, x2]
		plainValue = atLeast 1 $ elements optional
		
		genParams = do
			params <- atLeast 0 genParam
			let params' = intercalate "," params
			extraComma <- if null params
				then return ""
				else elements ["", ","]
			return $ concat [params', extraComma]
		
		genAddress = do
			m <- genMethod
			params <- genParams
			extraSemicolon <- elements ["", ";"]
			let addrStr = concat [m, ":", params, extraSemicolon]
			let Just [addr] = mkAddresses $ TL.pack addrStr
			return addr

subObject :: ObjectPath -> Gen I.Object
subObject parentPath = sized $ \n -> resize (min n 4) $ do
	let nonRoot = do
		x <- arbitrary
		case strObjectPath x of
			"/" -> nonRoot
			x'  -> return x'
	
	thisPath <- nonRoot
	let path' = case strObjectPath parentPath of
		"/" -> thisPath
		x   -> TL.append x thisPath
	let path = mkObjectPath_ path'
	ifaces <- arbitrary
	children <- halfSized . listOf . subObject $ path
	return $ I.Object path ifaces children

instance Arbitrary I.Object where
	arbitrary = arbitrary >>= subObject

instance Arbitrary I.Interface where
	arbitrary = do
		name <- arbitrary
		methods <- arbitrary
		signals <- arbitrary
		properties <- arbitrary
		return $ I.Interface name methods signals properties

instance Arbitrary I.Method where
	arbitrary = do
		name <- arbitrary
		inParams <- arbitrary
		outParams <- arbitrary
		return $ I.Method name inParams outParams

instance Arbitrary I.Signal where
	arbitrary = do
		name <- arbitrary
		params <- arbitrary
		return $ I.Signal name params

singleType :: Gen Signature
singleType = do
	t <- arbitrary
	case mkSignature $ typeCode t of
		Just x -> return x
		Nothing -> singleType

instance Arbitrary I.Parameter where
	arbitrary = do
		name <- listOf $ arbitrary `suchThat` isPrint
		sig <- singleType
		return $ I.Parameter (TL.pack name) sig

instance Arbitrary I.Property where
	arbitrary = do
		name <- listOf $ arbitrary `suchThat` isPrint
		sig <- singleType
		access <- elements
			[[], [I.Read], [I.Write],
			 [I.Read, I.Write]]
		return $ I.Property (TL.pack name) sig access

instance Arbitrary Flag where
	arbitrary = elements [NoReplyExpected, NoAutoStart]

instance Arbitrary Serial where
	arbitrary = fmap Serial arbitrary

instance Arbitrary MethodCall where
	arbitrary = do
		path   <- arbitrary
		member <- arbitrary
		iface  <- arbitrary
		dest   <- arbitrary
		flags  <- fmap Set.fromList arbitrary
		Structure body <- arbitrary
		return $ MethodCall path member iface dest flags body

instance Arbitrary MethodReturn where
	arbitrary = do
		serial <- arbitrary
		dest   <- arbitrary
		Structure body <- arbitrary
		return $ MethodReturn serial dest body

instance Arbitrary Error where
	arbitrary = do
		name   <- arbitrary
		serial <- arbitrary
		dest   <- arbitrary
		Structure body <- arbitrary
		return $ Error name serial dest body

instance Arbitrary Signal where
	arbitrary = do
		path   <- arbitrary
		member <- arbitrary
		iface  <- arbitrary
		dest   <- arbitrary
		Structure body <- arbitrary
		return $ Signal path member iface dest body

instance Arbitrary Type where
	arbitrary = oneof [atomicType, containerType]

atomicType :: Gen Type
atomicType = elements
	[ DBusBoolean
	, DBusByte
	, DBusWord16
	, DBusWord32
	, DBusWord64
	, DBusInt16
	, DBusInt32
	, DBusInt64
	, DBusDouble
	, DBusString
	, DBusObjectPath
	, DBusSignature
	]

containerType :: Gen Type
containerType = do
	c <- choose (0,3) :: Gen Int
	case c of
		0 -> fmap DBusArray arbitrary
		1 -> do
			kt <- atomicType
			vt <- arbitrary
			return $ DBusDictionary kt vt
		2 -> fmap DBusStructure $ halfSized arbitrary
		3 -> return DBusVariant

instance Arbitrary Variant where
	arbitrary = arbitrary >>= genVariant

genVariant :: Type -> Gen Variant
genVariant t = case t of
	DBusBoolean          -> fmap toVariant (arbitrary :: Gen Bool)
	DBusByte             -> fmap toVariant (arbitrary :: Gen Word8)
	DBusWord16           -> fmap toVariant (arbitrary :: Gen Word16)
	DBusWord32           -> fmap toVariant (arbitrary :: Gen Word32)
	DBusWord64           -> fmap toVariant (arbitrary :: Gen Word64)
	DBusInt16            -> fmap toVariant (arbitrary :: Gen Int16)
	DBusInt32            -> fmap toVariant (arbitrary :: Gen Int32)
	DBusInt64            -> fmap toVariant (arbitrary :: Gen Int64)
	DBusDouble           -> fmap toVariant (arbitrary :: Gen Double)
	DBusString           -> fmap toVariant (arbitrary :: Gen String)
	DBusObjectPath       -> fmap toVariant (arbitrary :: Gen ObjectPath)
	DBusSignature        -> fmap toVariant (arbitrary :: Gen Signature)
	(DBusArray _)        -> fmap toVariant (arbitrary :: Gen Array)
	(DBusDictionary _ _) -> fmap toVariant (arbitrary :: Gen Dictionary)
	(DBusStructure _)    -> fmap toVariant (arbitrary :: Gen Structure)
	DBusVariant          -> fmap toVariant (arbitrary :: Gen Variant)

instance Arbitrary Signature where
	arbitrary = sizedText 255 $ fmap (TL.concat . map typeCode) arbitrary

instance Arbitrary ObjectPath where
	arbitrary = fmap (mkObjectPath_ . TL.pack) path' where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
		path = fmap (intercalate "/" . ([] :)) genElements
		path' = frequency [(1, return "/"), (9, path)]
		genElements = atLeast 1 (atLeast 1 (elements c))

instance Arbitrary Array where
	arbitrary = do
		t <- atomicType
		xs <- listOf $ genVariant t
		return . fromJust $ arrayFromItems t xs

instance Arbitrary Dictionary where
	arbitrary = do
		kt <- atomicType
		vt <- atomicType
		ks <- listOf $ genVariant kt
		vs <- vectorOf (length ks) $ genVariant vt
		
		return . fromJust $ dictionaryFromItems kt vt $ zip ks vs

instance Arbitrary Structure where
	arbitrary = fmap Structure $ halfSized arbitrary

instance Arbitrary BusName where
	arbitrary = sizedText 255 (oneof [unique, wellKnown]) where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ "_-"
		c' = c ++ ['0'..'9']
		
		unique = do
			elems' <- atLeast 2 $ elems c'
			return . TL.pack $ ':' : intercalate "." elems'
		
		wellKnown = do
			elems' <- atLeast 2 $ elems c
			return . TL.pack $ intercalate "." elems'
		
		elems start = do
			x <- elements start
			xs <- atLeast 0 (elements c')
			return (x:xs)

instance Arbitrary InterfaceName where
	arbitrary = sizedText 255 genName where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
		c' = c ++ ['0'..'9']
		
		genName = fmap (TL.pack . intercalate ".") genElements
		genElements = atLeast 2 genElement
		genElement = do
			x <- elements c
			xs <- atLeast 0 (elements c')
			return (x:xs)

instance Arbitrary ErrorName where
	arbitrary = fmap (mkErrorName_ . strInterfaceName) arbitrary

instance Arbitrary MemberName where
	arbitrary = sizedText 255 genName where
		c = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
		c' = c ++ ['0'..'9']
		
		genName = do
			x <- elements c
			xs <- atLeast 0 (elements c')
			return . TL.pack $ (x:xs)

halfSized :: Gen a -> Gen a
halfSized gen = sized $ \n -> if n > 0 then
	resize (n `div` 2) gen
	else gen

-- }}}

-- misc util

funEq :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
funEq f g x = f x == g x

sizedText :: (IsString a, Arbitrary a) => Integer -> Gen TL.Text -> Gen a
sizedText maxSize gen = step where
	step = do
		s <- gen
		if toInteger (TL.length s) > maxSize
			then halfSized step
			else return . fromString . TL.unpack $ s

atLeast :: Int -> Gen a -> Gen [a]
atLeast minSize g = sized $ \n -> do
	count <- choose (minSize, max minSize n)
	replicateM count g

isRight :: Either a b -> Bool
isRight = either (const False) (const True)

test :: Testable a => F.TestName -> a -> F.Test
test name prop = F.plusTestOptions options (testProperty name prop) where
	options = F.TestOptions Nothing (Just 1) Nothing Nothing
