{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010-2011 John Millikin <jmillikin@gmail.com>
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

import           Test.Framework (Test, testGroup)
import qualified Test.Framework
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertFailure)
import qualified Test.HUnit
import           Test.QuickCheck

import qualified Control.Exception
import           Control.Monad (liftM, liftM2)
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Int (Int16, Int32, Int64)
import qualified Data.Map
import           Data.Maybe (isJust, isNothing, fromJust)
import           Data.String (IsString, fromString)
import qualified System.Posix.Env

import           DBus.Address
import           DBus.Client ()
import           DBus.Connection ()
import           DBus.MatchRule ()
import           DBus.Message ()
import           DBus.NameReservation ()
import           DBus.Types
import           DBus.Types.Internal (typeCode, checkSignature)
import           DBus.Wire ()
import           DBus.Introspection ()

tests :: Test
tests = testGroup "tests"
	[ test_Address
	, test_Signature
	, test_Types
	, test_Variant
	, test_ObjectPath
	, test_InterfaceName
	, test_MemberName
	, test_ErrorName
	, test_BusName
	]

properties :: Test
properties = testGroup "properties"
	[ testProperty "address-parsing" prop_AddressParsing
	, testProperty "signature-parsing" prop_SignatureParsing
	, testProperty "check-signature" prop_CheckSignature
	]

main :: IO ()
main = Test.Framework.defaultMain [tests, properties]

test_Address :: Test
test_Address = testGroup "address"
	[ testGroup "valid"
	  [ testCase "colon" $ do
	    	addr <- requireJust (address ":")
	    	assertEqual (addressMethod addr) ""
	    	assertEqual (addressParameters addr) (Data.Map.fromList [])
	  , testCase "just-scheme" $ do
	    	addr <- requireJust (address "a:")
	    	assertEqual (addressMethod addr) "a"
	    	assertEqual (addressParameters addr) (Data.Map.fromList [])
	  , testCase "param" $ do
	    	addr <- requireJust (address "a:b=c")
	    	assertEqual (addressMethod addr) "a"
	    	assertEqual (addressParameters addr) (Data.Map.fromList [("b", "c")])
	  , testCase "trailing-semicolon" $ do
	    	addrs <- requireJust (addresses "a:;")
	    	assertEqual (length addrs) 1
	    	let [addr1] = addrs
	    	assertEqual (addressMethod addr1) "a"
	    	assertEqual (addressParameters addr1) (Data.Map.fromList [])
	  , testCase "two-schemes" $ do
	    	addrs <- requireJust (addresses "a:;b:")
	    	assertEqual (length addrs) 2
	    	let [addr1, addr2] = addrs
	    	assertEqual (addressMethod addr1) "a"
	    	assertEqual (addressParameters addr1) (Data.Map.fromList [])
	    	assertEqual (addressMethod addr2) "b"
	    	assertEqual (addressParameters addr2) (Data.Map.fromList [])
	  , testCase "trailing-comma" $ do
	    	addr <- requireJust (address "a:b=c,")
	    	assertEqual (addressMethod addr) "a"
	    	assertEqual (addressParameters addr) (Data.Map.fromList [("b", "c")])
	  , testCase "encoded" $ do
	    	addr <- requireJust (address "a:b=%678")
	    	assertEqual (addressMethod addr) "a"
	    	assertEqual (addressParameters addr) (Data.Map.fromList [("b", "g8")])
	  ]
	, testGroup "invalid"
	  [ testCase "empty" (assertNothing (address ""))
	  , testCase "no-colon" (assertNothing (address "a"))
	  , testCase "no-equals" (assertNothing (address "a:b"))
	  , testCase "no-param" (assertNothing (address "a:,"))
	  , testCase "no-param-value" (assertNothing (address "a:b="))
	  ]
	, testGroup "passthrough"
	  [ testCase "plain" (assertEqual (Just "a:b=c") (addressText `fmap` address "a:b=c"))
	  , testCase "encoded" (assertEqual (Just "a:b=Z%5B") (addressText `fmap` address "a:b=%5a%5b"))
	  , testCase "optionally-encoded" (assertEqual (Just "a:b=-_/\\*.") (addressText `fmap` address "a:b=-_/\\*."))
	  , testCase "multiple-params" (assertEqual (Just "a:b=c,d=e") (addressText `fmap` address "a:b=c,d=e"))
	  ]
	, testGroup "instances"
	  [ testCase "eq" (assertEqual (address "a:b=c") (address "a:b=c"))
	  , testCase "show" (assertEqual "(Address \"a:b=c\")" (showsPrec 11 (fromJust (address "a:b=c")) ""))
	  ]
	, testGroup "well-known"
	  [ testCase "system" (withEnv "DBUS_SYSTEM_BUS_ADDRESS" (Just "a:b=c;d:") (do
	    	addrs <- getSystem
	    	assertEqual addrs (Just ["a:b=c", "d:"])))
	  , testCase "default-system" (withEnv "DBUS_SYSTEM_BUS_ADDRESS" Nothing (do
	    	addrs <- getSystem
	    	assertEqual addrs (Just ["unix:path=/var/run/dbus/system_bus_socket"])))
	  , testCase "session" (withEnv "DBUS_SESSION_BUS_ADDRESS" (Just "a:b=c;d:") (do
	    	addrs <- getSession
	    	assertEqual addrs (Just ["a:b=c", "d:"])))
	  , testCase "starter" (withEnv "DBUS_STARTER_BUS_ADDRESS" (Just "a:b=c;d:") (do
	    	addrs <- getStarter
	    	assertEqual addrs (Just ["a:b=c", "d:"])))
	  ]
	]

test_Signature :: Test
test_Signature = testGroup "signature"
	[ testGroup "valid"
	  [ testGroup "atom"
	    [ testCase "bool" $ do
	      	sig <- requireJust (signature "b")
	      	assertEqual (signatureTypes sig) [TypeBoolean]
	    , testCase "word8" $ do
	      	sig <- requireJust (signature "y")
	      	assertEqual (signatureTypes sig) [TypeWord8]
	    , testCase "word16" $ do
	      	sig <- requireJust (signature "q")
	      	assertEqual (signatureTypes sig) [TypeWord16]
	    , testCase "word32" $ do
	      	sig <- requireJust (signature "u")
	      	assertEqual (signatureTypes sig) [TypeWord32]
	    , testCase "word64" $ do
	      	sig <- requireJust (signature "t")
	      	assertEqual (signatureTypes sig) [TypeWord64]
	    , testCase "int16" $ do
	      	sig <- requireJust (signature "n")
	      	assertEqual (signatureTypes sig) [TypeInt16]
	    , testCase "int32" $ do
	      	sig <- requireJust (signature "i")
	      	assertEqual (signatureTypes sig) [TypeInt32]
	    , testCase "int64" $ do
	      	sig <- requireJust (signature "x")
	      	assertEqual (signatureTypes sig) [TypeInt64]
	    , testCase "double" $ do
	      	sig <- requireJust (signature "d")
	      	assertEqual (signatureTypes sig) [TypeDouble]
	    , testCase "string" $ do
	      	sig <- requireJust (signature "s")
	      	assertEqual (signatureTypes sig) [TypeString]
	    , testCase "object-path" $ do
	      	sig <- requireJust (signature "o")
	      	assertEqual (signatureTypes sig) [TypeObjectPath]
	    , testCase "signature" $ do
	      	sig <- requireJust (signature "g")
	      	assertEqual (signatureTypes sig) [TypeSignature]
	    ]
	  , testGroup "container"
	    [ testCase "variant" $ do
	      	sig <- requireJust (signature "v")
	      	assertEqual (signatureTypes sig) [TypeVariant]
	    , testCase "array" $ do
	      	sig <- requireJust (signature "ay")
	      	assertEqual (signatureTypes sig) [TypeArray TypeWord8]
	    , testCase "struct" $ do
	      	sig <- requireJust (signature "(yy)")
	      	assertEqual (signatureTypes sig) [TypeStructure [TypeWord8, TypeWord8]]
	    , testCase "dictionary" $ do
	      	sig <- requireJust (signature "a{yy}")
	      	assertEqual (signatureTypes sig) [TypeDictionary TypeWord8 TypeWord8]
	    ]
	  , testCase "empty" $ do
	    	sig <- requireJust (signature "")
	    	assertEqual (signatureTypes sig) []
	  ]
	, testGroup "invalid"
	  [ testCase "struct-code" (assertNothing (signature "r"))
	  , testCase "struct-empty" (assertNothing (signature "()"))
	  , testCase "dict-code" (assertNothing (signature "e"))
	  , testCase "dict-container-key" (assertNothing (signature "a{vy}"))
	  , testCase "unix-fd" (assertNothing (signature "h"))
	  ]
	, testGroup "length"
	  [ testCase "length-254" $ do
	    	sig <- requireJust (signature (T.replicate 254 "y"))
	    	assertEqual (signatureTypes sig) (replicate 254 TypeWord8)
	  , testCase "length-255" $ do
	    	sig <- requireJust (signature (T.replicate 255 "y"))
	    	assertEqual (signatureTypes sig) (replicate 255 TypeWord8)
	  , testCase "length-256" (assertNothing (signature (T.replicate 256 "y")))
	  ]
	, testGroup "instances"
	  [ testCase "show" (assertEqual "(Signature \"y\")" (showsPrec 11 (fromJust (signature "y")) ""))
	  ]
	]

test_Types :: Test
test_Types = testGroup "types"
	[ testGroup "instances"
	  [ testCase "eq" (assertEqual TypeWord8 TypeWord8)
	  , testGroup "show"
	    [ testCase "Boolean" (assertEqual "Bool" (show TypeBoolean))
	    , testCase "Word8" (assertEqual "Word8" (show TypeWord8))
	    , testCase "Word16" (assertEqual "Word16" (show TypeWord16))
	    , testCase "Word32" (assertEqual "Word32" (show TypeWord32))
	    , testCase "Word64" (assertEqual "Word64" (show TypeWord64))
	    , testCase "Int16" (assertEqual "Int16" (show TypeInt16))
	    , testCase "Int32" (assertEqual "Int32" (show TypeInt32))
	    , testCase "Int64" (assertEqual "Int64" (show TypeInt64))
	    , testCase "Double" (assertEqual "Double" (show TypeDouble))
	    , testCase "String" (assertEqual "String" (show TypeString))
	    , testCase "Signature" (assertEqual "Signature" (show TypeSignature))
	    , testCase "ObjectPath" (assertEqual "ObjectPath" (show TypeObjectPath))
	    , testCase "Variant" (assertEqual "Variant" (show TypeVariant))
	    , testCase "Array" (assertEqual "[Word8]" (show (TypeArray TypeWord8)))
	    , testCase "Dictionary" (assertEqual "Map Word8 (Map Word8 Word8)" (show (TypeDictionary TypeWord8 (TypeDictionary TypeWord8 TypeWord8))))
	    , testCase "Structure" (assertEqual "(Word8, Word16)" (show (TypeStructure [TypeWord8, TypeWord16])))
	    ]
	  ]
	]

test_Variant :: Test
test_Variant = testGroup "variant"
	[ testGroup "variantType"
	  [ testCase "bool" (assertEqual TypeBoolean (variantType (toVariant True)))
	  , testCase "word8" (assertEqual TypeWord8 (variantType (toVariant (0 :: Word8))))
	  , testCase "word16" (assertEqual TypeWord16 (variantType (toVariant (0 :: Word16))))
	  , testCase "word32" (assertEqual TypeWord32 (variantType (toVariant (0 :: Word32))))
	  , testCase "word64" (assertEqual TypeWord64 (variantType (toVariant (0 :: Word64))))
	  , testCase "int16" (assertEqual TypeInt16 (variantType (toVariant (0 :: Int16))))
	  , testCase "int32" (assertEqual TypeInt32 (variantType (toVariant (0 :: Int32))))
	  , testCase "int64" (assertEqual TypeInt64 (variantType (toVariant (0 :: Int64))))
	  , testCase "double" (assertEqual TypeDouble (variantType (toVariant (0 :: Double))))
	  , testCase "string" (assertEqual TypeString (variantType (toVariant (T.pack ""))))
	  , testCase "object-path" (assertEqual TypeObjectPath (variantType (toVariant (objectPath_ "/"))))
	  , testCase "signature" (assertEqual TypeSignature (variantType (toVariant (signature_ ""))))
	  , testCase "variant" (assertEqual TypeVariant (variantType (toVariant (toVariant True))))
	  , testCase "array" (assertEqual (TypeArray TypeBoolean) (variantType (toVariant [True])))
	  , testCase "dictionary" (assertEqual (TypeDictionary TypeBoolean TypeBoolean) (variantType (toVariant (Data.Map.fromList [(True, True)]))))
	  , testCase "structure" (assertEqual (TypeStructure [TypeBoolean, TypeBoolean]) (variantType (toVariant (True, True))))
	  ]
	, testGroup "instances"
	  [ testGroup "show"
	    [ testCase "bool" (assertEqual "Variant True" (show (toVariant True)))
	    , testCase "word8" (assertEqual "Variant 0" (show (toVariant (0 :: Word8))))
	    , testCase "word16" (assertEqual "Variant 0" (show (toVariant (0 :: Word16))))
	    , testCase "word32" (assertEqual "Variant 0" (show (toVariant (0 :: Word32))))
	    , testCase "word64" (assertEqual "Variant 0" (show (toVariant (0 :: Word64))))
	    , testCase "int16" (assertEqual "Variant 0" (show (toVariant (0 :: Int16))))
	    , testCase "int32" (assertEqual "Variant 0" (show (toVariant (0 :: Int32))))
	    , testCase "int64" (assertEqual "Variant 0" (show (toVariant (0 :: Int64))))
	    , testCase "double" (assertEqual "Variant 0.1" (show (toVariant (0.1 :: Double))))
	    , testCase "string" (assertEqual "Variant \"\"" (show (toVariant (T.pack ""))))
	    , testCase "object-path" (assertEqual "Variant (ObjectPath \"/\")" (show (toVariant (objectPath_ "/"))))
	    , testCase "signature" (assertEqual "Variant (Signature \"\")" (show (toVariant (signature_ ""))))
	    , testCase "variant" (assertEqual "Variant (Variant True)" (show (toVariant (toVariant True))))
	    , testCase "array" (assertEqual "(Variant [True, False, True])" (showsPrec 11 (toVariant [True, False, True]) ""))
	    , testCase "dictionary" (assertEqual "(Variant {False: True, True: False})" (showsPrec 11 (toVariant (Data.Map.fromList [(True, False), (False, True)])) ""))
	    , testCase "structure" (assertEqual "(Variant (True, False))" (showsPrec 11 (toVariant (True, False)) ""))
	    ]
	  ]
	]

test_ObjectPath :: Test
test_ObjectPath = testGroup "object-path"
	[ testGroup "valid"
	  [ testCase "root" (assertJust (objectPath "/"))
	  , testCase "plain-1" (assertJust (objectPath "/foo"))
	  , testCase "plain-2" (assertJust (objectPath "/foo/bar"))
	  , testCase "start-with-digit" (assertJust (objectPath "/foo/0"))
	  , testCase "all-characters" (assertJust (objectPath "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, testGroup "invalid"
	  [ testCase "empty" (assertNothing (objectPath ""))
	  , testCase "bad-char" (assertNothing (objectPath "/f!oo"))
	  , testCase "end-with-slash" (assertNothing (objectPath "/foo/"))
	  , testCase "empty-element" (assertNothing (objectPath "/foo//bar"))
	  , testCase "trailing-chars" (assertNothing (objectPath "/foo!"))
	  ]
	]

test_InterfaceName :: Test
test_InterfaceName = testGroup "interface-name"
	[ testGroup "valid"
	  [ testCase "plain" (assertJust (interfaceName "foo.bar"))
	  , testCase "has-digit" (assertJust (interfaceName "foo.bar0"))
	  , testCase "all-characters" (assertJust (interfaceName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, testGroup "invalid"
	  [ testCase "empty" (assertNothing (interfaceName ""))
	  , testCase "one-element" (assertNothing (interfaceName "foo"))
	  , testCase "start-with-digit" (assertNothing (interfaceName "foo.0bar"))
	  , testCase "trailing-chars" (assertNothing (interfaceName "foo.bar!"))
	  ]
	, testGroup "length"
	  [ testCase "length-254"  (assertJust (interfaceName ("f." `T.append` T.replicate 252 "y")))
	  , testCase "length-255" (assertJust (interfaceName ("f." `T.append` T.replicate 253 "y")))
	  , testCase "length-256" (assertNothing (interfaceName ("f." `T.append` T.replicate 254 "y")))
	  ]
	]

test_MemberName :: Test
test_MemberName = testGroup "member-name"
	[ testGroup "valid"
	  [ testCase "plain" (assertJust (memberName "foo"))
	  , testCase "has-digit" (assertJust (memberName "foo0"))
	  , testCase "all-characters" (assertJust (memberName "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, testGroup "invalid"
	  [ testCase "empty" (assertNothing (memberName ""))
	  , testCase "start-with-digit" (assertNothing (memberName "0foo"))
	  , testCase "trailing-chars" (assertNothing (memberName "foo!"))
	  ]
	, testGroup "length"
	  [ testCase "length-254"  (assertJust (memberName (T.replicate 254 "y")))
	  , testCase "length-255" (assertJust (memberName (T.replicate 255 "y")))
	  , testCase "length-256" (assertNothing (memberName (T.replicate 256 "y")))
	  ]
	]

test_ErrorName :: Test
test_ErrorName = testGroup "error-name"
	[ testGroup "valid"
	  [ testCase "plain" (assertJust (errorName "foo.bar"))
	  , testCase "has-digit" (assertJust (errorName "foo.bar0"))
	  , testCase "all-characters" (assertJust (errorName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, testGroup "invalid"
	  [ testCase "empty" (assertNothing (errorName ""))
	  , testCase "one-element" (assertNothing (errorName "foo"))
	  , testCase "start-with-digit" (assertNothing (errorName "foo.0bar"))
	  , testCase "trailing-chars" (assertNothing (errorName "foo.bar!"))
	  ]
	, testGroup "length"
	  [ testCase "length-254"  (assertJust (errorName ("f." `T.append` T.replicate 252 "y")))
	  , testCase "length-255" (assertJust (errorName ("f." `T.append` T.replicate 253 "y")))
	  , testCase "length-256" (assertNothing (errorName ("f." `T.append` T.replicate 254 "y")))
	  ]
	]

test_BusName :: Test
test_BusName = testGroup "bus-name"
	[ testGroup "valid"
	  [ testGroup "unique"
	    [ testCase "plain" (assertJust (busName ":foo.bar"))
	    , testCase "start-with-digit" (assertJust (busName ":foo.0bar"))
	    , testCase "all-characters" (assertJust (busName ":a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	    ]
	  , testGroup "well-known"
	    [ testCase "plain" (assertJust (busName "foo.bar"))
	    , testCase "has-digit" (assertJust (busName "foo.bar0"))
	    , testCase "all-characters" (assertJust (busName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	    ]
	  ]
	, testGroup "invalid"
	  [ testCase "empty" (assertNothing (busName ""))
	  , testCase "well-known-start-with-digit" (assertNothing (busName "foo.0bar"))
	  , testCase "well-known-one-element" (assertNothing (busName "foo"))
	  , testCase "unique-one-element" (assertNothing (busName ":foo"))
	  , testCase "trailing-chars" (assertNothing (busName "foo.bar!"))
	  ]
	, testGroup "length"
	  [ testCase "length-254"  (assertJust (busName (":0." `T.append` T.replicate 251 "y")))
	  , testCase "length-255" (assertJust (busName (":0." `T.append` T.replicate 252 "y")))
	  , testCase "length-256" (assertNothing (busName (":0." `T.append` T.replicate 253 "y")))
	  ]
	]

prop_AddressParsing :: Property
prop_AddressParsing = forAll genAddressText (isJust . address)

genAddressText :: Gen Text
genAddressText = gen where
	methodChars = filter (`notElem` ":;") ['!'..'~']
	keyChars = filter (`notElem` "=;,") ['!'..'~']
	optionallyEncoded = map (:[]) (concat
		[ ['0'..'9']
		, ['a'..'z']
		, ['A'..'Z']
		, "-_/\\*."
		])
	
	param = do
		key <- listOf1 (elements keyChars)
		value <- listOf1 (oneof
			[ elements optionallyEncoded
			, do
			  	c1 <- genHex
			  	c2 <- genHex
			  	return ['%', c1, c2]
			])
		return (key ++ "=" ++ concat value)
	
	gen = do
		method <- listOf (elements methodChars)
		params <- listOf param
		return (T.pack (method ++ ":" ++ (intercalate "," params)))

genHex :: Gen Char
genHex = elements (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])

prop_SignatureParsing :: Property
prop_SignatureParsing = forAll genSignatureText (isJust . signature)

genSignatureText :: Gen Text
genSignatureText = gen where
	any = oneof [atom, container]
	atom = elements ["b", "y", "q", "u", "t", "n", "i", "x", "d", "s", "o", "g"]
	container = oneof
		[ return "v"
		, do
		  	t <- any
		  	return ('a' : t)
		, do
		  	kt <- atom
		  	vt <- any
		  	return (concat ["a{", kt, vt, "}"])
		, do
		  	ts <- listOf1 (halfSized any)
		  	return (concat (["("] ++ ts ++ [")"]))
		]
	gen = do
		chars <- fmap concat (listOf any)
		if length chars > 255
			then halfSized gen
			else return (T.pack chars)

prop_CheckSignature :: Property
prop_CheckSignature = forAll (listOf1 arbitrary) prop where
	prop types = checkSignature types == signature (T.pack (concatMap typeCode types))

halfSized :: Gen a -> Gen a
halfSized gen = sized (\n -> if n > 0
	then resize (div n 2) gen
	else gen)

requireJust :: Maybe a -> IO a
requireJust (Just a) = return a
requireJust Nothing  = do
	assertFailure "expected: (Just _)\n but got: Nothing" -- throws
	error "requireJust: assertFailure didn't fail"

assertJust :: Maybe a -> IO ()
assertJust (Just _) = return ()
assertJust Nothing  = assertFailure "expected: (Just _)\n but got: Nothing"

assertNothing :: Maybe a -> Assertion
assertNothing Nothing = return ()
assertNothing (Just _)  = assertFailure "expected: (Just _)\n but got: Nothing"

assertEqual :: (Show a, Eq a) => a -> a -> Assertion
assertEqual = Test.HUnit.assertEqual ""

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value io = do
	let set val = case val of
		Just x -> System.Posix.Env.setEnv name x True
		Nothing -> System.Posix.Env.unsetEnv name
	old <- System.Posix.Env.getEnv name
	Control.Exception.bracket_ (set value) (set old) io

instance IsString Address where
	fromString s = case address (T.pack s) of
		Just addr -> addr
		Nothing -> error ("Invalid address: " ++ show s)

instance Arbitrary Type where
	arbitrary = oneof [atom, container] where
		atom = elements
			[ TypeBoolean
			, TypeWord8
			, TypeWord16
			, TypeWord32
			, TypeWord64
			, TypeInt16
			, TypeInt32
			, TypeInt64
			, TypeDouble
			, TypeString
			, TypeObjectPath
			, TypeSignature
			]
		container = oneof
			[ return TypeVariant
			, liftM TypeArray arbitrary
			, liftM2 TypeDictionary atom arbitrary
			, liftM TypeStructure (listOf1 (halfSized arbitrary))
			]
