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
import           Test.QuickCheck hiding ((.&.))

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Exception
import           Control.Monad (liftM, liftM2)
import qualified Data.Binary.Builder
import           Data.Bits ((.&.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.Char (chr)
import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text
import qualified Data.Text.Lazy
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Int (Int16, Int32, Int64)
import           Data.Map (Map)
import qualified Data.Map
import           Data.Maybe (isJust, fromJust)
import qualified Data.Set
import           Data.String (IsString, fromString)
import qualified Data.Vector
import qualified System.Posix.Env

import           DBus.Address
import           DBus.Client ()
import           DBus.Client.Simple ()
import           DBus.Connection ()
import           DBus.Message ()
import           DBus.Message.Internal hiding (errorName)
import           DBus.Types
import           DBus.Types.Internal
import           DBus.Wire (marshalMessage, unmarshalMessage)
import qualified DBus.Wire
import qualified DBus.Wire.Internal
import qualified DBus.Introspection

tests :: Test
tests = testGroup "tests"
	[ test_Address
	, test_Signature
	, test_Types
	, test_Variant
	, test_ObjectPath
	, test_ContainerBoxes
	, test_InterfaceName
	, test_MemberName
	, test_ErrorName
	, test_BusName
	, test_Wire
	, test_Introspection
	]

main :: IO ()
main = Test.Framework.defaultMain [tests]

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
	, testGroup "properties"
	  [ testProperty "address-parsing" (forAll genAddressText (isJust . address))
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
	, testGroup "properties"
	  [ testProperty "signature-parsing" (forAll genSignatureText (isJust . signature))
	  , let prop types = checkSignature types == signature (T.pack (concatMap typeCode types)) in
	    testProperty "check-signature" (forAll (listOf1 arbitrary) prop)
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
	[ testGroup "instances-of-IsAtom"
	  [ testCase "bool" (assertAtom TypeBoolean True)
	  , testCase "word8" (assertAtom TypeWord8 (0 :: Word8))
	  , testCase "word16" (assertAtom TypeWord16 (0 :: Word16))
	  , testCase "word32" (assertAtom TypeWord32 (0 :: Word32))
	  , testCase "word64" (assertAtom TypeWord64 (0 :: Word64))
	  , testCase "int16" (assertAtom TypeInt16 (0 :: Int16))
	  , testCase "int32" (assertAtom TypeInt32 (0 :: Int32))
	  , testCase "int64" (assertAtom TypeInt64 (0 :: Int64))
	  , testCase "double" (assertAtom TypeDouble (0 :: Double))
	  , testCase "text" (assertAtom TypeString (Data.Text.pack ""))
	  , testCase "lazy-text" (assertAtom TypeString (Data.Text.Lazy.pack ""))
	  , testCase "string" (assertAtom TypeString ("" :: String))
	  , testCase "object-path" (assertAtom TypeObjectPath (objectPath_ "/"))
	  , testCase "signature" (assertAtom TypeSignature (signature_ ""))
	  ]
	, testGroup "instances-of-IsValue"
	  [ testCase "variant" (assertValue TypeVariant (toVariant True))
	  , testCase "list" (assertValue (TypeArray TypeBoolean) [True])
	  , testCase "vector" (assertValue (TypeArray TypeBoolean) (Data.Vector.fromList [True]))
	  , testCase "bytestring-strict" (assertValue (TypeArray TypeWord8) Data.ByteString.empty)
	  , testCase "bytestring-lazy" (assertValue (TypeArray TypeWord8) Data.ByteString.Lazy.empty)
	  , testCase "map" (assertValue (TypeDictionary TypeBoolean TypeBoolean) (Data.Map.fromList [(True, True)]))
	  , testCase "tuple-2" (assertValue (TypeStructure (replicate 2 TypeBoolean)) (True, True))
	  , testCase "tuple-3" (assertValue (TypeStructure (replicate 3 TypeBoolean)) (True, True, True))
	  , testCase "tuple-4" (assertValue (TypeStructure (replicate 4 TypeBoolean)) (True, True, True, True))
	  , testCase "tuple-5" (assertValue (TypeStructure (replicate 5 TypeBoolean)) (True, True, True, True, True))
	  , testCase "tuple-6" (assertValue (TypeStructure (replicate 6 TypeBoolean)) (True, True, True, True, True, True))
	  , testCase "tuple-7" (assertValue (TypeStructure (replicate 7 TypeBoolean)) (True, True, True, True, True, True, True))
	  , testCase "tuple-8" (assertValue (TypeStructure (replicate 8 TypeBoolean)) (True, True, True, True, True, True, True, True))
	  , testCase "tuple-9" (assertValue (TypeStructure (replicate 9 TypeBoolean)) (True, True, True, True, True, True, True, True, True))
	  , testCase "tuple-10" (assertValue (TypeStructure (replicate 10 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True))
	  , testCase "tuple-11" (assertValue (TypeStructure (replicate 11 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True))
	  , testCase "tuple-12" (assertValue (TypeStructure (replicate 12 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True))
	  , testCase "tuple-13" (assertValue (TypeStructure (replicate 13 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True))
	  , testCase "tuple-14" (assertValue (TypeStructure (replicate 14 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True, True))
	  , testCase "tuple-15" (assertValue (TypeStructure (replicate 15 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True, True, True))
	  ]
	, testGroup "show-variant"
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
	  , testCase "array" (assertEqual "Variant [True, False]" (show (toVariant [True, False])))
	  , testGroup "array"
	    [ testCase "bytestring-strict" (assertEqual "Variant b\"\"" (show (toVariant Data.ByteString.empty)))
	    , testCase "bytestring-lazy" (assertEqual "Variant b\"\"" (show (toVariant Data.ByteString.Lazy.empty)))
	    , testCase "array-of-word8" (assertEqual "Variant b\"\"" (show (toVariant ([] :: [Word8]))))
	    ]
	  , testCase "dictionary" (assertEqual "(Variant {False: True, True: False})" (showsPrec 11 (toVariant (Data.Map.fromList [(True, False), (False, True)])) ""))
	  , testCase "structure" (assertEqual "(Variant (True, False))" (showsPrec 11 (toVariant (True, False)) ""))
	  ]
	, testCase "compare-byte-storage" $ do
	  	assertEqual (toValue (Data.Vector.fromList [0 :: Word8]))
	  	            (toValue (Data.Vector.fromList [0 :: Word8]))
	  	assertEqual (toValue (Data.Vector.fromList [0 :: Word8]))
	  	            (toValue (Data.ByteString.pack [0]))
	  	assertEqual (toValue (Data.Vector.fromList [0 :: Word8]))
	  	            (toValue (Data.ByteString.Lazy.pack [0]))
	  	
	  	assertEqual (toValue (Data.ByteString.pack [0]))
	  	            (toValue (Data.Vector.fromList [0 :: Word8]))
	  	assertEqual (toValue (Data.ByteString.pack [0]))
	  	            (toValue (Data.ByteString.pack [0]))
	  	assertEqual (toValue (Data.ByteString.pack [0]))
	  	            (toValue (Data.ByteString.Lazy.pack [0]))
	  	
	  	assertEqual (toValue (Data.ByteString.Lazy.pack [0]))
	  	            (toValue (Data.Vector.fromList [0 :: Word8]))
	  	assertEqual (toValue (Data.ByteString.Lazy.pack [0]))
	  	            (toValue (Data.ByteString.pack [0]))
	  	assertEqual (toValue (Data.ByteString.Lazy.pack [0]))
	  	            (toValue (Data.ByteString.Lazy.pack [0]))
	]

test_ContainerBoxes :: Test
test_ContainerBoxes = testGroup "container-boxes"
	[ testGroup "structure"
	  [ testCase "instance-of-Eq" (assertEqual (Structure []) (Structure []))
	  , testCase "instance-of-Show" (assertEqual "(True, False)" (show (Structure [toValue True, toValue False])))
	  , testCase "instance-of-IsVariant" (assertVariant (TypeStructure [TypeBoolean]) (Structure [toValue True]))
	  ]
	, testGroup "array"
	  [ testCase "instance-of-Eq" $ do
	    	assertEqual (Array TypeBoolean (Data.Vector.fromList [toValue True]))
	    	            (Array TypeBoolean (Data.Vector.fromList [toValue True]))
	    	assertEqual (Array TypeWord8 (Data.Vector.fromList [toValue (0 :: Word8)]))
	    	            (ArrayBytes (Data.ByteString.pack [0]))
	  , testCase "instance-of-Show" $ do
	    	assertEqual "[True, False]" (show (Array TypeBoolean (Data.Vector.fromList [toValue True, toValue False])))
	    	assertEqual "b\"\"" (show (Array TypeWord8 Data.Vector.empty))
	    	assertEqual "b\"\"" (show (ArrayBytes Data.ByteString.empty))
	  , testCase "instance-of-IsVariant" $ do
	    	assertVariant (TypeArray TypeWord8) (Array TypeWord8 Data.Vector.empty)
	    	assertVariant (TypeArray TypeWord8) (ArrayBytes Data.ByteString.empty)
	  ]
	, testGroup "dictionary"
	  [ testCase "instance-of-Eq" (assertEqual (Dictionary TypeWord8 TypeWord8 Data.Map.empty)
	                                           (Dictionary TypeWord8 TypeWord8 Data.Map.empty))
	  , testCase "instance-of-Show" (assertEqual "{}" (show (Dictionary TypeWord8 TypeWord8 Data.Map.empty)))
	  , testCase "instance-of-IsVariant" (assertVariant (TypeDictionary TypeWord8 TypeWord8) (Dictionary TypeWord8 TypeWord8 Data.Map.empty))
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
	[ testCase "instance-of-IsVariant" (assertVariant TypeString (interfaceName_ "foo.bar"))
	, testGroup "valid"
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
	[ testCase "instance-of-IsVariant" (assertVariant TypeString (memberName_ "foo"))
	, testGroup "valid"
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
	[ testCase "instance-of-IsVariant" (assertVariant TypeString (errorName_ "foo.bar"))
	, testGroup "valid"
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
	[ testCase "instance-of-IsVariant" (assertVariant TypeString (busName_ "foo.bar"))
	, testGroup "valid"
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

test_Wire :: Test
test_Wire = testGroup "wire"
	[ testProperty "value-passthrough" prop_ValuePassthrough
	, testGroup "message-passthrough"
	  [ testProperty "method-call" (forAll (arbitrary :: Gen MethodCall) (\msg e s ->
	    	let Right bytes = marshalMessage e s msg in
	    	let Right received = unmarshalMessage bytes in
	    	ReceivedMethodCall s Nothing msg == received))
	  , testProperty "method-return" (forAll (arbitrary :: Gen MethodReturn) (\msg e s ->
	    	let Right bytes = marshalMessage e s msg in
	    	let Right received = unmarshalMessage bytes in
	    	ReceivedMethodReturn s Nothing msg == received))
	  , testProperty "error" (forAll (arbitrary :: Gen Error) (\msg e s ->
	    	let Right bytes = marshalMessage e s msg in
	    	let Right received = unmarshalMessage bytes in
	    	ReceivedError s Nothing msg == received))
	  , testProperty "signal" (forAll (arbitrary :: Gen Signal) (\msg e s ->
	    	let Right bytes = marshalMessage e s msg in
	    	let Right received = unmarshalMessage bytes in
	    	ReceivedSignal s Nothing msg == received))
	  ]
	]

test_Introspection :: Test
test_Introspection = testGroup "introspection"
	[ testProperty "xml-passthrough" (\obj ->
	  	let (DBus.Introspection.Object path _ _) = obj in
	  	let Just xml = DBus.Introspection.toXML obj in
	  	DBus.Introspection.fromXML path xml == Just obj)
	]

prop_ValuePassthrough :: Value -> DBus.Wire.Endianness -> Bool
prop_ValuePassthrough v e = unmarshaled == v where
	Right bytes = marshal e v
	Right unmarshaled = unmarshal e (valueType v) bytes

marshal :: DBus.Wire.Endianness -> Value -> Either String ByteString
marshal e v = case DBus.Wire.Internal.unWire (DBus.Wire.Internal.marshal v) e (DBus.Wire.Internal.MarshalState Data.Binary.Builder.empty 0) of
	DBus.Wire.Internal.WireRR _ (DBus.Wire.Internal.MarshalState builder _) -> Right (Data.ByteString.concat (Data.ByteString.Lazy.toChunks (Data.Binary.Builder.toLazyByteString builder)))
	DBus.Wire.Internal.WireRL err -> Left err

unmarshal :: DBus.Wire.Endianness -> Type -> ByteString -> Either String Value
unmarshal e t bytes = case DBus.Wire.Internal.unWire (DBus.Wire.Internal.unmarshal t) e (DBus.Wire.Internal.UnmarshalState bytes 0) of
	DBus.Wire.Internal.WireRR v _ -> Right v
	DBus.Wire.Internal.WireRL err -> Left err

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

genSignatureText :: Gen Text
genSignatureText = gen where
	anyType = oneof [atom, container]
	atom = elements ["b", "y", "q", "u", "t", "n", "i", "x", "d", "s", "o", "g"]
	container = oneof
		[ return "v"
		, do
		  	t <- anyType
		  	return ('a' : t)
		, do
		  	kt <- atom
		  	vt <- anyType
		  	return (concat ["a{", kt, vt, "}"])
		, do
		  	ts <- listOf1 (halfSized anyType)
		  	return (concat (["("] ++ ts ++ [")"]))
		]
	gen = do
		chars <- fmap concat (listOf anyType)
		if length chars > 255
			then halfSized gen
			else return (T.pack chars)

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

assertAtom :: (Eq a, Show a, IsAtom a) => Type -> a -> Assertion
assertAtom t a = do
	assertEqual t (atomType (toAtom a))
	assertEqual (fromAtom (toAtom a)) (Just a)
	assertEqual (toAtom a) (toAtom a)
	assertValue t a

assertValue :: (Eq a, Show a, IsValue a) => Type -> a -> Assertion
assertValue t a = do
	assertEqual t (DBus.Types.typeOf a)
	assertEqual t (DBus.Types.Internal.typeOf a)
	assertEqual t (valueType (toValue a))
	assertEqual (fromValue (toValue a)) (Just a)
	assertEqual (toValue a) (toValue a)
	assertVariant t a

assertVariant :: (Eq a, Show a, IsVariant a) => Type -> a -> Assertion
assertVariant t a = do
	assertEqual t (variantType (toVariant a))
	assertEqual (fromVariant (toVariant a)) (Just a)
	assertEqual (toVariant a) (toVariant a)

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

instance Arbitrary Atom where
	arbitrary = oneof
		[ liftM AtomWord8 arbitrary
		, liftM AtomWord16 arbitrary
		, liftM AtomWord32 arbitrary
		, liftM AtomWord64 arbitrary
		, liftM AtomInt16 arbitrary
		, liftM AtomInt32 arbitrary
		, liftM AtomInt64 arbitrary
		, liftM AtomBool arbitrary
		, liftM AtomDouble arbitrary
		, liftM AtomText arbitrary
		, liftM AtomObjectPath arbitrary
		, liftM AtomSignature arbitrary
		]

instance Arbitrary Value where
	arbitrary = oneof
		[ liftM ValueAtom arbitrary
		, liftM ValueBytes arbitrary
		
		-- TODO: proper arbitrary ValueVector
		, elements
		  [ toValue (Data.Vector.fromList ([] :: [Word8]))
		  , toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word8]))
		  , toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word16]))
		  , toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word32]))
		  , toValue (Data.Vector.fromList ([0, 1, 2, 3, 4, 5] :: [Word64]))
		  , toValue (Data.Vector.fromList (["foo", "bar", "baz"] :: [Text]))
		  ]
		
		-- TODO: proper arbitrary ValueMap
		, elements
		  [ toValue (Data.Map.fromList [] :: Map Text Text)
		  , toValue (Data.Map.fromList [("foo", "bar"), ("baz", "qux")] :: Map Text Text)
		  ]
		
		, liftM ValueStructure (listOf1 (halfSized arbitrary))
		, liftM ValueVariant arbitrary
		]

instance Arbitrary Variant where
	arbitrary = do
		val <- arbitrary
		case checkSignature [valueType val] of
			Just _ -> return (Variant val)
			Nothing -> halfSized arbitrary

instance Arbitrary Data.Text.Text where
	arbitrary = liftM Data.Text.pack genUnicode

genUnicode :: Gen [Char]
genUnicode = string where
	string = sized $ \n -> do
		k <- choose (0,n)
		sequence [ char | _ <- [1..k] ]
	
	excluding :: [a -> Bool] -> Gen a -> Gen a
	excluding bad gen = loop where
		loop = do
			x <- gen
			if or (map ($ x) bad)
				then loop
				else return x
	
	reserved = [lowSurrogate, highSurrogate, noncharacter]
	lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
	highSurrogate c = c >= 0xD800 && c <= 0xDBFF
	noncharacter c = masked == 0xFFFE || masked == 0xFFFF where
		masked = c .&. 0xFFFF
	
	ascii = choose (0x20, 0x7F)
	plane0 = choose (0xF0, 0xFFFF)
	plane1 = oneof [ choose (0x10000, 0x10FFF)
	               , choose (0x11000, 0x11FFF)
	               , choose (0x12000, 0x12FFF)
	               , choose (0x13000, 0x13FFF)
	               , choose (0x1D000, 0x1DFFF)
	               , choose (0x1F000, 0x1FFFF)
	               ]
	plane2 = oneof [ choose (0x20000, 0x20FFF)
	               , choose (0x21000, 0x21FFF)
	               , choose (0x22000, 0x22FFF)
	               , choose (0x23000, 0x23FFF)
	               , choose (0x24000, 0x24FFF)
	               , choose (0x25000, 0x25FFF)
	               , choose (0x26000, 0x26FFF)
	               , choose (0x27000, 0x27FFF)
	               , choose (0x28000, 0x28FFF)
	               , choose (0x29000, 0x29FFF)
	               , choose (0x2A000, 0x2AFFF)
	               , choose (0x2B000, 0x2BFFF)
	               , choose (0x2F000, 0x2FFFF)
	               ]
	plane14 = choose (0xE0000, 0xE0FFF)
	planes = [ascii, plane0, plane1, plane2, plane14]
	
	char = chr `fmap` excluding reserved (oneof planes)

instance Arbitrary Data.ByteString.ByteString where
	arbitrary = liftM Data.ByteString.pack arbitrary

instance Arbitrary Data.ByteString.Lazy.ByteString where
	arbitrary = liftM Data.ByteString.Lazy.fromChunks arbitrary

instance Arbitrary ObjectPath where
	-- TODO: proper arbitrary generation
	arbitrary = elements
		[ objectPath_ "/"
		, objectPath_ "/foo"
		, objectPath_ "/foo/bar"
		]

instance Arbitrary InterfaceName where
	-- TODO: proper arbitrary generation
	arbitrary = elements
		[ interfaceName_ "foo.bar"
		, interfaceName_ "foo.bar0"
		]

instance Arbitrary MemberName where
	-- TODO: proper arbitrary generation
	arbitrary = elements
		[ memberName_ "foo"
		, memberName_ "foo0"
		]

instance Arbitrary ErrorName where
	-- TODO: proper arbitrary generation
	arbitrary = elements
		[ errorName_ "foo.bar"
		, errorName_ "foo.bar0"
		]

instance Arbitrary BusName where
	-- TODO: proper arbitrary generation
	arbitrary = elements
		[ busName_ "foo.bar"
		, busName_ ":foo.bar"
		, busName_ ":foo.bar"
		]

instance (Arbitrary a, Ord a) => Arbitrary (Data.Set.Set a) where
	arbitrary = liftM Data.Set.fromList arbitrary

instance Arbitrary Signature where
	arbitrary = liftM signature_ genSignatureText

instance Arbitrary DBus.Wire.Endianness where
	arbitrary = elements [DBus.Wire.BigEndian, DBus.Wire.LittleEndian]

instance Arbitrary Flag where
	arbitrary = elements [NoReplyExpected, NoAutoStart]

instance Arbitrary Serial where
	arbitrary = liftM Serial arbitrary

instance Arbitrary MethodCall where
	arbitrary = MethodCall
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> genMessageBody

instance Arbitrary MethodReturn where
	arbitrary = MethodReturn
		<$> arbitrary
		<*> arbitrary
		<*> genMessageBody

instance Arbitrary Error where
	arbitrary = Error
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> genMessageBody

instance Arbitrary Signal where
	arbitrary = Signal
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> genMessageBody

genMessageBody :: Gen [Variant]
genMessageBody = do
	vars <- arbitrary
	case checkSignature (map variantType vars) of
		Just _ -> return vars
		Nothing -> halfSized genMessageBody

instance Arbitrary DBus.Introspection.Object where
	arbitrary = arbitrary >>= subObject

subObject :: ObjectPath -> Gen DBus.Introspection.Object
subObject parentPath = sized $ \n -> resize (min n 4) $ do
	let nonRoot = do
		x <- arbitrary
		case objectPathText x of
			"/" -> nonRoot
			x'  -> return x'
	
	thisPath <- nonRoot
	let path' = case objectPathText parentPath of
		"/" -> thisPath
		x   -> Data.Text.append x thisPath
	let path = objectPath_ path'
	ifaces <- arbitrary
	children <- halfSized (listOf (subObject path))
	return (DBus.Introspection.Object path ifaces children)

instance Arbitrary DBus.Introspection.Interface where
	arbitrary = DBus.Introspection.Interface
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary

instance Arbitrary DBus.Introspection.Method where
	arbitrary = DBus.Introspection.Method
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary

instance Arbitrary DBus.Introspection.Signal where
	arbitrary = DBus.Introspection.Signal
		<$> arbitrary
		<*> arbitrary

instance Arbitrary DBus.Introspection.Parameter where
	arbitrary = DBus.Introspection.Parameter
		<$> arbitrary
		<*> singleType

instance Arbitrary DBus.Introspection.Property where
	arbitrary = DBus.Introspection.Property
		<$> arbitrary
		<*> singleType
		<*> elements
			[ []
			, [ DBus.Introspection.Read ]
			, [ DBus.Introspection.Write ]
			, [ DBus.Introspection.Read
			  , DBus.Introspection.Write ]
			]

singleType :: Gen Signature
singleType = do
	t <- arbitrary
	case checkSignature [t] of
		Just s -> return s
		Nothing -> halfSized singleType
