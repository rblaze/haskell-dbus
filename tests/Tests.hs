{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RelaxedPolyRec #-}

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

import           Prelude hiding (fail)

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding ((.&.), property)

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Exception
import           Control.Monad (liftM, liftM2)
import           Control.Monad.IO.Class (liftIO)
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

tests :: [Suite]
tests = [ suite_Address
        , suite_Signature
        , suite_Types
        , suite_Variant
        , suite_ObjectPath
        , suite_ContainerBoxes
        , suite_InterfaceName
        , suite_MemberName
        , suite_ErrorName
        , suite_BusName
        , suite_Wire
        , suite_Introspection
        ]

main :: IO ()
main = Test.Chell.defaultMain tests

suite_Address :: Suite
suite_Address = suite "address"
	[ suite "valid"
		[ test $ assertions "colon" $ do
			let m_addr = address ":"
			$assert $ just m_addr
			let Just addr = m_addr
			$expect $ equal (addressMethod addr) ""
			$expect $ equal (addressParameters addr) (Data.Map.fromList [])
		
		, test $ assertions "just-scheme" $ do
			addr <- requireJust (address "a:")
			$expect $ equal (addressMethod addr) "a"
			$expect $ equal (addressParameters addr) (Data.Map.fromList [])
		
		, test $ assertions "param" $ do
			addr <- requireJust (address "a:b=c")
			$expect $ equal (addressMethod addr) "a"
			$expect $ equal (addressParameters addr) (Data.Map.fromList [("b", "c")])
		
		, test $ assertions "trailing-semicolon" $ do
			addrs <- requireJust (addresses "a:;")
			$assert $ equal (length addrs) 1
			let [addr1] = addrs
			$expect $ equal (addressMethod addr1) "a"
			$expect $ equal (addressParameters addr1) (Data.Map.fromList [])
		
		, test $ assertions "two-schemes" $ do
			addrs <- requireJust (addresses "a:;b:")
			$assert $ equal (length addrs) 2
			let [addr1, addr2] = addrs
			$expect $ equal (addressMethod addr1) "a"
			$expect $ equal (addressParameters addr1) (Data.Map.fromList [])
			$expect $ equal (addressMethod addr2) "b"
			$expect $ equal (addressParameters addr2) (Data.Map.fromList [])
		
		, test $ assertions "trailing-comma" $ do
			addr <- requireJust (address "a:b=c,")
			$expect $ equal (addressMethod addr) "a"
			$expect $ equal (addressParameters addr) (Data.Map.fromList [("b", "c")])
		
		, test $ assertions "encoded" $ do
			addr <- requireJust (address "a:b=%678")
			$expect $ equal (addressMethod addr) "a"
			$expect $ equal (addressParameters addr) (Data.Map.fromList [("b", "g8")])
		]
	
	, suite "invalid"
		[ test $ assertions "empty" $ do
			$expect $ nothing (address "")
		
		, test $ assertions "no-colon" $ do
			$expect $ nothing (address "a")
		
		, test $ assertions "no-equals" $ do
			$expect $ nothing (address "a:b")
		
		, test $ assertions "no-param" $ do
			$expect $ nothing (address "a:,")
		
		, test $ assertions "no-param-value" $ do
			$expect $ nothing (address "a:b=")
		]
	
	, suite "passthrough"
		[ test $ assertions "plain" $ do
			$assert $ equal (Just "a:b=c") (addressText `fmap` address "a:b=c")
		, test $ assertions "encoded" $ do
			$assert $ equal (Just "a:b=Z%5B") (addressText `fmap` address "a:b=%5a%5b")
		, test $ assertions "optionally-encoded" $ do
			$assert $ equal (Just "a:b=-_/\\*.") (addressText `fmap` address "a:b=-_/\\*.")
		, test $ assertions "multiple-params" $ do
			$assert $ equal (Just "a:b=c,d=e") (addressText `fmap` address "a:b=c,d=e")
		]
	
	, suite "instances"
		[ test $ assertions "eq" $ do
			$assert $ equal (address "a:b=c") (address "a:b=c")
		, test $ assertions "show" $ do
			$assert $ equal "(Address \"a:b=c\")" (showsPrec 11 (fromJust (address "a:b=c")) "")
		]
	
	, suite "well-known"
		[ test $ assertions "system" $ do
			liftIO $ System.Posix.Env.setEnv "DBUS_SYSTEM_BUS_ADDRESS" "a:b=c;d:" True
			addrs <- liftIO getSystem
			$assert $ equal addrs (Just ["a:b=c", "d:"])
		
		, test $ assertions "default-system" $ do
			liftIO $ System.Posix.Env.unsetEnv "DBUS_SYSTEM_BUS_ADDRESS"
			addrs <- liftIO getSystem
			$assert $ equal addrs (Just ["unix:path=/var/run/dbus/system_bus_socket"])
		
		, test $ assertions "session" $ do
			liftIO $ System.Posix.Env.setEnv "DBUS_SESSION_BUS_ADDRESS" "a:b=c;d:" True
			addrs <- liftIO getSession
			$assert $ equal addrs (Just ["a:b=c", "d:"])
		
		, test $ assertions "starter" $ do
			liftIO $ System.Posix.Env.setEnv "DBUS_STARTER_BUS_ADDRESS" "a:b=c;d:" True
			addrs <- liftIO getStarter
			$assert $ equal addrs (Just ["a:b=c", "d:"])
		]
	
	, suite "properties"
		[ property "address-parsing" (forAll genAddressText (isJust . address))
		]
	]

suite_Signature :: Suite
suite_Signature = suite "signature"
	[ suite "valid"
		[ suite "atom"
			[ test $ assertions "bool" $ do
				sig <- requireJust (signature "b")
				$assert $ equal (signatureTypes sig) [TypeBoolean]
			, test $ assertions "word8" $ do
				sig <- requireJust (signature "y")
				$assert $ equal (signatureTypes sig) [TypeWord8]
			, test $ assertions "word16" $ do
				sig <- requireJust (signature "q")
				$assert $ equal (signatureTypes sig) [TypeWord16]
			, test $ assertions "word32" $ do
				sig <- requireJust (signature "u")
				$assert $ equal (signatureTypes sig) [TypeWord32]
			, test $ assertions "word64" $ do
				sig <- requireJust (signature "t")
				$assert $ equal (signatureTypes sig) [TypeWord64]
			, test $ assertions "int16" $ do
				sig <- requireJust (signature "n")
				$assert $ equal (signatureTypes sig) [TypeInt16]
			, test $ assertions "int32" $ do
				sig <- requireJust (signature "i")
				$assert $ equal (signatureTypes sig) [TypeInt32]
			, test $ assertions "int64" $ do
				sig <- requireJust (signature "x")
				$assert $ equal (signatureTypes sig) [TypeInt64]
			, test $ assertions "double" $ do
				sig <- requireJust (signature "d")
				$assert $ equal (signatureTypes sig) [TypeDouble]
			, test $ assertions "string" $ do
				sig <- requireJust (signature "s")
				$assert $ equal (signatureTypes sig) [TypeString]
			, test $ assertions "object-path" $ do
				sig <- requireJust (signature "o")
				$assert $ equal (signatureTypes sig) [TypeObjectPath]
			, test $ assertions "signature" $ do
				sig <- requireJust (signature "g")
				$assert $ equal (signatureTypes sig) [TypeSignature]
			]
		
		, suite "container"
			[ test $ assertions "variant" $ do
				sig <- requireJust (signature "v")
				$assert $ equal (signatureTypes sig) [TypeVariant]
			, test $ assertions "array" $ do
				sig <- requireJust (signature "ay")
				$assert $ equal (signatureTypes sig) [TypeArray TypeWord8]
			, test $ assertions "struct" $ do
				sig <- requireJust (signature "(yy)")
				$assert $ equal (signatureTypes sig) [TypeStructure [TypeWord8, TypeWord8]]
			, test $ assertions "dictionary" $ do
				sig <- requireJust (signature "a{yy}")
				$assert $ equal (signatureTypes sig) [TypeDictionary TypeWord8 TypeWord8]
			]
		
		, test $ assertions "empty" $ do
			sig <- requireJust (signature "")
			$assert $ equal (signatureTypes sig) []
		]
	
	, suite "invalid"
		[ test $ assertions "struct-code" ($assert $ nothing (signature "r"))
		, test $ assertions "struct-empty" ($assert $ nothing (signature "()"))
		, test $ assertions "dict-code" ($assert $ nothing (signature "e"))
		, test $ assertions "dict-container-key" ($assert $ nothing (signature "a{vy}"))
		, test $ assertions "unix-fd" ($assert $ nothing (signature "h"))
		]
	
	, suite "length"
		[ test $ assertions "length-254" $ do
				sig <- requireJust (signature (T.replicate 254 "y"))
				$assert $ equal (signatureTypes sig) (replicate 254 TypeWord8)
		, test $ assertions "length-255" $ do
				sig <- requireJust (signature (T.replicate 255 "y"))
				$assert $ equal (signatureTypes sig) (replicate 255 TypeWord8)
		, test $ assertions "length-256" ($assert $ nothing (signature (T.replicate 256 "y")))
		]
	
	, suite "instances"
		[ test $ assertions "show" ($assert $ equal "(Signature \"y\")" (showsPrec 11 (fromJust (signature "y")) ""))
		]
	
	, suite "properties"
		[ property "signature-parsing" (forAll genSignatureText (isJust . signature))
		, property "check-signature" $ do
			let prop types = checkSignature types == signature (T.pack (concatMap typeCode types))
			forAll (listOf1 arbitrary) prop
		]
	]

suite_Types :: Suite
suite_Types = suite "types"
	[ suite "instances"
		[ test $ assertions "eq" ($assert $ equal TypeWord8 TypeWord8)
		, suite "show"
			[ test $ assertions "Boolean" ($assert $ equal "Bool" (show TypeBoolean))
			, test $ assertions "Word8" ($assert $ equal "Word8" (show TypeWord8))
			, test $ assertions "Word16" ($assert $ equal "Word16" (show TypeWord16))
			, test $ assertions "Word32" ($assert $ equal "Word32" (show TypeWord32))
			, test $ assertions "Word64" ($assert $ equal "Word64" (show TypeWord64))
			, test $ assertions "Int16" ($assert $ equal "Int16" (show TypeInt16))
			, test $ assertions "Int32" ($assert $ equal "Int32" (show TypeInt32))
			, test $ assertions "Int64" ($assert $ equal "Int64" (show TypeInt64))
			, test $ assertions "Double" ($assert $ equal "Double" (show TypeDouble))
			, test $ assertions "String" ($assert $ equal "String" (show TypeString))
			, test $ assertions "Signature" ($assert $ equal "Signature" (show TypeSignature))
			, test $ assertions "ObjectPath" ($assert $ equal "ObjectPath" (show TypeObjectPath))
			, test $ assertions "Variant" ($assert $ equal "Variant" (show TypeVariant))
			, test $ assertions "Array" ($assert $ equal "[Word8]" (show (TypeArray TypeWord8)))
			, test $ assertions "Dictionary" ($assert $ equal "Map Word8 (Map Word8 Word8)" (show (TypeDictionary TypeWord8 (TypeDictionary TypeWord8 TypeWord8))))
			, test $ assertions "Structure" ($assert $ equal "(Word8, Word16)" (show (TypeStructure [TypeWord8, TypeWord16])))
			]
		]
	]

suite_Variant :: Suite
suite_Variant = suite "variant"
	[ suite "instances-of-IsAtom"
		[ test $ assertions "bool" (assertAtom TypeBoolean True)
		, test $ assertions "word8" (assertAtom TypeWord8 (0 :: Word8))
		, test $ assertions "word16" (assertAtom TypeWord16 (0 :: Word16))
		, test $ assertions "word32" (assertAtom TypeWord32 (0 :: Word32))
		, test $ assertions "word64" (assertAtom TypeWord64 (0 :: Word64))
		, test $ assertions "int16" (assertAtom TypeInt16 (0 :: Int16))
		, test $ assertions "int32" (assertAtom TypeInt32 (0 :: Int32))
		, test $ assertions "int64" (assertAtom TypeInt64 (0 :: Int64))
		, test $ assertions "double" (assertAtom TypeDouble (0 :: Double))
		, test $ assertions "text" (assertAtom TypeString (Data.Text.pack ""))
		, test $ assertions "lazy-text" (assertAtom TypeString (Data.Text.Lazy.pack ""))
		, test $ assertions "string" (assertAtom TypeString ("" :: String))
		, test $ assertions "object-path" (assertAtom TypeObjectPath (objectPath_ "/"))
		, test $ assertions "signature" (assertAtom TypeSignature (signature_ ""))
		]
	
	, suite "instances-of-IsValue"
		[ test $ assertions "variant" (assertValue TypeVariant (toVariant True))
		, test $ assertions "list" (assertValue (TypeArray TypeBoolean) [True])
		, test $ assertions "vector" (assertValue (TypeArray TypeBoolean) (Data.Vector.fromList [True]))
		, test $ assertions "bytestring-strict" (assertValue (TypeArray TypeWord8) Data.ByteString.empty)
		, test $ assertions "bytestring-lazy" (assertValue (TypeArray TypeWord8) Data.ByteString.Lazy.empty)
		, test $ assertions "map" (assertValue (TypeDictionary TypeBoolean TypeBoolean) (Data.Map.fromList [(True, True)]))
		, test $ assertions "tuple-2" (assertValue (TypeStructure (replicate 2 TypeBoolean)) (True, True))
		, test $ assertions "tuple-3" (assertValue (TypeStructure (replicate 3 TypeBoolean)) (True, True, True))
		, test $ assertions "tuple-4" (assertValue (TypeStructure (replicate 4 TypeBoolean)) (True, True, True, True))
		, test $ assertions "tuple-5" (assertValue (TypeStructure (replicate 5 TypeBoolean)) (True, True, True, True, True))
		, test $ assertions "tuple-6" (assertValue (TypeStructure (replicate 6 TypeBoolean)) (True, True, True, True, True, True))
		, test $ assertions "tuple-7" (assertValue (TypeStructure (replicate 7 TypeBoolean)) (True, True, True, True, True, True, True))
		, test $ assertions "tuple-8" (assertValue (TypeStructure (replicate 8 TypeBoolean)) (True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-9" (assertValue (TypeStructure (replicate 9 TypeBoolean)) (True, True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-10" (assertValue (TypeStructure (replicate 10 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-11" (assertValue (TypeStructure (replicate 11 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-12" (assertValue (TypeStructure (replicate 12 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-13" (assertValue (TypeStructure (replicate 13 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-14" (assertValue (TypeStructure (replicate 14 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True, True))
		, test $ assertions "tuple-15" (assertValue (TypeStructure (replicate 15 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True, True, True))
		]
	
	, suite "show-variant"
		[ test $ assertions "bool" ($assert $ equal "Variant True" (show (toVariant True)))
		, test $ assertions "word8" ($assert $ equal "Variant 0" (show (toVariant (0 :: Word8))))
		, test $ assertions "word16" ($assert $ equal "Variant 0" (show (toVariant (0 :: Word16))))
		, test $ assertions "word32" ($assert $ equal "Variant 0" (show (toVariant (0 :: Word32))))
		, test $ assertions "word64" ($assert $ equal "Variant 0" (show (toVariant (0 :: Word64))))
		, test $ assertions "int16" ($assert $ equal "Variant 0" (show (toVariant (0 :: Int16))))
		, test $ assertions "int32" ($assert $ equal "Variant 0" (show (toVariant (0 :: Int32))))
		, test $ assertions "int64" ($assert $ equal "Variant 0" (show (toVariant (0 :: Int64))))
		, test $ assertions "double" ($assert $ equal "Variant 0.1" (show (toVariant (0.1 :: Double))))
		, test $ assertions "string" ($assert $ equal "Variant \"\"" (show (toVariant (T.pack ""))))
		, test $ assertions "object-path" ($assert $ equal "Variant (ObjectPath \"/\")" (show (toVariant (objectPath_ "/"))))
		, test $ assertions "signature" ($assert $ equal "Variant (Signature \"\")" (show (toVariant (signature_ ""))))
		, test $ assertions "variant" ($assert $ equal "Variant (Variant True)" (show (toVariant (toVariant True))))
		, test $ assertions "array" ($assert $ equal "Variant [True, False]" (show (toVariant [True, False])))
		, suite "array"
			[ test $ assertions "bytestring-strict" ($assert $ equal "Variant b\"\"" (show (toVariant Data.ByteString.empty)))
			, test $ assertions "bytestring-lazy" ($assert $ equal "Variant b\"\"" (show (toVariant Data.ByteString.Lazy.empty)))
			, test $ assertions "array-of-word8" ($assert $ equal "Variant b\"\"" (show (toVariant ([] :: [Word8]))))
			]
		, test $ assertions "dictionary" ($assert $ equal "(Variant {False: True, True: False})" (showsPrec 11 (toVariant (Data.Map.fromList [(True, False), (False, True)])) ""))
		, test $ assertions "structure" ($assert $ equal "(Variant (True, False))" (showsPrec 11 (toVariant (True, False)) ""))
		]
	
	, test $ assertions "compare-byte-storage" $ do
			$assert $ equal (toValue (Data.Vector.fromList [0 :: Word8]))
			            (toValue (Data.Vector.fromList [0 :: Word8]))
			$assert $ equal (toValue (Data.Vector.fromList [0 :: Word8]))
			            (toValue (Data.ByteString.pack [0]))
			$assert $ equal (toValue (Data.Vector.fromList [0 :: Word8]))
			            (toValue (Data.ByteString.Lazy.pack [0]))
			
			$assert $ equal (toValue (Data.ByteString.pack [0]))
			            (toValue (Data.Vector.fromList [0 :: Word8]))
			$assert $ equal (toValue (Data.ByteString.pack [0]))
			            (toValue (Data.ByteString.pack [0]))
			$assert $ equal (toValue (Data.ByteString.pack [0]))
			            (toValue (Data.ByteString.Lazy.pack [0]))
			
			$assert $ equal (toValue (Data.ByteString.Lazy.pack [0]))
			            (toValue (Data.Vector.fromList [0 :: Word8]))
			$assert $ equal (toValue (Data.ByteString.Lazy.pack [0]))
			            (toValue (Data.ByteString.pack [0]))
			$assert $ equal (toValue (Data.ByteString.Lazy.pack [0]))
			            (toValue (Data.ByteString.Lazy.pack [0]))
	]

suite_ContainerBoxes :: Suite
suite_ContainerBoxes = suite "container-boxes"
	[ suite "structure"
		[ test $ assertions "instance-of-Eq" ($assert $ equal (Structure []) (Structure []))
		, test $ assertions "instance-of-Show" ($assert $ equal "(True, False)" (show (Structure [toValue True, toValue False])))
		, test $ assertions "instance-of-IsVariant" (assertVariant (TypeStructure [TypeBoolean]) (Structure [toValue True]))
		]
	
	, suite "array"
		[ test $ assertions "instance-of-Eq" $ do
			$assert $ equal (Array TypeBoolean (Data.Vector.fromList [toValue True]))
			            (Array TypeBoolean (Data.Vector.fromList [toValue True]))
			$assert $ equal (Array TypeWord8 (Data.Vector.fromList [toValue (0 :: Word8)]))
			            (ArrayBytes (Data.ByteString.pack [0]))
		, test $ assertions "instance-of-Show" $ do
			$assert $ equal "[True, False]" (show (Array TypeBoolean (Data.Vector.fromList [toValue True, toValue False])))
			$assert $ equal "b\"\"" (show (Array TypeWord8 Data.Vector.empty))
			$assert $ equal "b\"\"" (show (ArrayBytes Data.ByteString.empty))
		, test $ assertions "instance-of-IsVariant" $ do
			assertVariant (TypeArray TypeWord8) (Array TypeWord8 Data.Vector.empty)
			assertVariant (TypeArray TypeWord8) (ArrayBytes Data.ByteString.empty)
		]
	
	, suite "dictionary"
		[ test $ assertions "instance-of-Eq" ($assert $ equal (Dictionary TypeWord8 TypeWord8 Data.Map.empty)
		                                                  (Dictionary TypeWord8 TypeWord8 Data.Map.empty))
		, test $ assertions "instance-of-Show" ($assert $ equal "{}" (show (Dictionary TypeWord8 TypeWord8 Data.Map.empty)))
		, test $ assertions "instance-of-IsVariant" (assertVariant (TypeDictionary TypeWord8 TypeWord8) (Dictionary TypeWord8 TypeWord8 Data.Map.empty))
		]
	]

suite_ObjectPath :: Suite
suite_ObjectPath = suite "object-path"
	[ suite "valid"
		[ test $ assertions "root" ($assert $ just (objectPath "/"))
		, test $ assertions "plain-1" ($assert $ just (objectPath "/foo"))
		, test $ assertions "plain-2" ($assert $ just (objectPath "/foo/bar"))
		, test $ assertions "start-with-digit" ($assert $ just (objectPath "/foo/0"))
		, test $ assertions "all-characters" ($assert $ just (objectPath "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
		]
	
	, suite "invalid"
		[ test $ assertions "empty" ($assert $ nothing (objectPath ""))
		, test $ assertions "bad-char" ($assert $ nothing (objectPath "/f!oo"))
		, test $ assertions "end-with-slash" ($assert $ nothing (objectPath "/foo/"))
		, test $ assertions "empty-element" ($assert $ nothing (objectPath "/foo//bar"))
		, test $ assertions "trailing-chars" ($assert $ nothing (objectPath "/foo!"))
		]
	]

suite_InterfaceName :: Suite
suite_InterfaceName = suite "interface-name"
	[ test $ assertions "instance-of-IsVariant" (assertVariant TypeString (interfaceName_ "foo.bar"))
	, suite "valid"
		[ test $ assertions "plain" ($assert $ just (interfaceName "foo.bar"))
		, test $ assertions "has-digit" ($assert $ just (interfaceName "foo.bar0"))
		, test $ assertions "all-characters" ($assert $ just (interfaceName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
		]
	
	, suite "invalid"
		[ test $ assertions "empty" ($assert $ nothing (interfaceName ""))
		, test $ assertions "one-element" ($assert $ nothing (interfaceName "foo"))
		, test $ assertions "start-with-digit" ($assert $ nothing (interfaceName "foo.0bar"))
		, test $ assertions "trailing-chars" ($assert $ nothing (interfaceName "foo.bar!"))
		]
	
	, suite "length"
		[ test $ assertions "length-254" ($assert $ just (interfaceName ("f." `T.append` T.replicate 252 "y")))
		, test $ assertions "length-255" ($assert $ just (interfaceName ("f." `T.append` T.replicate 253 "y")))
		, test $ assertions "length-256" ($assert $ nothing (interfaceName ("f." `T.append` T.replicate 254 "y")))
		]
	]

suite_MemberName :: Suite
suite_MemberName = suite "member-name"
	[ test $ assertions "instance-of-IsVariant" (assertVariant TypeString (memberName_ "foo"))
	, suite "valid"
		[ test $ assertions "plain" ($assert $ just (memberName "foo"))
		, test $ assertions "has-digit" ($assert $ just (memberName "foo0"))
		, test $ assertions "all-characters" ($assert $ just (memberName "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
		]
	
	, suite "invalid"
		[ test $ assertions "empty" ($assert $ nothing (memberName ""))
		, test $ assertions "start-with-digit" ($assert $ nothing (memberName "0foo"))
		, test $ assertions "trailing-chars" ($assert $ nothing (memberName "foo!"))
		]
	
	, suite "length"
		[ test $ assertions "length-254" ($assert $ just (memberName (T.replicate 254 "y")))
		, test $ assertions "length-255" ($assert $ just (memberName (T.replicate 255 "y")))
		, test $ assertions "length-256" ($assert $ nothing (memberName (T.replicate 256 "y")))
		]
	]

suite_ErrorName :: Suite
suite_ErrorName = suite "error-name"
	[ test $ assertions "instance-of-IsVariant" (assertVariant TypeString (errorName_ "foo.bar"))
	, suite "valid"
		[ test $ assertions "plain" ($assert $ just (errorName "foo.bar"))
		, test $ assertions "has-digit" ($assert $ just (errorName "foo.bar0"))
		, test $ assertions "all-characters" ($assert $ just (errorName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
		]
	
	, suite "invalid"
		[ test $ assertions "empty" ($assert $ nothing (errorName ""))
		, test $ assertions "one-element" ($assert $ nothing (errorName "foo"))
		, test $ assertions "start-with-digit" ($assert $ nothing (errorName "foo.0bar"))
		, test $ assertions "trailing-chars" ($assert $ nothing (errorName "foo.bar!"))
		]
	
	, suite "length"
		[ test $ assertions "length-254" ($assert $ just (errorName ("f." `T.append` T.replicate 252 "y")))
		, test $ assertions "length-255" ($assert $ just (errorName ("f." `T.append` T.replicate 253 "y")))
		, test $ assertions "length-256" ($assert $ nothing (errorName ("f." `T.append` T.replicate 254 "y")))
		]
	]

suite_BusName :: Suite
suite_BusName = suite "bus-name"
	[ test $ assertions "instance-of-IsVariant" (assertVariant TypeString (busName_ "foo.bar"))
	, suite "valid"
		[ suite "unique"
			[ test $ assertions "plain" ($assert $ just (busName ":foo.bar"))
			, test $ assertions "start-with-digit" ($assert $ just (busName ":foo.0bar"))
			, test $ assertions "all-characters" ($assert $ just (busName ":a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
			]
		, suite "well-known"
			[ test $ assertions "plain" ($assert $ just (busName "foo.bar"))
			, test $ assertions "has-digit" ($assert $ just (busName "foo.bar0"))
			, test $ assertions "all-characters" ($assert $ just (busName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
			]
		]
	
	, suite "invalid"
		[ test $ assertions "empty" ($assert $ nothing (busName ""))
		, test $ assertions "well-known-start-with-digit" ($assert $ nothing (busName "foo.0bar"))
		, test $ assertions "well-known-one-element" ($assert $ nothing (busName "foo"))
		, test $ assertions "unique-one-element" ($assert $ nothing (busName ":foo"))
		, test $ assertions "trailing-chars" ($assert $ nothing (busName "foo.bar!"))
		]
	
	, suite "length"
		[ test $ assertions "length-254" ($assert $ just (busName (":0." `T.append` T.replicate 251 "y")))
		, test $ assertions "length-255" ($assert $ just (busName (":0." `T.append` T.replicate 252 "y")))
		, test $ assertions "length-256" ($assert $ nothing (busName (":0." `T.append` T.replicate 253 "y")))
		]
	]

suite_Wire :: Suite
suite_Wire = suite "wire"
	[ property "value-passthrough" $ \v e ->
		let Right bytes = marshal e v in
		let Right unmarshaled = unmarshal e (valueType v) bytes in
		unmarshaled == v
	
	, suite "message-passthrough"
		[ property "method-call" $ forAll (arbitrary :: Gen MethodCall) $ \msg e s ->
			let Right bytes = marshalMessage e s msg in
			let Right received = unmarshalMessage bytes in
			ReceivedMethodCall s Nothing msg == received
		
		, property "method-return" $ forAll (arbitrary :: Gen MethodReturn) $ \msg e s ->
			let Right bytes = marshalMessage e s msg in
			let Right received = unmarshalMessage bytes in
			ReceivedMethodReturn s Nothing msg == received
		
		, property "error" $ forAll (arbitrary :: Gen Error) $ \msg e s ->
			let Right bytes = marshalMessage e s msg in
			let Right received = unmarshalMessage bytes in
			ReceivedError s Nothing msg == received
		
		, property "signal" $ forAll (arbitrary :: Gen Signal) $ \msg e s ->
			let Right bytes = marshalMessage e s msg in
			let Right received = unmarshalMessage bytes in
			ReceivedSignal s Nothing msg == received
		]
	]

suite_Introspection :: Suite
suite_Introspection = suite "introspection"
	[ property "xml-passthrough" $ \obj ->
		let (DBus.Introspection.Object path _ _) = obj in
		let Just xml = DBus.Introspection.toXML obj in
		DBus.Introspection.fromXML path xml == Just obj
	]

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

requireJust :: Maybe a -> Assertions a
requireJust (Just a) = return a
requireJust Nothing  = $fail "requireJust: got Nothing"

assertAtom :: (Eq a, Show a, IsAtom a) => Type -> a -> Assertions ()
assertAtom t a = do
	$expect $ equal t (atomType (toAtom a))
	$expect $ equal (fromAtom (toAtom a)) (Just a)
	$expect $ equal (toAtom a) (toAtom a)
	assertValue t a

assertValue :: (Eq a, Show a, IsValue a) => Type -> a -> Assertions ()
assertValue t a = do
	$expect $ equal t (DBus.Types.typeOf a)
	$expect $ equal t (DBus.Types.Internal.typeOf a)
	$expect $ equal t (valueType (toValue a))
	$expect $ equal (fromValue (toValue a)) (Just a)
	$expect $ equal (toValue a) (toValue a)
	assertVariant t a

assertVariant :: (Eq a, Show a, IsVariant a) => Type -> a -> Assertions ()
assertVariant t a = do
	$expect $ equal t (variantType (toVariant a))
	$expect $ equal (fromVariant (toVariant a)) (Just a)
	$expect $ equal (toVariant a) (toVariant a)

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
		<*> arbitrary
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
