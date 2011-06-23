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

import qualified Test.Framework as F
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit (Assertion, assertFailure, assertEqual)
import           Test.QuickCheck

import qualified Control.Exception
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
import           DBus.Wire ()
import           DBus.Introspection ()

tests :: [F.Test]
tests = [ test_Address
	, test_Signature
	, test_Types
	, test_Variant
	, test_ObjectPath
	, test_InterfaceName
	, test_MemberName
	, test_ErrorName
	, test_BusName
	]

main :: IO ()
main = F.defaultMain tests

test_Address :: F.Test
test_Address = F.testGroup "address"
	[ F.testGroup "valid"
	  [ testCase "colon" (assertJust (address ":"))
	  , testCase "just-scheme" (assertJust (address "a:"))
	  , testCase "param" (assertJust (address "a:b=c"))
	  , testCase "trailing-semicolon" (assertJust (addresses "a:;"))
	  , testCase "two-schemes" (assertJust (addresses "a:;b:"))
	  , testCase "trailing-comma" (assertJust (address "a:b=c,"))
	  , testCase "encoded" (assertEqual "" (address "a:b=g8") (address "a:b=%678"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (address ""))
	  , testCase "no-colon" (assertNothing (address "a"))
	  , testCase "no-equals" (assertNothing (address "a:b"))
	  , testCase "no-param" (assertNothing (address "a:,"))
	  , testCase "no-param-value" (assertNothing (address "a:b="))
	  ]
	, F.testGroup "passthrough"
	  [ testCase "plain" (assertEqual "" (Just "a:b=c") (addressText `fmap` address "a:b=c"))
	  , testCase "encoded" (assertEqual "" (Just "a:b=Z%5B") (addressText `fmap` address "a:b=%5a%5b"))
	  , testCase "optionally-encoded" (assertEqual "" (Just "a:b=-_/\\*.") (addressText `fmap` address "a:b=-_/\\*."))
	  , testCase "multiple-params" (assertEqual "" (Just "a:b=c,d=e") (addressText `fmap` address "a:b=c,d=e"))
	  ]
	, F.testGroup "instances"
	  [ testCase "eq" (assertEqual "" (address "a:b=c") (address "a:b=c"))
	  , testCase "show" (assertEqual "" "(Address \"a:b=c\")" (showsPrec 11 (fromJust (address "a:b=c")) ""))
	  ]
	, F.testGroup "well-known"
	  [ testCase "system" (withEnv "DBUS_SYSTEM_BUS_ADDRESS" (Just "a:b=c;d:") (do
	    	addrs <- getSystem
	    	assertEqual "" addrs (Just ["a:b=c", "d:"])))
	  , testCase "default-system" (withEnv "DBUS_SYSTEM_BUS_ADDRESS" Nothing (do
	    	addrs <- getSystem
	    	assertEqual "" addrs (Just ["unix:path=/var/run/dbus/system_bus_socket"])))
	  , testCase "session" (withEnv "DBUS_SESSION_BUS_ADDRESS" (Just "a:b=c;d:") (do
	    	addrs <- getSession
	    	assertEqual "" addrs (Just ["a:b=c", "d:"])))
	  , testCase "starter" (withEnv "DBUS_STARTER_BUS_ADDRESS" (Just "a:b=c;d:") (do
	    	addrs <- getStarter
	    	assertEqual "" addrs (Just ["a:b=c", "d:"])))
	  ]
	]

test_Signature :: F.Test
test_Signature = F.testGroup "signature"
	[ F.testGroup "valid"
	  [ F.testGroup "atom"
	    [ testCase "bool" (assertJust (signature "b"))
	    , testCase "word8" (assertJust (signature "y"))
	    , testCase "word16" (assertJust (signature "q"))
	    , testCase "word32" (assertJust (signature "u"))
	    , testCase "word64" (assertJust (signature "t"))
	    , testCase "int16" (assertJust (signature "x"))
	    , testCase "int32" (assertJust (signature "i"))
	    , testCase "int64" (assertJust (signature "x"))
	    , testCase "double" (assertJust (signature "d"))
	    , testCase "string" (assertJust (signature "s"))
	    , testCase "object-path" (assertJust (signature "o"))
	    , testCase "signature" (assertJust (signature "g"))
	    ]
	  , F.testGroup "container"
	    [ testCase "variant" (assertJust (signature "v"))
	    , testCase "array" (assertJust (signature "ay"))
	    , testCase "struct" (assertJust (signature "(yy)"))
	    , testCase "dictionary" (assertJust (signature "a{yy}"))
	    ]
	  , testCase "empty" (assertJust (signature ""))
	  ]
	, F.testGroup "invalid"
	  [ testCase "struct-code" (assertNothing (signature "r"))
	  , testCase "struct-empty" (assertNothing (signature "()"))
	  , testCase "dict-code" (assertNothing (signature "e"))
	  , testCase "dict-container-key" (assertNothing (signature "a{vy}"))
	  , testCase "unix-fd" (assertNothing (signature "h"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (signature (T.replicate 254 "y")))
	  , testCase "length-255" (assertJust (signature (T.replicate 255 "y")))
	  , testCase "length-256" (assertNothing (signature (T.replicate 256 "y")))
	  ]
	, F.testGroup "instances"
	  [ testCase "show" (assertEqual "" "(Signature \"y\")" (showsPrec 11 (fromJust (signature "y")) ""))
	  ]
	]

test_Types :: F.Test
test_Types = F.testGroup "types"
	[ F.testGroup "instances"
	  [ testCase "eq" (assertEqual "" TypeWord8 TypeWord8)
	  , F.testGroup "show"
	    [ testCase "Boolean" (assertEqual "" "Bool" (show TypeBoolean))
	    , testCase "Word8" (assertEqual "" "Word8" (show TypeWord8))
	    , testCase "Word16" (assertEqual "" "Word16" (show TypeWord16))
	    , testCase "Word32" (assertEqual "" "Word32" (show TypeWord32))
	    , testCase "Word64" (assertEqual "" "Word64" (show TypeWord64))
	    , testCase "Int16" (assertEqual "" "Int16" (show TypeInt16))
	    , testCase "Int32" (assertEqual "" "Int32" (show TypeInt32))
	    , testCase "Int64" (assertEqual "" "Int64" (show TypeInt64))
	    , testCase "Double" (assertEqual "" "Double" (show TypeDouble))
	    , testCase "String" (assertEqual "" "String" (show TypeString))
	    , testCase "Signature" (assertEqual "" "Signature" (show TypeSignature))
	    , testCase "ObjectPath" (assertEqual "" "ObjectPath" (show TypeObjectPath))
	    , testCase "Variant" (assertEqual "" "Variant" (show TypeVariant))
	    , testCase "Array" (assertEqual "" "[Word8]" (show (TypeArray TypeWord8)))
	    , testCase "Dictionary" (assertEqual "" "Map Word8 (Map Word8 Word8)" (show (TypeDictionary TypeWord8 (TypeDictionary TypeWord8 TypeWord8))))
	    , testCase "Structure" (assertEqual "" "(Word8, Word16)" (show (TypeStructure [TypeWord8, TypeWord16])))
	    ]
	  ]
	]

test_Variant :: F.Test
test_Variant = F.testGroup "variant"
	[ F.testGroup "instances"
	  [ F.testGroup "show"
	    [ testCase "array" (assertEqual "" "(Variant [True, False, True])" (showsPrec 11 (toVariant [True, False, True]) ""))
	    , testCase "dictionary" (assertEqual "" "(Variant {False: True, True: False})" (showsPrec 11 (toVariant (Data.Map.fromList [(True, False), (False, True)])) ""))
	    , testCase "structure" (assertEqual "" "(Variant (True, False))" (showsPrec 11 (toVariant (True, False)) ""))
	    ]
	  ]
	]

test_ObjectPath :: F.Test
test_ObjectPath = F.testGroup "object-path"
	[ F.testGroup "valid"
	  [ testCase "root" (assertJust (objectPath "/"))
	  , testCase "plain-1" (assertJust (objectPath "/foo"))
	  , testCase "plain-2" (assertJust (objectPath "/foo/bar"))
	  , testCase "start-with-digit" (assertJust (objectPath "/foo/0"))
	  , testCase "all-characters" (assertJust (objectPath "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (objectPath ""))
	  , testCase "bad-char" (assertNothing (objectPath "/f!oo"))
	  , testCase "end-with-slash" (assertNothing (objectPath "/foo/"))
	  , testCase "empty-element" (assertNothing (objectPath "/foo//bar"))
	  , testCase "trailing-chars" (assertNothing (objectPath "/foo!"))
	  ]
	]

test_InterfaceName :: F.Test
test_InterfaceName = F.testGroup "interface-name"
	[ F.testGroup "valid"
	  [ testCase "plain" (assertJust (interfaceName "foo.bar"))
	  , testCase "has-digit" (assertJust (interfaceName "foo.bar0"))
	  , testCase "all-characters" (assertJust (interfaceName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (interfaceName ""))
	  , testCase "one-element" (assertNothing (interfaceName "foo"))
	  , testCase "start-with-digit" (assertNothing (interfaceName "foo.0bar"))
	  , testCase "trailing-chars" (assertNothing (interfaceName "foo.bar!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (interfaceName ("f." `T.append` T.replicate 252 "y")))
	  , testCase "length-255" (assertJust (interfaceName ("f." `T.append` T.replicate 253 "y")))
	  , testCase "length-256" (assertNothing (interfaceName ("f." `T.append` T.replicate 254 "y")))
	  ]
	]

test_MemberName :: F.Test
test_MemberName = F.testGroup "member-name"
	[ F.testGroup "valid"
	  [ testCase "plain" (assertJust (memberName "foo"))
	  , testCase "has-digit" (assertJust (memberName "foo0"))
	  , testCase "all-characters" (assertJust (memberName "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (memberName ""))
	  , testCase "start-with-digit" (assertNothing (memberName "0foo"))
	  , testCase "trailing-chars" (assertNothing (memberName "foo!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (memberName (T.replicate 254 "y")))
	  , testCase "length-255" (assertJust (memberName (T.replicate 255 "y")))
	  , testCase "length-256" (assertNothing (memberName (T.replicate 256 "y")))
	  ]
	]

test_ErrorName :: F.Test
test_ErrorName = F.testGroup "error-name"
	[ F.testGroup "valid"
	  [ testCase "plain" (assertJust (errorName "foo.bar"))
	  , testCase "has-digit" (assertJust (errorName "foo.bar0"))
	  , testCase "all-characters" (assertJust (errorName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (errorName ""))
	  , testCase "one-element" (assertNothing (errorName "foo"))
	  , testCase "start-with-digit" (assertNothing (errorName "foo.0bar"))
	  , testCase "trailing-chars" (assertNothing (errorName "foo.bar!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (errorName ("f." `T.append` T.replicate 252 "y")))
	  , testCase "length-255" (assertJust (errorName ("f." `T.append` T.replicate 253 "y")))
	  , testCase "length-256" (assertNothing (errorName ("f." `T.append` T.replicate 254 "y")))
	  ]
	]

test_BusName :: F.Test
test_BusName = F.testGroup "bus-name"
	[ F.testGroup "valid"
	  [ F.testGroup "unique"
	    [ testCase "plain" (assertJust (busName ":foo.bar"))
	    , testCase "start-with-digit" (assertJust (busName ":foo.0bar"))
	    , testCase "all-characters" (assertJust (busName ":a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	    ]
	  , F.testGroup "well-known"
	    [ testCase "plain" (assertJust (busName "foo.bar"))
	    , testCase "has-digit" (assertJust (busName "foo.bar0"))
	    , testCase "all-characters" (assertJust (busName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	    ]
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (busName ""))
	  , testCase "well-known-start-with-digit" (assertNothing (busName "foo.0bar"))
	  , testCase "well-known-one-element" (assertNothing (busName "foo"))
	  , testCase "unique-one-element" (assertNothing (busName ":foo"))
	  , testCase "trailing-chars" (assertNothing (busName "foo.bar!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (busName (":0." `T.append` T.replicate 251 "y")))
	  , testCase "length-255" (assertJust (busName (":0." `T.append` T.replicate 252 "y")))
	  , testCase "length-256" (assertNothing (busName (":0." `T.append` T.replicate 253 "y")))
	  ]
	]

assertJust :: Maybe a -> Assertion
assertJust (Just _) = return ()
assertJust Nothing  = assertFailure "expected: (Just _)\n but got: Nothing"

assertNothing :: Maybe a -> Assertion
assertNothing Nothing = return ()
assertNothing (Just _)  = assertFailure "expected: (Just _)\n but got: Nothing"

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
