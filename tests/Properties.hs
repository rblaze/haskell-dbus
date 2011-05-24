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

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Int (Int16, Int32, Int64)
import           Data.Maybe (isJust, isNothing)

import           DBus.Address
import           DBus.Message
import           DBus.Types
import           DBus.Wire
import qualified DBus.Introspection as I

tests :: [F.Test]
tests = [ test_Address
	, test_Signature
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
	  [ testCase "colon" (assertJust (mkAddresses ":"))
	  , testCase "just-scheme" (assertJust (mkAddresses "a:"))
	  , testCase "param" (assertJust (mkAddresses "a:b=c"))
	  , testCase "trailing-semicolon" (assertJust (mkAddresses "a:;"))
	  , testCase "two-schemes" (assertJust (mkAddresses "a:;b:"))
	  , testCase "trailing-comma" (assertJust (mkAddresses "a:b=c,"))
	  , testCase "encoded" (assertEqual "" (mkAddresses "a:b=g8") (mkAddresses "a:b=%678"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (mkAddresses ""))
	  , testCase "no-colon" (assertNothing (mkAddresses "a"))
	  , testCase "no-equals" (assertNothing (mkAddresses "a:b"))
	  , testCase "no-param" (assertNothing (mkAddresses "a:,"))
	  , testCase "no-param-value" (assertNothing (mkAddresses "a:b="))
	  ]
	, F.testGroup "passthrough"
	  [ testCase "plain" (assertEqual "" (Just "a:b=c") (strAddress `fmap` head `fmap` mkAddresses "a:b=c"))
	  , testCase "encoded" (assertEqual "" (Just "a:b=Z%5B") (strAddress `fmap` head `fmap` mkAddresses "a:b=%5a%5b"))
	  , testCase "optionally-encoded" (assertEqual "" (Just "a:b=-_/\\*.") (strAddress `fmap` head `fmap` mkAddresses "a:b=-_/\\*."))
	  ]
	]

test_Signature :: F.Test
test_Signature = F.testGroup "signature"
	[ F.testGroup "valid"
	  [ F.testGroup "atom"
	    [ testCase "bool" (assertJust (mkSignature "b"))
	    , testCase "word8" (assertJust (mkSignature "y"))
	    , testCase "word16" (assertJust (mkSignature "q"))
	    , testCase "word32" (assertJust (mkSignature "u"))
	    , testCase "word64" (assertJust (mkSignature "t"))
	    , testCase "int16" (assertJust (mkSignature "x"))
	    , testCase "int32" (assertJust (mkSignature "i"))
	    , testCase "int64" (assertJust (mkSignature "x"))
	    , testCase "double" (assertJust (mkSignature "d"))
	    , testCase "string" (assertJust (mkSignature "s"))
	    , testCase "object-path" (assertJust (mkSignature "o"))
	    , testCase "signature" (assertJust (mkSignature "g"))
	    ]
	  , F.testGroup "container"
	    [ testCase "variant" (assertJust (mkSignature "v"))
	    , testCase "array" (assertJust (mkSignature "ay"))
	    , testCase "struct" (assertJust (mkSignature "(yy)"))
	    , testCase "dictionary" (assertJust (mkSignature "a{yy}"))
	    ]
	  , testCase "empty" (assertJust (mkSignature ""))
	  ]
	, F.testGroup "invalid"
	  [ testCase "struct-code" (assertNothing (mkSignature "r"))
	  , testCase "struct-empty" (assertNothing (mkSignature "()"))
	  , testCase "dict-code" (assertNothing (mkSignature "e"))
	  , testCase "dict-container-key" (assertNothing (mkSignature "a{vy}"))
	  , testCase "unix-fd" (assertNothing (mkSignature "h"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (mkSignature (TL.replicate 254 "y")))
	  , testCase "length-255" (assertJust (mkSignature (TL.replicate 255 "y")))
	  , testCase "length-256" (assertNothing (mkSignature (TL.replicate 256 "y")))
	  ]
	]

test_ObjectPath :: F.Test
test_ObjectPath = F.testGroup "object-path"
	[ F.testGroup "valid"
	  [ testCase "root" (assertJust (mkObjectPath "/"))
	  , testCase "plain-1" (assertJust (mkObjectPath "/foo"))
	  , testCase "plain-2" (assertJust (mkObjectPath "/foo/bar"))
	  , testCase "start-with-digit" (assertJust (mkObjectPath "/foo/0"))
	  , testCase "all-characters" (assertJust (mkObjectPath "/abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (mkObjectPath ""))
	  , testCase "bad-char" (assertNothing (mkObjectPath "/f!oo"))
	  , testCase "end-with-slash" (assertNothing (mkObjectPath "/foo/"))
	  , testCase "empty-element" (assertNothing (mkObjectPath "/foo//bar"))
	  , testCase "trailing-chars" (assertNothing (mkObjectPath "/foo!"))
	  ]
	]

test_InterfaceName :: F.Test
test_InterfaceName = F.testGroup "interface-name"
	[ F.testGroup "valid"
	  [ testCase "plain" (assertJust (mkInterfaceName "foo.bar"))
	  , testCase "has-digit" (assertJust (mkInterfaceName "foo.bar0"))
	  , testCase "all-characters" (assertJust (mkInterfaceName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (mkInterfaceName ""))
	  , testCase "one-element" (assertNothing (mkInterfaceName "foo"))
	  , testCase "start-with-digit" (assertNothing (mkInterfaceName "foo.0bar"))
	  , testCase "trailing-chars" (assertNothing (mkInterfaceName "foo.bar!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (mkInterfaceName ("f." `TL.append` TL.replicate 252 "y")))
	  , testCase "length-255" (assertJust (mkInterfaceName ("f." `TL.append` TL.replicate 253 "y")))
	  , testCase "length-256" (assertNothing (mkInterfaceName ("f." `TL.append` TL.replicate 254 "y")))
	  ]
	]

test_MemberName :: F.Test
test_MemberName = F.testGroup "member-name"
	[ F.testGroup "valid"
	  [ testCase "plain" (assertJust (mkMemberName "foo"))
	  , testCase "has-digit" (assertJust (mkMemberName "foo0"))
	  , testCase "all-characters" (assertJust (mkMemberName "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (mkMemberName ""))
	  , testCase "start-with-digit" (assertNothing (mkMemberName "0foo"))
	  , testCase "trailing-chars" (assertNothing (mkMemberName "foo!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (mkMemberName (TL.replicate 254 "y")))
	  , testCase "length-255" (assertJust (mkMemberName (TL.replicate 255 "y")))
	  , testCase "length-256" (assertNothing (mkMemberName (TL.replicate 256 "y")))
	  ]
	]

test_ErrorName :: F.Test
test_ErrorName = F.testGroup "error-name"
	[ F.testGroup "valid"
	  [ testCase "plain" (assertJust (mkErrorName "foo.bar"))
	  , testCase "has-digit" (assertJust (mkErrorName "foo.bar0"))
	  , testCase "all-characters" (assertJust (mkErrorName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (mkErrorName ""))
	  , testCase "one-element" (assertNothing (mkErrorName "foo"))
	  , testCase "start-with-digit" (assertNothing (mkErrorName "foo.0bar"))
	  , testCase "trailing-chars" (assertNothing (mkErrorName "foo.bar!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (mkErrorName ("f." `TL.append` TL.replicate 252 "y")))
	  , testCase "length-255" (assertJust (mkErrorName ("f." `TL.append` TL.replicate 253 "y")))
	  , testCase "length-256" (assertNothing (mkErrorName ("f." `TL.append` TL.replicate 254 "y")))
	  ]
	]

test_BusName :: F.Test
test_BusName = F.testGroup "bus-name"
	[ F.testGroup "valid"
	  [ F.testGroup "unique"
	    [ testCase "plain" (assertJust (mkBusName ":foo.bar"))
	    , testCase "start-with-digit" (assertJust (mkBusName ":foo.0bar"))
	    , testCase "all-characters" (assertJust (mkBusName ":a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	    ]
	  , F.testGroup "well-known"
	    [ testCase "plain" (assertJust (mkBusName "foo.bar"))
	    , testCase "has-digit" (assertJust (mkBusName "foo.bar0"))
	    , testCase "all-characters" (assertJust (mkBusName "a.abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"))
	    ]
	  ]
	, F.testGroup "invalid"
	  [ testCase "empty" (assertNothing (mkBusName ""))
	  , testCase "well-known-start-with-digit" (assertNothing (mkBusName "foo.0bar"))
	  , testCase "well-known-one-element" (assertNothing (mkBusName "foo"))
	  , testCase "unique-one-element" (assertNothing (mkBusName ":foo"))
	  , testCase "trailing-chars" (assertNothing (mkBusName "foo.bar!"))
	  ]
	, F.testGroup "length"
	  [ testCase "length-254"  (assertJust (mkBusName (":0." `TL.append` TL.replicate 251 "y")))
	  , testCase "length-255" (assertJust (mkBusName (":0." `TL.append` TL.replicate 252 "y")))
	  , testCase "length-256" (assertNothing (mkBusName (":0." `TL.append` TL.replicate 253 "y")))
	  ]
	]

assertJust :: Maybe a -> Assertion
assertJust (Just _) = return ()
assertJust Nothing  = assertFailure "expected: (Just _)\n but got: Nothing"

assertNothing :: Maybe a -> Assertion
assertNothing Nothing = return ()
assertNothing (Just _)  = assertFailure "expected: (Just _)\n but got: Nothing"
