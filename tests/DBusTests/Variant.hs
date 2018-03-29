{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module DBusTests.Variant (test_Variant) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import System.Posix.Types (Fd)
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Map
import qualified Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy
import qualified Data.Vector

import DBus
import DBus.Internal.Types (toValue)

import DBusTests.Util

test_Variant :: TestTree
test_Variant = testGroup "Variant"
    [ test_IsAtom
    , test_IsValue
    , test_Show
    , test_ByteStorage
    ]

test_IsAtom :: TestTree
test_IsAtom = testCase "IsAtom" $ do
    assertAtom TypeBoolean True
    assertAtom TypeWord8 (0 :: Word8)
    assertAtom TypeWord16 (0 :: Word16)
    assertAtom TypeWord32 (0 :: Word32)
    assertAtom TypeWord64 (0 :: Word64)
    assertAtom TypeInt16 (0 :: Int16)
    assertAtom TypeInt32 (0 :: Int32)
    assertAtom TypeInt64 (0 :: Int64)
    assertAtom TypeDouble (0 :: Double)
    assertAtom TypeUnixFd (0 :: Fd)
    assertAtom TypeString (Data.Text.pack "")
    assertAtom TypeString (Data.Text.Lazy.pack "")
    assertAtom TypeString ("" :: String)
    assertAtom TypeObjectPath (objectPath_ "/")
    assertAtom TypeSignature (signature_ [])

test_IsValue :: TestTree
test_IsValue = testCase "IsValue" $ do
    assertValue TypeVariant (toVariant True)
    assertValue (TypeArray TypeBoolean) [True]
    assertValue (TypeArray TypeBoolean) (Data.Vector.fromList [True])
    assertValue (TypeArray TypeWord8) Data.ByteString.empty
    assertValue (TypeArray TypeWord8) Data.ByteString.Lazy.empty
    assertValue (TypeDictionary TypeBoolean TypeBoolean) (Data.Map.fromList [(True, True)])
    assertValue (TypeStructure (replicate 2 TypeBoolean)) (True, True)
    assertValue (TypeStructure (replicate 3 TypeBoolean)) (True, True, True)
    assertValue (TypeStructure (replicate 4 TypeBoolean)) (True, True, True, True)
    assertValue (TypeStructure (replicate 5 TypeBoolean)) (True, True, True, True, True)
    assertValue (TypeStructure (replicate 6 TypeBoolean)) (True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 7 TypeBoolean)) (True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 8 TypeBoolean)) (True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 9 TypeBoolean)) (True, True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 10 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 11 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 12 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 13 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 14 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True, True)
    assertValue (TypeStructure (replicate 15 TypeBoolean)) (True, True, True, True, True, True, True, True, True, True, True, True, True, True, True)

test_Show :: TestTree
test_Show = testCase "show" $ do
    "Variant True" @=? show (toVariant True)
    "Variant 0" @=? show (toVariant (0 :: Word8))
    "Variant 0" @=? show (toVariant (0 :: Word16))
    "Variant 0" @=? show (toVariant (0 :: Word32))
    "Variant 0" @=? show (toVariant (0 :: Word64))
    "Variant 0" @=? show (toVariant (0 :: Int16))
    "Variant 0" @=? show (toVariant (0 :: Int32))
    "Variant 0" @=? show (toVariant (0 :: Int64))
    "Variant 0.1" @=? show (toVariant (0.1 :: Double))
    "Variant (UnixFd 1)" @=? show (toVariant (1 :: Fd))
    "Variant \"\"" @=? show (toVariant (T.pack ""))
    "Variant (ObjectPath \"/\")" @=? show (toVariant (objectPath_ "/"))
    "Variant (Signature \"\")" @=? show (toVariant (signature_ []))
    "Variant (Variant True)" @=? show (toVariant (toVariant True))
    "Variant [True, False]" @=? show (toVariant [True, False])

    "Variant b\"\"" @=? show (toVariant Data.ByteString.empty)
    "Variant b\"\"" @=? show (toVariant Data.ByteString.Lazy.empty)
    "Variant b\"\"" @=? show (toVariant ([] :: [Word8]))

    "(Variant {False: True, True: False})" @=? showsPrec 11 (toVariant (Data.Map.fromList [(True, False), (False, True)])) ""
    "(Variant (True, False))" @=? showsPrec 11 (toVariant (True, False)) ""

test_ByteStorage :: TestTree
test_ByteStorage = testCase "byte-storage" $ do
    -- Vector Word8 -> Vector Word8
    toValue (Data.Vector.fromList [0 :: Word8])
        @=? toValue (Data.Vector.fromList [0 :: Word8])

    -- Vector Word8 -> ByteString
    toValue (Data.Vector.fromList [0 :: Word8])
        @=? toValue (Data.ByteString.pack [0])

    -- Vector Word8 -> Lazy.ByteString
    toValue (Data.Vector.fromList [0 :: Word8])
        @=? toValue (Data.ByteString.Lazy.pack [0])

    -- ByteString -> Vector Word8
    toValue (Data.ByteString.pack [0])
        @=? toValue (Data.Vector.fromList [0 :: Word8])
    -- ByteString -> ByteString
    toValue (Data.ByteString.pack [0])
        @=? toValue (Data.ByteString.pack [0])
    -- ByteString -> Lazy.ByteString
    toValue (Data.ByteString.pack [0])
        @=? toValue (Data.ByteString.Lazy.pack [0])

    -- Lazy.ByteString -> Vector Word8
    toValue (Data.ByteString.Lazy.pack [0])
        @=? toValue (Data.Vector.fromList [0 :: Word8])
    -- Lazy.ByteString -> ByteString
    toValue (Data.ByteString.Lazy.pack [0])
        @=? toValue (Data.ByteString.pack [0])
    -- Lazy.ByteString -> Lazy.ByteString
    toValue (Data.ByteString.Lazy.pack [0])
        @=? toValue (Data.ByteString.Lazy.pack [0])
