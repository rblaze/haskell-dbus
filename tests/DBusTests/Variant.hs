{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2012 John Millikin <jmillikin@gmail.com>
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

module DBusTests.Variant (test_Variant) where

import           Prelude hiding (fail)

import           Test.Chell

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Text as T
import qualified Data.Text
import qualified Data.Text.Lazy
import           Data.Word (Word8, Word16, Word32, Word64)
import           Data.Int (Int16, Int32, Int64)
import qualified Data.Map
import qualified Data.Vector

import           DBus
import           DBus.Types (toValue)

import           DBusTests.Util

test_Variant :: Suite
test_Variant = suite "Variant"
	test_IsAtom
	test_IsValue
	test_Show
	test_ByteStorage

test_IsAtom :: Test
test_IsAtom = assertions "IsAtom" $ do
	assertAtom TypeBoolean True
	assertAtom TypeWord8 (0 :: Word8)
	assertAtom TypeWord16 (0 :: Word16)
	assertAtom TypeWord32 (0 :: Word32)
	assertAtom TypeWord64 (0 :: Word64)
	assertAtom TypeInt16 (0 :: Int16)
	assertAtom TypeInt32 (0 :: Int32)
	assertAtom TypeInt64 (0 :: Int64)
	assertAtom TypeDouble (0 :: Double)
	assertAtom TypeString (Data.Text.pack "")
	assertAtom TypeString (Data.Text.Lazy.pack "")
	assertAtom TypeString ("" :: String)
	assertAtom TypeObjectPath (objectPath_ "/")
	assertAtom TypeSignature (signature_ [])

test_IsValue :: Test
test_IsValue = assertions "IsValue" $ do
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

test_Show :: Test
test_Show = assertions "show" $ do
	$expect $ equal "Variant True" (show (toVariant True))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Word8)))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Word16)))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Word32)))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Word64)))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Int16)))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Int32)))
	$expect $ equal "Variant 0" (show (toVariant (0 :: Int64)))
	$expect $ equal "Variant 0.1" (show (toVariant (0.1 :: Double)))
	$expect $ equal "Variant \"\"" (show (toVariant (T.pack "")))
	$expect $ equal "Variant (ObjectPath \"/\")" (show (toVariant (objectPath_ "/")))
	$expect $ equal "Variant (Signature \"\")" (show (toVariant (signature_ [])))
	$expect $ equal "Variant (Variant True)" (show (toVariant (toVariant True)))
	$expect $ equal "Variant [True, False]" (show (toVariant [True, False]))
	
	$expect $ equal "Variant b\"\"" (show (toVariant Data.ByteString.empty))
	$expect $ equal "Variant b\"\"" (show (toVariant Data.ByteString.Lazy.empty))
	$expect $ equal "Variant b\"\"" (show (toVariant ([] :: [Word8])))
	
	$expect $ equal "(Variant {False: True, True: False})" (showsPrec 11 (toVariant (Data.Map.fromList [(True, False), (False, True)])) "")
	$expect $ equal "(Variant (True, False))" (showsPrec 11 (toVariant (True, False)) "")

test_ByteStorage :: Test
test_ByteStorage = assertions "byte-storage" $ do
	-- Vector Word8 -> Vector Word8
	$assert $ equal
		(toValue (Data.Vector.fromList [0 :: Word8]))
		(toValue (Data.Vector.fromList [0 :: Word8]))
	
	-- Vector Word8 -> ByteString
	$assert $ equal
		(toValue (Data.Vector.fromList [0 :: Word8]))
		(toValue (Data.ByteString.pack [0]))
	
	-- Vector Word8 -> Lazy.ByteString
	$assert $ equal
		(toValue (Data.Vector.fromList [0 :: Word8]))
		(toValue (Data.ByteString.Lazy.pack [0]))
	
	-- ByteString -> Vector Word8
	$assert $ equal
		(toValue (Data.ByteString.pack [0]))
		(toValue (Data.Vector.fromList [0 :: Word8]))
	-- ByteString -> ByteString
	$assert $ equal
		(toValue (Data.ByteString.pack [0]))
		(toValue (Data.ByteString.pack [0]))
	-- ByteString -> Lazy.ByteString
	$assert $ equal
		(toValue (Data.ByteString.pack [0]))
		(toValue (Data.ByteString.Lazy.pack [0]))
	
	-- Lazy.ByteString -> Vector Word8
	$assert $ equal
		(toValue (Data.ByteString.Lazy.pack [0]))
		(toValue (Data.Vector.fromList [0 :: Word8]))
	-- Lazy.ByteString -> ByteString
	$assert $ equal
		(toValue (Data.ByteString.Lazy.pack [0]))
		(toValue (Data.ByteString.pack [0]))
	-- Lazy.ByteString -> Lazy.ByteString
	$assert $ equal
		(toValue (Data.ByteString.Lazy.pack [0]))
		(toValue (Data.ByteString.Lazy.pack [0]))
