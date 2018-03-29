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

module DBusTests.Signature (test_Signature) where

import Data.Maybe
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DBus

import DBusTests.Util

test_Signature :: TestTree
test_Signature = testGroup "Signature"
    [ test_BuildSignature
    , test_ParseSignature
    , test_ParseInvalid
    , test_FormatSignature
    , test_IsAtom
    , test_ShowType
    ]

test_BuildSignature :: TestTree
test_BuildSignature = testProperty "signature" prop where
    prop = forAll gen_SignatureTypes check
    check types = case signature types of
        Nothing -> False
        Just sig -> signatureTypes sig == types

test_ParseSignature :: TestTree
test_ParseSignature = testProperty "parseSignature" prop where
    prop = forAll gen_SignatureString check
    check (s, types) = case parseSignature s of
        Nothing -> False
        Just sig -> signatureTypes sig == types

test_ParseInvalid :: TestTree
test_ParseInvalid = testCase "parse-invalid" $ do
    -- at most 255 characters
    assertBool "valid parse failed" $
        isJust (parseSignature (replicate 254 'y'))
    assertBool "valid parse failed" $
        isJust (parseSignature (replicate 255 'y'))
    Nothing @=? parseSignature (replicate 256 'y')

    -- length also enforced by 'signature'
    assertBool "valid parse failed" $
        isJust (signature (replicate 255 TypeWord8))
    Nothing @=? signature (replicate 256 TypeWord8)

    -- struct code
    Nothing @=? parseSignature "r"

    -- empty struct
    Nothing @=? parseSignature "()"
    Nothing @=? signature [TypeStructure []]

    -- dict code
    Nothing @=? parseSignature "e"

    -- non-atomic dict key
    Nothing @=? parseSignature "a{vy}"
    Nothing @=? signature [TypeDictionary TypeVariant TypeVariant]

test_FormatSignature :: TestTree
test_FormatSignature = testProperty "formatSignature" prop where
    prop = forAll gen_SignatureString check
    check (s, _) = let
        Just sig = parseSignature s
        in formatSignature sig == s

test_IsAtom :: TestTree
test_IsAtom = testCase "IsAtom" $ do
    let Just sig = signature []
    assertAtom TypeSignature sig

test_ShowType :: TestTree
test_ShowType = testCase "show-type" $ do
    "Bool" @=? show TypeBoolean
    "Bool" @=? show TypeBoolean
    "Word8" @=? show TypeWord8
    "Word16" @=? show TypeWord16
    "Word32" @=? show TypeWord32
    "Word64" @=? show TypeWord64
    "Int16" @=? show TypeInt16
    "Int32" @=? show TypeInt32
    "Int64" @=? show TypeInt64
    "Double" @=? show TypeDouble
    "UnixFd" @=? show TypeUnixFd
    "String" @=? show TypeString
    "Signature" @=? show TypeSignature
    "ObjectPath" @=? show TypeObjectPath
    "Variant" @=? show TypeVariant
    "[Word8]" @=? show (TypeArray TypeWord8)
    "Dict Word8 (Dict Word8 Word8)" @=? show (TypeDictionary TypeWord8 (TypeDictionary TypeWord8 TypeWord8))
    "(Word8, Word16)" @=? show (TypeStructure [TypeWord8, TypeWord16])

gen_SignatureTypes :: Gen [Type]
gen_SignatureTypes = do
    (_, ts) <- gen_SignatureString
    return ts

gen_SignatureString :: Gen (String, [Type])
gen_SignatureString = gen where
    anyType = oneof [atom, container]
    atom = elements
        [ ("b", TypeBoolean)
        , ("y", TypeWord8)
        , ("q", TypeWord16)
        , ("u", TypeWord32)
        , ("t", TypeWord64)
        , ("n", TypeInt16)
        , ("i", TypeInt32)
        , ("x", TypeInt64)
        , ("d", TypeDouble)
        , ("h", TypeUnixFd)
        , ("s", TypeString)
        , ("o", TypeObjectPath)
        , ("g", TypeSignature)
        ]
    container = oneof
        [ return ("v", TypeVariant)
        , array
        , dict
        , struct
        ]
    array = do
        (tCode, tEnum) <- anyType
        return ('a':tCode, TypeArray tEnum)
    dict = do
        (kCode, kEnum) <- atom
        (vCode, vEnum) <- anyType
        return (concat ["a{", kCode, vCode, "}"], TypeDictionary kEnum vEnum)
    struct = do
        ts <- listOf1 (halfSized anyType)
        let (codes, enums) = unzip ts
        return ("(" ++ concat codes ++ ")", TypeStructure enums)
    gen = do
        types <- listOf anyType
        let (codes, enums) = unzip types
        let chars = concat codes
        if length chars > 255
            then halfSized gen
            else return (chars, enums)

instance Arbitrary Signature where
    arbitrary = do
        ts <- gen_SignatureTypes
        let Just sig = signature ts
        return sig
