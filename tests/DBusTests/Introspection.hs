{-# LANGUAGE OverloadedStrings #-}
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

module DBusTests.Introspection (test_Introspection) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DBus
import qualified DBus.Introspection as I

import DBusTests.InterfaceName ()
import DBusTests.MemberName ()
import DBusTests.ObjectPath ()
import DBusTests.Signature ()
import DBusTests.Util (halfSized)

test_Introspection :: TestTree
test_Introspection = testGroup "Introspection"
    [ test_XmlPassthrough
    , test_XmlParse
    , test_XmlParseFailed
    , test_XmlWriteFailed
    ]

test_XmlPassthrough :: TestTree
test_XmlPassthrough = testProperty "xml-passthrough" $ \obj -> let
    path = I.objectPath obj
    Just xml = I.formatXML obj
    in I.parseXML path xml == Just obj

buildEmptyObject :: String -> I.Object
buildEmptyObject name = I.Object (objectPath_ name) [] []

test_XmlParse :: TestTree
test_XmlParse = testCase "xml-parse" $ do
    -- root object path can be inferred
    I.parseXML (objectPath_ "/") "<node><node name='foo'/></node>"
        @?= Just (buildEmptyObject "/")
            { I.objectChildren =
                [ buildEmptyObject "/foo"
                ]
            }

test_XmlParseFailed :: TestTree
test_XmlParseFailed = testCase "xml-parse-failed" $ do
    Nothing @=? I.parseXML (objectPath_ "/") "<invalid>"
    Nothing @=? I.parseXML (objectPath_ "/") "<invalid/>"

    -- invalid property access
    Nothing @=? I.parseXML (objectPath_ "/")
        "<node>\
        \  <interface name='com.example.Foo'>\
        \    <property type='s' access='invalid'>\
        \    </property>\
        \  </interface>\
        \</node>"

    -- invalid parameter type
    Nothing @=? I.parseXML (objectPath_ "/")
        "<node>\
        \  <interface name='com.example.Foo'>\
        \    <method name='Foo'>\
        \      <arg type='yy'/>\
        \    </method>\
        \  </interface>\
        \</node>"

test_XmlWriteFailed :: TestTree
test_XmlWriteFailed = testCase "xml-write-failed" $ do
    -- child's object path isn't under parent's
    Nothing @=? I.formatXML (buildEmptyObject "/foo")
        { I.objectChildren =
            [ buildEmptyObject "/bar" ]
        }

    -- invalid type
    Nothing @=? I.formatXML
            ((buildEmptyObject "/foo")
            { I.objectInterfaces =
                [ I.Interface (interfaceName_ "/bar") [] []
                                [ I.Property "prop" (TypeDictionary TypeVariant TypeVariant) True True ]]})

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

instance Arbitrary I.Object where
    arbitrary = arbitrary >>= subObject

subObject :: ObjectPath -> Gen I.Object
subObject parentPath = sized $ \n -> resize (min n 4) $ do
    let nonRoot = do
        x <- resize 10 arbitrary
        case formatObjectPath x of
            "/" -> nonRoot
            x'  -> return x'

    thisPath <- nonRoot
    let path' = case formatObjectPath parentPath of
            "/" -> thisPath
            x   -> x ++ thisPath
    let path = objectPath_ path'
    ifaces <- arbitrary
    children <- halfSized (listOf (subObject path))
    return $ I.Object path ifaces children

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
        args <- arbitrary
        return $ (I.Method name args)

instance Arbitrary I.Signal where
    arbitrary = do
        name <- arbitrary
        args <- arbitrary
        return $ I.Signal name args

instance Arbitrary I.MethodArg where
    arbitrary = I.MethodArg
        <$> gen_Ascii
        <*> arbitrary
        <*> arbitrary

instance Arbitrary I.Direction where
    arbitrary = elements [I.In, I.Out]

instance Arbitrary I.SignalArg where
    arbitrary = I.SignalArg
        <$> gen_Ascii
        <*> arbitrary

instance Arbitrary I.Property where
    arbitrary = do
        name <- gen_Ascii
        t <- arbitrary
        canRead <- arbitrary
        canWrite <- arbitrary
        return I.Property
            { I.propertyName = name
            , I.propertyType = t
            , I.propertyRead = canRead
            , I.propertyWrite = canWrite
            }

gen_Ascii :: Gen String
gen_Ascii = listOf (elements ['!'..'~'])
