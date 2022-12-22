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
import qualified Data.Text as T

import DBus
import qualified DBus.Introspection.Parse as I
import qualified DBus.Introspection.Render as I
import qualified DBus.Introspection.Types as I

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
    , test_XmlIgnoreUnrecognizedTag
    ]

test_XmlPassthrough :: TestTree
test_XmlPassthrough = testProperty "xml-passthrough" $ \obj -> let
    path = I.objectPath obj
    Just xml = I.formatXML obj
    in I.parseXML path (T.pack xml) == Just obj

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

    -- missing closing tag
    Nothing @=? I.parseXML (objectPath_ "/")
        "<node>\
        \  <interface name='com.example.Foo'>\
        \</node>"

    -- missing attribute name
    Nothing @=? I.parseXML (objectPath_ "/")
        "<node>\
        \  <interface 'com.example.Foo'>\
        \  </interface>\
        \</node>"

    -- invalid interface name
    Nothing @=? I.parseXML (objectPath_ "/")
        "<node>\
        \  <interface name=5>\
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

test_XmlIgnoreUnrecognizedTag :: TestTree
test_XmlIgnoreUnrecognizedTag = testCase "xml-ignore-unrecognized-tag" $ do
    -- ignore tp namespace tags
    I.parseXML (objectPath_ "/")
        "<node xmlns:tp='http://telepathy.freedesktop.org/wiki/DbusSpec#extensions-v0' name='/'> \
        \  <node name='foo'/>\
        \  <tp:enum name='FOO' type='s'>\
        \    <tp:docstring>sample docstring</tp:docstring>\
        \    <tp:enumvalue suffix='BAR' value=''/>\
        \  </tp:enum>\
        \</node>"
        @?= Just (buildEmptyObject "/")
            { I.objectChildren =
                [ buildEmptyObject "/foo"
                ]
            }

    -- ignore nested tp tags
    I.parseXML (objectPath_ "/")
        "<node xmlns:tp='http://telepathy.freedesktop.org/wiki/DbusSpec#extensions-v0' name='/'> \
        \  <interface name='com.example.Foo'>\
        \    <tp:docstring>sample docstring</tp:docstring>\
        \    <method name='bar'>\
        \      <tp:docstring>bar docstring</tp:docstring>\
        \    </method>\
        \  </interface>\
        \</node>"
        @?= Just (buildEmptyObject "/")
            { I.objectInterfaces =
                [ I.Interface (interfaceName_ "com.example.Foo") [I.Method (memberName_ "bar") []] [] []
                ]
            }

    -- ignore invalid parameter type
    I.parseXML (objectPath_ "/")
        "<node>\
        \  <interface name='com.example.Foo'>\
        \    <method name='Foo'>\
        \      <arg type='yy'/>\
        \    </method>\
        \  </interface>\
        \</node>"
        @?= Just (buildEmptyObject "/")
            { I.objectInterfaces =
                [ I.Interface (interfaceName_ "com.example.Foo") [I.Method (memberName_ "Foo") []] [] []
                ]
            }

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
