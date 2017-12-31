-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
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

module DBusTests.Introspection (test_Introspection) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import DBus
import qualified DBus.Introspection as Introspection

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
    path = Introspection.objectPath obj
    Just xml = Introspection.formatXML obj
    in Introspection.parseXML path xml == Just obj

test_XmlParse :: TestTree
test_XmlParse = testCase "xml-parse" $ do
    -- root object path can be inferred
    Introspection.parseXML (objectPath_ "/") "<node><node name='foo'/></node>"
        @?= Just (Introspection.object (objectPath_ "/"))
            { Introspection.objectChildren =
                [ Introspection.object (objectPath_ "/foo")
                ]
            }

test_XmlParseFailed :: TestTree
test_XmlParseFailed = testCase "xml-parse-failed" $ do
    Nothing @=? Introspection.parseXML (objectPath_ "/") "<invalid>"
    Nothing @=? Introspection.parseXML (objectPath_ "/") "<invalid/>"

    -- invalid property access
    Nothing @=? Introspection.parseXML (objectPath_ "/")
        "<node>\
        \  <interface name='com.example.Foo'>\
        \    <property type='s' access='invalid'>\
        \    </property>\
        \  </interface>\
        \</node>"

    -- invalid parameter type
    Nothing @=? Introspection.parseXML (objectPath_ "/")
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
    Nothing @=? Introspection.formatXML (Introspection.object (objectPath_ "/foo"))
        { Introspection.objectChildren =
            [ Introspection.object (objectPath_ "/bar")
            ]
        }

    -- invalid type
    Nothing @=? Introspection.formatXML (Introspection.object (objectPath_ "/foo"))
        { Introspection.objectInterfaces =
            [ (Introspection.interface (interfaceName_ "/bar"))
                { Introspection.interfaceProperties =
                    [ Introspection.property "prop" (TypeDictionary TypeVariant TypeVariant)
                    ]
                }
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

instance Arbitrary Introspection.Object where
    arbitrary = arbitrary >>= subObject

subObject :: ObjectPath -> Gen Introspection.Object
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
    return (Introspection.object path)
        { Introspection.objectInterfaces = ifaces
        , Introspection.objectChildren = children
        }

instance Arbitrary Introspection.Interface where
    arbitrary = do
        name <- arbitrary
        methods <- arbitrary
        signals <- arbitrary
        properties <- arbitrary
        return (Introspection.interface name)
            { Introspection.interfaceMethods = methods
            , Introspection.interfaceSignals = signals
            , Introspection.interfaceProperties = properties
            }

instance Arbitrary Introspection.Method where
    arbitrary = do
        name <- arbitrary
        args <- arbitrary
        return (Introspection.method name)
            { Introspection.methodArgs = args
            }

instance Arbitrary Introspection.Signal where
    arbitrary = do
        name <- arbitrary
        args <- arbitrary
        return (Introspection.signal name)
            { Introspection.signalArgs = args
            }

instance Arbitrary Introspection.MethodArg where
    arbitrary = Introspection.methodArg
        <$> gen_Ascii
        <*> arbitrary
        <*> arbitrary

instance Arbitrary Introspection.Direction where
    arbitrary = elements [Introspection.directionIn, Introspection.directionOut]

instance Arbitrary Introspection.SignalArg where
    arbitrary = Introspection.signalArg
        <$> gen_Ascii
        <*> arbitrary

instance Arbitrary Introspection.Property where
    arbitrary = do
        name <- gen_Ascii
        t <- arbitrary
        canRead <- arbitrary
        canWrite <- arbitrary
        return (Introspection.property name t)
            { Introspection.propertyRead = canRead
            , Introspection.propertyWrite = canWrite
            }

gen_Ascii :: Gen String
gen_Ascii = listOf (elements ['!'..'~'])
