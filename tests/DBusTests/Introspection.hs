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

module DBusTests.Introspection (test_Introspection) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (liftM, liftM2)

import           DBus
import qualified DBus.Introspection as Introspection

import           DBusTests.InterfaceName ()
import           DBusTests.MemberName ()
import           DBusTests.ObjectPath ()
import           DBusTests.Signature ()
import           DBusTests.Util (halfSized)

test_Introspection :: Suite
test_Introspection = suite "Introspection"
	[ test_XmlPassthrough
	, test_XmlParse
	, test_XmlParseFailed
	, test_XmlWriteFailed
	]

test_XmlPassthrough :: Test
test_XmlPassthrough = property "xml-passthrough" $ \obj -> let
	path = Introspection.objectPath obj
	Just xml = Introspection.formatXML obj
	in Introspection.parseXML path xml == Just obj

test_XmlParse :: Test
test_XmlParse = assertions "xml-parse" $ do
	-- root object path can be inferred
	$expect (equal
		(Introspection.parseXML (objectPath_ "/") "<node><node name='foo'/></node>")
		(Just (Introspection.object (objectPath_ "/"))
			{ Introspection.objectChildren = 
				[ Introspection.object (objectPath_ "/foo")
				]
			}
		))

test_XmlParseFailed :: Test
test_XmlParseFailed = assertions "xml-parse-failed" $ do
	$expect (nothing (Introspection.parseXML (objectPath_ "/") "<invalid>"))
	$expect (nothing (Introspection.parseXML (objectPath_ "/") "<invalid/>"))
	
	-- invalid property access
	$expect (nothing (Introspection.parseXML (objectPath_ "/")
		"<node>\
		\  <interface name='com.example.Foo'>\
		\    <property type='s' access='invalid'>\
		\    </property>\
		\  </interface>\
		\</node>"))
	
	-- invalid parameter type
	$expect (nothing (Introspection.parseXML (objectPath_ "/")
		"<node>\
		\  <interface name='com.example.Foo'>\
		\    <method name='Foo'>\
		\      <arg type='yy'/>\
		\    </method>\
		\  </interface>\
		\</node>"))

test_XmlWriteFailed :: Test
test_XmlWriteFailed = assertions "xml-write-failed" $ do
	-- child's object path isn't under parent's
	$expect (nothing (Introspection.formatXML (Introspection.object (objectPath_ "/foo"))
		{ Introspection.objectChildren =
			[ Introspection.object (objectPath_ "/bar")
			]
		}))
	
	-- invalid type
	$expect (nothing (Introspection.formatXML (Introspection.object (objectPath_ "/foo"))
		{ Introspection.objectInterfaces =
			[ (Introspection.interface (interfaceName_ "/bar"))
				{ Introspection.interfaceProperties =
					[ Introspection.property "prop" (TypeDictionary TypeVariant TypeVariant)
					]
				}
			]
		}))

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
