{-# LANGUAGE OverloadedStrings #-}
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

module DBusTests.Introspection (test_Introspection) where

import           Test.Chell
import           Test.Chell.QuickCheck
import           Test.QuickCheck hiding (property)

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad (liftM, liftM2)
import qualified Data.Text as T

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
	]

test_XmlPassthrough :: Suite
test_XmlPassthrough = property "xml-passthrough" $ \obj -> let
	(Introspection.Object path _ _) = obj
	Just xml = Introspection.toXML obj
	in Introspection.fromXML path xml == Just obj

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
		case objectPathText x of
			"/" -> nonRoot
			x'  -> return x'
	
	thisPath <- nonRoot
	let path' = case objectPathText parentPath of
		"/" -> thisPath
		x   -> T.append x thisPath
	let path = objectPath_ path'
	ifaces <- arbitrary
	children <- halfSized (listOf (subObject path))
	return (Introspection.Object path ifaces children)

instance Arbitrary Introspection.Interface where
	arbitrary = Introspection.Interface
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary
		<*> arbitrary

instance Arbitrary Introspection.Method where
	arbitrary = Introspection.Method
		<$> arbitrary
		<*> arbitrary
		<*> arbitrary

instance Arbitrary Introspection.Signal where
	arbitrary = Introspection.Signal
		<$> arbitrary
		<*> arbitrary

instance Arbitrary Introspection.Parameter where
	arbitrary = Introspection.Parameter
		<$> gen_Text
		<*> arbitrary

instance Arbitrary Introspection.Property where
	arbitrary = Introspection.Property
		<$> gen_Text
		<*> arbitrary
		<*> elements
			[ []
			, [ Introspection.Read ]
			, [ Introspection.Write ]
			, [ Introspection.Read
			  , Introspection.Write ]
			]

gen_Text :: Gen T.Text
gen_Text = fmap T.pack (listOf (elements ['!'..'~']))
