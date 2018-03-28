-- Copyright (C) 2012 John Millikin <john@john-millikin.com>
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

module Main
	( tests
	, main
	) where

import           Test.Chell

import           DBusTests.Address
import           DBusTests.BusName
import           DBusTests.Client
import           DBusTests.ErrorName
import           DBusTests.Integration
import           DBusTests.InterfaceName
import           DBusTests.Introspection
import           DBusTests.MemberName
import           DBusTests.Message
import           DBusTests.ObjectPath
import           DBusTests.Serialization
import           DBusTests.Socket
import           DBusTests.Signature
import           DBusTests.Transport
import           DBusTests.Variant
import           DBusTests.Wire

-- import all dbus modules here to ensure they show up in the coverage report,
-- even if not tested.
import           DBus ()
import           DBus.Address ()
import           DBus.Client ()
import           DBus.Introspection ()
import           DBus.Message ()
import           DBus.Socket ()
import           DBus.Types ()
import           DBus.Wire ()

tests :: [Suite]
tests =
	[ test_Address
	, test_BusName
	, test_Client
	, test_ErrorName
	, test_Integration
	, test_InterfaceName
	, test_Introspection
	, test_MemberName
	, test_Message
	, test_ObjectPath
	, test_Serialization
	, test_Signature
	, test_Socket
	, test_Transport
	, test_Variant
	, test_Wire
	]

main :: IO ()
main = Test.Chell.defaultMain tests
