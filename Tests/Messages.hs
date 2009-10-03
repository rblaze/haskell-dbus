{-
  Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE RankNTypes #-}
module Tests.Messages (messageProperties) where

import Data.Set (fromList)
import Test.QuickCheck
import Tests.Instances ()
import qualified Data.ByteString.Lazy as L
import DBus.Message
import DBus.Types

import qualified Data.Binary.Get as G

messageProperties = concat
	[ headerFieldProperties
	, forAllMessages prop_Marshal
	, [ property prop_Unmarshal0
	  , property prop_Unmarshal1
	  , property prop_Unmarshal2
	  , property prop_Unmarshal3
	  ]
	]

headerFieldProperties =
	[ property prop_HeaderFieldVariable
	]

prop_HeaderFieldVariable x = fromVariant (toVariant x) == Just x
	where types = [x :: HeaderField]

prop_Marshal gen = forAll arbitrary (\e s -> forAll gen (not . L.null . marshal e s))

prop_Unmarshal0 e s msg = checkUnmarshal expected e s msg where
	expected = ReceivedMethodCall s Nothing msg

prop_Unmarshal1 e s msg = checkUnmarshal expected e s msg where
	expected = ReceivedMethodReturn s Nothing msg

prop_Unmarshal2 e s msg = checkUnmarshal expected e s msg where
	expected = ReceivedError s Nothing msg

prop_Unmarshal3 e s msg = checkUnmarshal expected e s msg where
	expected = ReceivedSignal s Nothing msg

checkUnmarshal expected e s msg = unmarshaled == Right expected where
	bytes = marshal e s msg
	getBytes = G.getLazyByteString . fromIntegral
	unmarshaled = G.runGet (unmarshal getBytes) bytes

forAllMessages :: (forall a. (Show a, Arbitrary a, Message a, Eq a) =>
                  (Gen a -> Property)) -> [Property]
forAllMessages t =
	[ t (arbitrary :: Gen MethodCall)
	, t (arbitrary :: Gen MethodReturn)
	, t (arbitrary :: Gen Error)
	, t (arbitrary :: Gen Signal)
	]

instance Arbitrary Flag where
	coarbitrary = undefined
	arbitrary = elements [NoReplyExpected, NoAutoStart]

instance Arbitrary HeaderField where
	coarbitrary = undefined
	arbitrary = do
		x <- choose (1, 8)
		case (x :: Int) of
			1 -> fmap Path arbitrary
			2 -> fmap Interface arbitrary
			3 -> fmap Member arbitrary
			4 -> fmap ErrorName arbitrary
			5 -> fmap ReplySerial arbitrary
			6 -> fmap Destination arbitrary
			7 -> fmap Sender arbitrary
			8 -> fmap Signature arbitrary

instance Arbitrary MethodCall where
	coarbitrary = undefined
	arbitrary = do
		path   <- arbitrary
		member <- arbitrary
		iface  <- arbitrary
		dest   <- arbitrary
		flags  <- fmap fromList arbitrary
		body   <- sized (\n -> resize (n `div` 2) arbitrary)
		return $ MethodCall path member iface dest flags body

instance Arbitrary MethodReturn where
	coarbitrary = undefined
	arbitrary = do
		serial <- arbitrary
		dest   <- arbitrary
		flags  <- fmap fromList arbitrary
		body   <- sized (\n -> resize (n `div` 2) arbitrary)
		return $ MethodReturn serial dest flags body

instance Arbitrary Error where
	coarbitrary = undefined
	arbitrary = do
		name   <- arbitrary
		serial <- arbitrary
		dest   <- arbitrary
		flags  <- fmap fromList arbitrary
		body   <- sized (\n -> resize (n `div` 2) arbitrary)
		return $ Error name serial dest flags body

instance Arbitrary Signal where
	coarbitrary = undefined
	arbitrary = do
		path   <- arbitrary
		member <- arbitrary
		iface  <- arbitrary
		dest   <- arbitrary
		flags  <- fmap fromList arbitrary
		body   <- sized (\n -> resize (n `div` 2) arbitrary)
		return $ Signal path member iface dest flags body
