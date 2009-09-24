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

module Tests.Messages (messageProperties) where

import Test.QuickCheck
import Tests.Instances ()
import DBus.Message
import DBus.Types

messageProperties = concat
	[ headerFieldProperties
	]

headerFieldProperties =
	[ property prop_HeaderFieldVariable
	]

prop_HeaderFieldVariable x = fromVariant (toVariant x) == Just x
	where types = [x :: HeaderField]

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
