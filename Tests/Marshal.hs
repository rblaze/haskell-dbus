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

module Tests.Marshal (marshalProperties) where

import qualified Control.Exception as E
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Maybe (fromJust)

import Test.QuickCheck
import Tests.Instances ()
import DBus.Types
import DBus.Protocol.Marshal

marshalProperties :: [Property]
marshalProperties =
	[ property prop_MarshalAnyVariant
	, property prop_MarshalAtom
	]

-- Check that any variant can be marshaled successfully.
prop_MarshalAnyVariant e x = noError $ marshal e [x]

-- Any atomic value should marshal to *something*
prop_MarshalAtom e x = not . L.null . marshal e $ [atomToVariant x]

-- TODO: test bytes of marshaled atoms

-- TODO: test bytes of marshaled containers

-- Helper to determine whether the given pure function raised an exception.
-- No exceptions should be raised during the normal process of marshaling.
noError :: a -> Bool
noError x = unsafePerformIO $ E.catch io onError where
	onError :: E.SomeException -> IO Bool
	onError = const (return False)
	io = E.evaluate x >> return True
