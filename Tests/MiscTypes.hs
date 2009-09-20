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

module Tests.MiscTypes (miscTypeProperties) where

import Data.Maybe (isNothing)
import Data.Word (Word8)
import Test.QuickCheck
import Tests.Instances ()
import DBus.Types

miscTypeProperties = concat
	[ objectPathProperties
	, interfaceNameProperties
	, busNameProperties
	, memberNameProperties
	, errorNameProperties
	, endiannessProperties
	, serialProperties
	]

prop_Equality x = x == x

-- Object paths

objectPathProperties =
	[ property (prop_Equality :: ObjectPath -> Bool)
	, property prop_ObjectPathIdentity
	] ++ map property prop_ObjectPathInvalid

prop_ObjectPathIdentity x = mkObjectPath (strObjectPath x) == Just x

prop_ObjectPathInvalid = let invalid = isNothing . mkObjectPath in 
	[ invalid ""
	, invalid "a"
	, invalid "/a/"
	, invalid "/a/-"
	]

-- Endianness

endiannessProperties =
	[ property (prop_Equality :: Endianness -> Bool)
	, property prop_EndianToVariant
	, property prop_EndianFromVariant
	]

prop_EndianToVariant x@(LittleEndian) = toVariant x == toVariant (0x6C :: Word8)
prop_EndianToVariant x@(BigEndian) = toVariant x == toVariant (0x42 :: Word8)

endianWords :: Gen Word8
endianWords = oneof [ return 0x6C , return 0x42 , arbitrary]

prop_EndianFromVariant = forAll endianWords $ \x -> let
	v = fromVariant (toVariant x) in case x of
		0x6C -> v == Just LittleEndian
		0x42 -> v == Just BigEndian
		_    -> isNothing v

-- Interface names

interfaceNameProperties =
	[ property (prop_Equality :: InterfaceName -> Bool)
	, property prop_InterfaceNameIdentity
	] ++ map property prop_InterfaceNameInvalid

prop_InterfaceNameIdentity x = mkInterfaceName (strInterfaceName x) == Just x

prop_InterfaceNameInvalid = let invalid = isNothing . mkInterfaceName in 
	[ invalid ""
	, invalid "0"
	, invalid "a"
	, invalid "a."
	, invalid "a.1"
	, invalid $ "a." ++ replicate 255 'b'
	]

-- Bus names

busNameProperties =
	[ property (prop_Equality :: BusName -> Bool)
	, property prop_BusNameIdentity
	] ++ map property prop_BusNameInvalid

prop_BusNameIdentity x = mkBusName (strBusName x) == Just x

prop_BusNameInvalid = let invalid = isNothing . mkBusName in 
	[ invalid ""
	, invalid ":"
	, invalid ":0"
	, invalid ":a"
	, invalid "0"
	, invalid "a"
	, invalid "a."
	, invalid "a.1"
	, invalid $ "a." ++ replicate 255 'b'
	]

-- Member names

memberNameProperties =
	[ property (prop_Equality :: MemberName -> Bool)
	, property prop_MemberNameIdentity
	] ++ map property prop_MemberNameInvalid

prop_MemberNameIdentity x = mkMemberName (strMemberName x) == Just x

prop_MemberNameInvalid = let invalid = isNothing . mkMemberName in 
	[ invalid ""
	, invalid "0"
	, invalid $ replicate 256 'a'
	]

-- Error names

errorNameProperties =
	[ property (prop_Equality :: ErrorName -> Bool)
	, property prop_ErrorNameIdentity
	] ++ map property prop_ErrorNameInvalid

prop_ErrorNameIdentity x = mkErrorName (strErrorName x) == Just x

prop_ErrorNameInvalid = let invalid = isNothing . mkErrorName in 
	[ invalid ""
	, invalid "0"
	, invalid "a"
	, invalid "a."
	, invalid "a.1"
	, invalid $ "a." ++ replicate 255 'b'
	]

-- Serials

serialProperties = 
	[ property prop_SerialIncremented
	]

prop_SerialIncremented x = nextSerial x > x
