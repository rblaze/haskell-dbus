-- Copyright (C) 2009-2012 John Millikin <jmillikin@gmail.com>
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

module DBus
	(
	-- * Messages
	  Message ( messageFlags
	          , messageBody
	          )
	, Flag (..)
	, UnknownMessage (..)
	, MethodCall (..)
	, MethodReturn (..)
	, MethodError (..)
	, methodErrorMessage
	, Signal (..)
	, ReceivedMessage (..)
	
	-- * Variants
	, Variant
	, IsVariant(..)
	, variantType
	
	, IsAtom
	, IsValue
	, typeOf
	
	-- * Signatures
	, Signature
	, Type(..)
	, signature
	, signature_
	, signatureText
	, signatureTypes
	, parseSignature
	
	-- * Object paths
	, ObjectPath
	, objectPath
	, objectPath_
	, objectPathText
	
	-- * Interface names
	, InterfaceName
	, interfaceName
	, interfaceName_
	, interfaceNameText
	
	-- * Member names
	, MemberName
	, memberName
	, memberName_
	, memberNameText
	
	-- * Error names
	, ErrorName
	, errorName
	, errorName_
	, errorNameText
	
	-- * Bus names
	, BusName
	, busName
	, busName_
	, busNameText
	
	-- * Heterogenous containers
	, Structure
	, Array
	, Dictionary
	, structureItems
	, arrayItems
	, dictionaryItems
	
	-- * Addresses
	, Address
	, addressMethod
	, addressParameters
	, address
	, formatAddress
	, formatAddresses
	, parseAddress
	, parseAddresses
	, getSystemAddress
	, getSessionAddress
	, getStarterAddress
	
	-- * Message marshaling
	, Endianness (..)
	, MarshalError
	, UnmarshalError
	, marshalMessage
	, unmarshalMessage
	
	-- ** Message serials
	, Serial
	, serialValue
	, firstSerial
	, nextSerial
	
	-- * D-Bus UUIDs
	, UUID
	, formatUUID
	, randomUUID
	) where

import           Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as Char8
import           Data.Word (Word16)
import           System.Random (randomRIO)
import           Text.Printf (printf)

import           DBus.Address
import           DBus.Message
import qualified DBus.Types
import           DBus.Types hiding (typeOf)
import           DBus.Wire

typeOf :: IsValue a => a -> Type
typeOf = DBus.Types.typeOf

-- | A D-Bus UUID is 128 bits of data, usually randomly generated. They are
-- used for identifying unique server instances to clients.
--
-- Older versions of the D-Bus spec also called these values /GUIDs/.
--
-- D-Bus UUIDs are not the same as the RFC-standardized UUIDs or GUIDs.
newtype UUID = UUID Char8.ByteString
	deriving (Eq, Ord, Show)

-- | Format a D-Bus UUID as hex-encoded ASCII.
formatUUID :: UUID -> Char8.ByteString
formatUUID (UUID bytes) = bytes

-- | Generate a random D-Bus UUID. This value is suitable for use in a
-- randomly-allocated address, or as a listener's socket address
-- @\"guid\"@ parameter.
randomUUID :: IO UUID
randomUUID = do
	-- The version of System.Random bundled with ghc < 7.2 doesn't define
	-- instances for any of the fixed-length word types, so we imitate
	-- them using the instance for Int.
	--
	-- 128 bits is 8 16-bit integers. We use chunks of 16 instead of 32
	-- because Int is not guaranteed to be able to store a Word32.
	let hexInt16 i = printf "%04x" (i :: Int)
	int16s <- replicateM 8 (randomRIO (0, fromIntegral (maxBound :: Word16)))
	return (UUID (Char8.pack (concatMap hexInt16 int16s)))
