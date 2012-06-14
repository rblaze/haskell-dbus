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
	  Message
	
	-- ** Method calls
	, MethodCall
	, methodCall
	, methodCallPath
	, methodCallInterface
	, methodCallMember
	, methodCallSender
	, methodCallDestination
	, methodCallAutoStart
	, methodCallReplyExpected
	, methodCallBody
	
	-- ** Method returns
	, MethodReturn
	, methodReturn
	, methodReturnSerial
	, methodReturnSender
	, methodReturnDestination
	, methodReturnBody
	
	-- ** Method errors
	, MethodError
	, methodError
	, methodErrorName
	, methodErrorSerial
	, methodErrorSender
	, methodErrorDestination
	, methodErrorBody
	, methodErrorMessage
	
	-- ** Signals
	, Signal
	, signal
	, signalPath
	, signalMember
	, signalInterface
	, signalSender
	, signalDestination
	, signalBody
	
	-- ** Received messages
	, ReceivedMessage(ReceivedMethodCall, ReceivedMethodReturn, ReceivedMethodError, ReceivedSignal)
	, receivedMessageSerial
	, receivedMessageSender
	, receivedMessageBody
	
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
	, signatureTypes
	, formatSignature
	, parseSignature
	
	-- * Object paths
	, ObjectPath
	, objectPath_
	, formatObjectPath
	, parseObjectPath
	
	-- * Interface names
	, InterfaceName
	, interfaceName_
	, formatInterfaceName
	, parseInterfaceName
	
	-- * Member names
	, MemberName
	, memberName_
	, formatMemberName
	, parseMemberName
	
	-- * Error names
	, ErrorName
	, errorName_
	, formatErrorName
	, parseErrorName
	
	-- * Bus names
	, BusName
	, busName_
	, formatBusName
	, parseBusName
	
	-- * Heterogenous containers
	
	-- ** Structures
	, Structure
	, structureItems
	
	-- ** Arrays
	, Array
	, arrayItems
	
	-- ** Dictionaries
	, Dictionary
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
	
	-- ** Marshal
	, marshal
	, MarshalError
	, marshalErrorMessage
	
	-- ** Unmarshal
	, unmarshal
	, UnmarshalError
	, unmarshalErrorMessage
	
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

methodCall :: ObjectPath -> InterfaceName -> MemberName -> MethodCall
methodCall path iface member = MethodCall path (Just iface) member Nothing Nothing True True []

methodReturn :: Serial -> MethodReturn
methodReturn s = MethodReturn s Nothing Nothing []

methodError :: Serial -> ErrorName -> MethodError
methodError s name = MethodError name s Nothing Nothing []

signal :: ObjectPath -> InterfaceName -> MemberName -> Signal
signal path iface member = Signal path iface member Nothing Nothing []

receivedMessageSerial :: ReceivedMessage -> Serial
receivedMessageSerial (ReceivedMethodCall s _) = s
receivedMessageSerial (ReceivedMethodReturn s _) = s
receivedMessageSerial (ReceivedMethodError s _) = s
receivedMessageSerial (ReceivedSignal s _) = s
receivedMessageSerial (ReceivedUnknown s _) = s

receivedMessageSender :: ReceivedMessage -> Maybe BusName
receivedMessageSender (ReceivedMethodCall _ msg) = methodCallSender msg
receivedMessageSender (ReceivedMethodReturn _ msg) = methodReturnSender msg
receivedMessageSender (ReceivedMethodError _ msg) = methodErrorSender msg
receivedMessageSender (ReceivedSignal _ msg) = signalSender msg
receivedMessageSender (ReceivedUnknown _ msg) = unknownMessageSender msg

receivedMessageBody :: ReceivedMessage -> [Variant]
receivedMessageBody (ReceivedMethodCall _ msg) = methodCallBody msg
receivedMessageBody (ReceivedMethodReturn _ msg) = methodReturnBody msg
receivedMessageBody (ReceivedMethodError _ msg) = methodErrorBody msg
receivedMessageBody (ReceivedSignal _ msg) = signalBody msg
receivedMessageBody (ReceivedUnknown _ msg) = unknownMessageBody msg

-- | Convert a 'Message' into a 'Char8.ByteString'. Although unusual, it is
-- possible for marshaling to fail; if this occurs, an error will be
-- returned instead.
marshal :: Message msg => Endianness -> Serial -> msg -> Either MarshalError Char8.ByteString
marshal = marshalMessage

-- | Parse a 'Char8.ByteString' into a 'ReceivedMessage'. The result can be
-- inspected to see what type of message was parsed. Unknown message types
-- can still be parsed successfully, as long as they otherwise conform to
-- the D-Bus standard.
unmarshal :: Char8.ByteString -> Either UnmarshalError ReceivedMessage
unmarshal = unmarshalMessage

-- | A D-Bus UUID is 128 bits of data, usually randomly generated. They are
-- used for identifying unique server instances to clients.
--
-- Older versions of the D-Bus spec also called these values /GUIDs/.
--
-- D-Bus UUIDs are not the same as the RFC-standardized UUIDs or GUIDs.
newtype UUID = UUID Char8.ByteString
	deriving (Eq, Ord, Show)

-- | Format a D-Bus UUID as hex-encoded ASCII.
formatUUID :: UUID -> String
formatUUID (UUID bytes) = Char8.unpack bytes

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
