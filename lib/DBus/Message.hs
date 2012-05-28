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

module DBus.Message where

import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set
import           Data.Set (Set)
import           Data.Word (Word8)

import           DBus.Types

class Message a where
	messageTypeCode     :: a -> Word8
	messageHeaderFields :: a -> [HeaderField]
	messageFlags        :: a -> Set Flag
	messageBody         :: a -> [Variant]

maybe' :: (a -> b) -> Maybe a -> [b]
maybe' f = maybe [] (\x' -> [f x'])

data UnknownMessage = UnknownMessage
	{ unknownMessageType :: Word8
	, unknownMessageFlags :: Set Flag
	, unknownMessageBody :: [Variant]
	}
	deriving (Show, Eq)

data HeaderField
	= HeaderPath        ObjectPath
	| HeaderInterface   InterfaceName
	| HeaderMember      MemberName
	| HeaderErrorName   ErrorName
	| HeaderReplySerial Serial
	| HeaderDestination BusName
	| HeaderSender      BusName
	| HeaderSignature   Signature
	deriving (Show, Eq)

data Flag
	= NoReplyExpected
	| NoAutoStart
	deriving (Show, Eq, Ord)

data MethodCall = MethodCall
	{ methodCallPath        :: ObjectPath
	, methodCallMember      :: MemberName
	, methodCallInterface   :: Maybe InterfaceName
	, methodCallSender      :: Maybe BusName
	, methodCallDestination :: Maybe BusName
	, methodCallFlags       :: Set Flag
	, methodCallBody        :: [Variant]
	}
	deriving (Show, Eq)

instance Message MethodCall where
	messageTypeCode _ = 1
	messageFlags      = methodCallFlags
	messageBody       = methodCallBody
	messageHeaderFields m = concat
		[ [ HeaderPath (methodCallPath m)
		  , HeaderMember (methodCallMember m)
		  ]
		, maybe' HeaderInterface (methodCallInterface m)
		, maybe' HeaderSender (methodCallSender m)
		, maybe' HeaderDestination (methodCallDestination m)
		]

data MethodReturn = MethodReturn
	{ methodReturnSerial      :: Serial
	, methodReturnSender      :: Maybe BusName
	, methodReturnDestination :: Maybe BusName
	, methodReturnBody        :: [Variant]
	}
	deriving (Show, Eq)

instance Message MethodReturn where
	messageTypeCode _ = 2
	messageFlags    _ = Data.Set.fromList [NoReplyExpected, NoAutoStart]
	messageBody       = methodReturnBody
	messageHeaderFields m = concat
		[ [ HeaderReplySerial (methodReturnSerial m)
		  ]
		, maybe' HeaderSender (methodReturnSender m)
		, maybe' HeaderDestination (methodReturnDestination m)
		]

data MethodError = MethodError
	{ methodErrorName        :: ErrorName
	, methodErrorSerial      :: Serial
	, methodErrorSender      :: Maybe BusName
	, methodErrorDestination :: Maybe BusName
	, methodErrorBody        :: [Variant]
	}
	deriving (Show, Eq)

instance Message MethodError where
	messageTypeCode _ = 3
	messageFlags    _ = Data.Set.fromList [NoReplyExpected, NoAutoStart]
	messageBody       = methodErrorBody
	messageHeaderFields m = concat
		[ [ HeaderErrorName (methodErrorName m)
		  , HeaderReplySerial (methodErrorSerial m)
		  ]
		, maybe' HeaderSender (methodErrorSender m)
		, maybe' HeaderDestination (methodErrorDestination m)
		]

methodErrorMessage :: MethodError -> String
methodErrorMessage err = fromMaybe "(no error message)" $ do
	field <- listToMaybe (methodErrorBody err)
	msg <- fromVariant field
	if null msg
		then Nothing
		else return msg

data Signal = Signal
	{ signalPath        :: ObjectPath
	, signalMember      :: MemberName
	, signalInterface   :: InterfaceName
	, signalSender      :: Maybe BusName
	, signalDestination :: Maybe BusName
	, signalBody        :: [Variant]
	}
	deriving (Show, Eq)

instance Message Signal where
	messageTypeCode _ = 4
	messageFlags    _ = Data.Set.fromList [NoReplyExpected, NoAutoStart]
	messageBody       = signalBody
	messageHeaderFields m = concat
		[ [ HeaderPath (signalPath m)
		  , HeaderMember (signalMember m)
		  , HeaderInterface (signalInterface m)
		  ]
		, maybe' HeaderSender (signalSender m)
		, maybe' HeaderDestination (signalDestination m)
		]

-- | Not an actual message type, but a wrapper around messages received from
-- the bus. Each value contains the message's 'Serial' and possibly the
-- origin's 'BusName'
data ReceivedMessage
	= ReceivedMethodCall Serial MethodCall
	| ReceivedMethodReturn Serial MethodReturn
	| ReceivedMethodError Serial MethodError
	| ReceivedSignal Serial Signal
	| ReceivedUnknown Serial UnknownMessage
	deriving (Show, Eq)
