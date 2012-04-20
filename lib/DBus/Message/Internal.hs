{-# LANGUAGE OverloadedStrings #-}

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

module DBus.Message.Internal where

import           Data.Maybe (fromMaybe)
import qualified Data.Set
import           Data.Set (Set)
import qualified Data.Text
import           Data.Text (Text)
import           Data.Word (Word8, Word32)

import           DBus.Types hiding (errorName)
import           DBus.Util (maybeIndex)

class Message a where
	messageTypeCode     :: a -> Word8
	messageHeaderFields :: a -> [HeaderField]
	messageFlags        :: a -> Set Flag
	messageBody         :: a -> [Variant]

maybe' :: (a -> b) -> Maybe a -> [b]
maybe' f = maybe [] (\x' -> [f x'])

data Unknown = Unknown
	{ unknownType    :: Word8
	, unknownFlags   :: Set Flag
	, unknownBody    :: [Variant]
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

-- | A value used to uniquely identify a particular message within a session.
-- 'Serial's are 32&#8208;bit unsigned integers, and eventually wrap.

newtype Serial = Serial Word32
	deriving (Eq, Ord, Show)

instance IsVariant Serial where
	toVariant (Serial x) = toVariant x
	fromVariant = fmap Serial . fromVariant

serialValue :: Serial -> Word32
serialValue (Serial x) = x

data MethodCall = MethodCall
	{ methodCallPath        :: ObjectPath
	, methodCallMember      :: MemberName
	, methodCallInterface   :: Maybe InterfaceName
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
		, maybe' HeaderDestination (methodCallDestination m)
		]

data MethodReturn = MethodReturn
	{ methodReturnSerial      :: Serial
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
		, maybe' HeaderDestination (methodReturnDestination m)
		]

data Error = Error
	{ errorName        :: ErrorName
	, errorSerial      :: Serial
	, errorDestination :: Maybe BusName
	, errorBody        :: [Variant]
	}
	deriving (Show, Eq)

instance Message Error where
	messageTypeCode _ = 3
	messageFlags    _ = Data.Set.fromList [NoReplyExpected, NoAutoStart]
	messageBody       = errorBody
	messageHeaderFields m = concat
		[ [ HeaderErrorName (errorName m)
		  , HeaderReplySerial (errorSerial m)
		  ]
		, maybe' HeaderDestination (errorDestination m)
		]

errorMessage :: Error -> Text
errorMessage msg = fromMaybe "(no error message)" $ do
	field <- maybeIndex (errorBody msg) 0
	text <- fromVariant field
	if Data.Text.null text
		then Nothing
		else return text

data Signal = Signal
	{ signalDestination :: Maybe BusName
	, signalPath        :: ObjectPath
	, signalInterface   :: InterfaceName
	, signalMember      :: MemberName
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
		, maybe' HeaderDestination (signalDestination m)
		]

-- | Not an actual message type, but a wrapper around messages received from
-- the bus. Each value contains the message&#8217;s 'Serial' and possibly the
-- origin&#8217;s 'BusName'
data ReceivedMessage
	= ReceivedMethodCall   Serial (Maybe BusName) MethodCall
	| ReceivedMethodReturn Serial (Maybe BusName) MethodReturn
	| ReceivedError        Serial (Maybe BusName) Error
	| ReceivedSignal       Serial (Maybe BusName) Signal
	| ReceivedUnknown      Serial (Maybe BusName) Unknown
	deriving (Show, Eq)

receivedSerial :: ReceivedMessage -> Serial
receivedSerial (ReceivedMethodCall   s _ _) = s
receivedSerial (ReceivedMethodReturn s _ _) = s
receivedSerial (ReceivedError        s _ _) = s
receivedSerial (ReceivedSignal       s _ _) = s
receivedSerial (ReceivedUnknown      s _ _) = s

receivedSender :: ReceivedMessage -> Maybe BusName
receivedSender (ReceivedMethodCall   _ s _) = s
receivedSender (ReceivedMethodReturn _ s _) = s
receivedSender (ReceivedError        _ s _) = s
receivedSender (ReceivedSignal       _ s _) = s
receivedSender (ReceivedUnknown      _ s _) = s

receivedBody :: ReceivedMessage -> [Variant]
receivedBody (ReceivedMethodCall   _ _ x) = messageBody x
receivedBody (ReceivedMethodReturn _ _ x) = messageBody x
receivedBody (ReceivedError        _ _ x) = messageBody x
receivedBody (ReceivedSignal       _ _ x) = messageBody x
receivedBody (ReceivedUnknown      _ _ x) = unknownBody x
