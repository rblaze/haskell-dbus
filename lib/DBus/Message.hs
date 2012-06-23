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

module DBus.Message
	( Message(..)
	
	, UnknownMessage(..)
	, MethodCall(..)
	, MethodReturn(..)
	, MethodError(..)
	, methodErrorMessage
	, Signal(..)
	, ReceivedMessage(..)
	
	-- for use in Wire
	, HeaderField(..)
	, setMethodCallFlags
	) where

import           Data.Bits ((.|.), (.&.))
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Word (Word8)

import           DBus.Types

class Message a where
	messageTypeCode :: a -> Word8
	messageHeaderFields :: a -> [HeaderField]
	messageBody :: a -> [Variant]
	
	messageFlags :: a -> Word8
	messageFlags _ = 0

maybe' :: (a -> b) -> Maybe a -> [b]
maybe' f = maybe [] (\x' -> [f x'])

data UnknownMessage = UnknownMessage
	{ unknownMessageType :: Word8
	, unknownMessageSender :: Maybe BusName
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

-- | A method call is a request to run some procedure exported by the
-- remote process. Procedures are identified by an (object_path,
-- interface_name, method_name) tuple.
data MethodCall = MethodCall
	{
	-- | The object path of the method call. Conceptually, object paths
	-- act like a procedural language's pointers. Each object referenced
	-- by a path is a collection of procedures.
	  methodCallPath :: ObjectPath
	
	-- | The interface of the method call. Each object may implement any
	-- number of interfaces. Each method is part of at least one
	-- interface.
	--
	-- In certain cases, this may be @Nothing@, but most users should set
	-- it to a value.
	, methodCallInterface :: Maybe InterfaceName
	
	-- | The method name of the method call. Method names are unique within
	-- an interface, but might not be unique within an object.
	, methodCallMember :: MemberName
	
	-- | The name of the application that sent this call.
	--
	-- Most users will just leave this empty, because the bus overwrites
	-- the sender for security reasons. Setting the sender manually is
	-- used for peer-peer connections.
	--
	-- Defaults to @Nothing@.
	, methodCallSender :: Maybe BusName
	
	-- | The name of the application to send the call to.
	--
	-- Most users should set this. If a message with no destination is
	-- sent to the bus, the bus will behave as if the destination was
	-- set to @org.freedesktop.DBus@. For peer-peer connections, the
	-- destination can be empty because there is only one peer.
	--
	-- Defaults to @Nothing@.
	, methodCallDestination :: Maybe BusName
	
	-- | Set whether a reply is expected. This can save network and cpu
	-- resources by inhibiting unnecessary replies.
	--
	-- Defaults to @True@.
	, methodCallReplyExpected :: Bool
	
	-- | Set whether the bus should auto-start the remote
	--
	-- Defaults to @True@.
	, methodCallAutoStart :: Bool
	
	-- | The arguments to the method call. See 'toVariant'.
	--
	-- Defaults to @[]@.
	, methodCallBody :: [Variant]
	}
	deriving (Eq, Show)

setMethodCallFlags :: MethodCall -> Word8 -> MethodCall
setMethodCallFlags c w = c
	{ methodCallReplyExpected = w .&. 0x1 == 0
	, methodCallAutoStart = w .&. 0x2 == 0
	}

instance Message MethodCall where
	messageTypeCode _ = 1
	messageFlags c = foldr (.|.) 0
		[ if methodCallReplyExpected c then 0 else 0x1
		, if methodCallAutoStart c then 0 else 0x2
		]
	messageBody = methodCallBody
	messageHeaderFields m = concat
		[ [ HeaderPath (methodCallPath m)
		  , HeaderMember (methodCallMember m)
		  ]
		, maybe' HeaderInterface (methodCallInterface m)
		, maybe' HeaderSender (methodCallSender m)
		, maybe' HeaderDestination (methodCallDestination m)
		]

-- | A method return is a reply to a method call, indicating that the call
-- succeeded.
data MethodReturn = MethodReturn
	{
	-- | The serial of the original method call. This lets the original
	-- caller match up this reply to the pending call.
	  methodReturnSerial :: Serial
	
	-- | The name of the application that is returning from a call.
	--
	-- Most users will just leave this empty, because the bus overwrites
	-- the sender for security reasons. Setting the sender manually is
	-- used for peer-peer connections.
	--
	-- Defaults to @Nothing@.
	, methodReturnSender :: Maybe BusName
	
	-- | The name of the application that initiated the call.
	--
	-- Most users should set this. If a message with no destination is
	-- sent to the bus, the bus will behave as if the destination was
	-- set to @org.freedesktop.DBus@. For peer-peer connections, the
	-- destination can be empty because there is only one peer.
	--
	-- Defaults to @Nothing@.
	, methodReturnDestination :: Maybe BusName
	
	-- | Values returned from the method call. See 'toVariant'.
	--
	-- Defaults to @[]@.
	, methodReturnBody :: [Variant]
	}
	deriving (Show, Eq)

instance Message MethodReturn where
	messageTypeCode _ = 2
	messageBody       = methodReturnBody
	messageHeaderFields m = concat
		[ [ HeaderReplySerial (methodReturnSerial m)
		  ]
		, maybe' HeaderSender (methodReturnSender m)
		, maybe' HeaderDestination (methodReturnDestination m)
		]

-- | A method error is a reply to a method call, indicating that the call
-- received an error and did not succeed.
data MethodError = MethodError
	{
	-- | The name of the error type. Names are used so clients can
	-- handle certain classes of error differently from others.
	  methodErrorName :: ErrorName
	
	-- | The serial of the original method call. This lets the original
	-- caller match up this reply to the pending call.
	, methodErrorSerial :: Serial
	
	-- | The name of the application that is returning from a call.
	--
	-- Most users will just leave this empty, because the bus overwrites
	-- the sender for security reasons. Setting the sender manually is
	-- used for peer-peer connections.
	--
	-- Defaults to @Nothing@.
	, methodErrorSender :: Maybe BusName
	
	-- | The name of the application that initiated the call.
	--
	-- Most users should set this. If a message with no destination is
	-- sent to the bus, the bus will behave as if the destination was
	-- set to @org.freedesktop.DBus@. For peer-peer connections, the
	-- destination can be empty because there is only one peer.
	--
	-- Defaults to @Nothing@.
	, methodErrorDestination :: Maybe BusName
	
	-- | Additional information about the error. By convention, if
	-- the error body contains any items, the first item should be a
	-- string describing the error.
	, methodErrorBody :: [Variant]
	}
	deriving (Show, Eq)

instance Message MethodError where
	messageTypeCode _ = 3
	messageBody       = methodErrorBody
	messageHeaderFields m = concat
		[ [ HeaderErrorName (methodErrorName m)
		  , HeaderReplySerial (methodErrorSerial m)
		  ]
		, maybe' HeaderSender (methodErrorSender m)
		, maybe' HeaderDestination (methodErrorDestination m)
		]

-- | Get a human-readable description of the error, by returning the first
-- item in the error body if it's a string.
methodErrorMessage :: MethodError -> String
methodErrorMessage err = fromMaybe "(no error message)" $ do
	field <- listToMaybe (methodErrorBody err)
	msg <- fromVariant field
	if null msg
		then Nothing
		else return msg

-- | Signals are broadcast by applications to notify other clients of some
-- event.
data Signal = Signal
	{
	-- | The path of the object that emitted this signal.
	  signalPath :: ObjectPath
	
	-- | The interface that this signal belongs to.
	, signalInterface :: InterfaceName
	
	-- | The name of this signal.
	, signalMember :: MemberName
	
	-- | The name of the application that emitted this signal.
	--
	-- Most users will just leave this empty, because the bus overwrites
	-- the sender for security reasons. Setting the sender manually is
	-- used for peer-peer connections.
	--
	-- Defaults to @Nothing@.
	, signalSender :: Maybe BusName
	
	-- | The name of the application to emit the signal to. If @Nothing@,
	-- the signal is sent to any application that has registered an
	-- appropriate match rule.
	--
	-- Defaults to @Nothing@.
	, signalDestination :: Maybe BusName
	
	-- | Additional information about the signal, such as the new value
	-- or the time.
	--
	-- Defaults to @[]@.
	, signalBody :: [Variant]
	}
	deriving (Show, Eq)

instance Message Signal where
	messageTypeCode _ = 4
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
-- the bus. Each value contains the message's 'Serial'.
--
-- If casing against these constructors, always include a default case to
-- handle messages of an unknown type. New message types may be added to the
-- D-Bus specification, and applications should handle them gracefully by
-- either ignoring or logging them.
data ReceivedMessage
	= ReceivedMethodCall Serial MethodCall
	| ReceivedMethodReturn Serial MethodReturn
	| ReceivedMethodError Serial MethodError
	| ReceivedSignal Serial Signal
	| ReceivedUnknown Serial UnknownMessage
	deriving (Show, Eq)
