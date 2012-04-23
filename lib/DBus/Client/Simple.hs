{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

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

module DBus.Client.Simple
	(
	-- * Main module
	  module DBus
	
	-- * Clients
	, Client
	, connectSystem
	, connectSession
	, connectStarter
	, disconnect
	, emit
	
	-- * Proxies
	, Proxy
	, proxy
	, call
	, DBus.Client.Simple.listen
	
	-- * Name reservation
	, RequestNameFlag(..)
	, RequestNameReply(..)
	, ReleaseNameReply(..)
	, requestName
	, releaseName
	
	-- * Exporting objects
	, Method
	, AutoSignature
	, AutoReply
	, method
	, export
	, throwError
	) where

import           Control.Exception (throwIO)
import           Data.Bits ((.|.))
import qualified Data.Text -- for haddock
import qualified Data.Set
import           Data.Word (Word32)

import           DBus
import           DBus.Client hiding (call, method, emit, export)
import qualified DBus.Client
import           DBus.Connection.Error
import           DBus.Constants (errorInvalidParameters)
import           DBus.Util (maybeIndex)

-- | Connect to the bus specified in the environment variable
-- @DBUS_SESSION_BUS_ADDRESS@, which must be set.
connectSession :: IO Client
connectSession = do
	env <- getSessionAddress
	case env of
		Nothing -> connectionError (concat
			[ "connectSession: DBUS_SESSION_BUS_ADDRESS is"
			, " missing or invalid."
			])
		Just addr -> do
			ret <- DBus.Client.connect addr
			case ret of
				Left err -> throwIO err
				Right client -> return client

-- | Connect to the bus specified in the environment variable
-- @DBUS_SYSTEM_BUS_ADDRESS@, or to
-- @unix:path=\/var\/run\/dbus\/system_bus_socket@ if @DBUS_SYSTEM_BUS_ADDRESS@
-- is not set.
connectSystem :: IO Client
connectSystem = do
	env <- getSystemAddress
	case env of
		Nothing -> connectionError (concat
			[ "connectSession: DBUS_SYSTEM_BUS_ADDRESS is"
			, " invalid."
			])
		Just addr -> do
			ret <- DBus.Client.connect addr
			case ret of
				Left err -> throwIO err
				Right client -> return client

-- | Connect to the bus specified in the environment variable
-- @DBUS_STARTER_ADDRESS@, which must be set.
connectStarter :: IO Client
connectStarter = do
	env <- getStarterAddress
	case env of
		Nothing -> connectionError (concat
			[ "connectSession: DBUS_STARTER_BUS_ADDRESS is"
			, " missing or invalid."
			])
		Just addr -> do
			ret <- DBus.Client.connect addr
			case ret of
				Left err -> throwIO err
				Right client -> return client

data Proxy = Proxy Client BusName ObjectPath

proxy :: Client -> BusName -> ObjectPath -> IO Proxy
proxy client dest path = return (Proxy client dest path)

call_ :: Client -> MethodCall -> IO MethodReturn
call_ client msg = do
	result <- DBus.Client.call client msg
	case result of
		Left err -> connectionError ("Call failed: " ++ Data.Text.unpack (methodErrorMessage err))
		Right ret -> return ret

call :: Proxy -> InterfaceName -> MemberName -> [Variant] -> IO [Variant]
call (Proxy client dest path) iface member body = do
	reply <- call_ client $ MethodCall
		{ methodCallDestination = Just dest
		, methodCallMember = member
		, methodCallInterface = Just iface
		, methodCallPath = path
		, methodCallFlags = Data.Set.empty
		, methodCallBody = body
		}
	return (methodReturnBody reply)

emit :: Client -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> IO ()
emit client path iface member body = DBus.Client.emit client $ Signal
	{ signalDestination = Nothing
	, signalPath = path
	, signalInterface = iface
	, signalMember = member
	, signalBody = body
	}

listen :: Proxy -> InterfaceName -> MemberName -> (BusName -> Signal -> IO ()) -> IO ()
listen (Proxy client dest path) iface member = DBus.Client.listen client (MatchRule
	{ matchSender = Just dest
	, matchInterface = Just iface
	, matchMember = Just member
	, matchPath = Just path
	, matchDestination = Nothing
	})

data RequestNameFlag
	= AllowReplacement
	| ReplaceExisting
	| DoNotQueue
	deriving (Show)

data RequestNameReply
	= PrimaryOwner
	| InQueue
	| Exists
	| AlreadyOwner
	deriving (Show)

data ReleaseNameReply
	= Released
	| NonExistent
	| NotOwner
	deriving (Show)

encodeFlags :: [RequestNameFlag] -> Word32
encodeFlags = foldr (.|.) 0 . map flagValue where
	flagValue AllowReplacement = 0x1
	flagValue ReplaceExisting  = 0x2
	flagValue DoNotQueue       = 0x4

requestName :: Client -> BusName -> [RequestNameFlag] -> IO RequestNameReply
requestName client name flags = do
	bus <- proxy client "org.freedesktop.DBus" "/org/freedesktop/DBus"
	reply <- call bus "org.freedesktop.DBus" "RequestName"
		[ toVariant name
		, toVariant (encodeFlags flags)
		]
	case (maybeIndex reply 0 >>= fromVariant :: Maybe Word32) of
		Just 1 -> return PrimaryOwner
		Just 2 -> return InQueue
		Just 3 -> return Exists
		Just 4 -> return AlreadyOwner
		_ -> connectionError "Call failed: received invalid reply"

releaseName :: Client -> BusName -> IO ReleaseNameReply
releaseName client name = do
	bus <- proxy client "org.freedesktop.DBus" "/org/freedesktop/DBus"
	reply <- call bus "org.freedesktop.DBus" "ReleaseName"
		[ toVariant name
		]
	case (maybeIndex reply 0 >>= fromVariant :: Maybe Word32) of
		Just 1 -> return Released
		Just 2 -> return NonExistent
		Just 3 -> return NotOwner
		_ -> connectionError "Call failed: received invalid reply"

-- | Used to automatically generate method signatures for introspection
-- documents. To support automatic signatures, a method#8217;s parameters and
-- return value must all be instances of 'IsValue'.
--
-- This class maps Haskell idioms to D&#8208;Bus; it is therefore unable to
-- generate some signatures. In particular, it does not support methods
-- which accept/return a single structure, or single&#8208;element structures.
-- It also cannot generate signatures for methods with parameters or return
-- values which are only instances of 'IsVariant'. For these cases, please
-- use 'DBus.Client.method'.
--
-- To match common Haskell use, if the return value is a tuple, it will be
-- converted to a list of return values.
class AutoSignature a where
	funTypes :: a -> ([Type], [Type])

instance AutoSignature (IO ()) where
	funTypes _ = ([], [])

instance IsValue a => AutoSignature (IO a) where
	funTypes io = ([], case ioT io undefined of
		(_, t) -> case t of
			TypeStructure ts -> ts
			_ -> [t])

ioT :: IsValue a => IO a -> a -> (a, Type)
ioT _ a = (a, typeOf a)

instance (IsValue a, AutoSignature fun) => AutoSignature (a -> fun) where
	funTypes fn = case valueT undefined of
		(a, t) -> case funTypes (fn a) of
			(ts, ts') -> (t : ts, ts')

valueT :: IsValue a => a -> (a, Type)
valueT a = (a, typeOf a)

-- | Used to automatically generate a 'Reply' from a return value. See
-- 'AutoSignature' for some caveats about supported signatures.
--
-- To match common Haskell use, if the return value is a tuple, it will be
-- converted to a list of return values.
class AutoReply fun where
	apply :: fun -> [Variant] -> Maybe (IO [Variant])

instance AutoReply (IO ()) where
	apply io [] = Just (io >> return [])
	apply _ _ = Nothing

instance IsVariant a => AutoReply (IO a) where
	apply io [] = Just (do
		var <- fmap toVariant io
		case fromVariant var of
			Just struct -> return (structureItems struct)
			Nothing -> return [var])
	apply _ _ = Nothing

instance (IsVariant a, AutoReply fun) => AutoReply (a -> fun) where
	apply _ [] = Nothing
	apply fn (v:vs) = case fromVariant v of
		Just v' -> apply (fn v') vs
		Nothing -> Nothing

-- | Prepare a Haskell function for export. This automatically detects the
-- function#8217;s type signature; see 'AutoSignature' and 'AutoReply'.
--
-- To manage the type signature and marshaling yourself, use
-- 'DBus.Client.method' instead.
method :: (AutoSignature fun, AutoReply fun) => InterfaceName -> MemberName -> fun -> Method
method iface name fun = DBus.Client.method iface name inSig outSig io where
	(typesIn, typesOut) = funTypes fun
	inSig = case signature typesIn of
		Just sig -> sig
		Nothing -> invalid "input"
	outSig = case signature typesOut of
		Just sig -> sig
		Nothing -> invalid "output"
	io vs = case apply fun vs of
		Nothing -> return (ReplyError errorInvalidParameters [])
		Just io' -> fmap ReplyReturn io'
	
	invalid label = error (concat
		[ "Method "
		, Data.Text.unpack (interfaceNameText iface)
		, "."
		, Data.Text.unpack (memberNameText name)
		, " has an invalid "
		, label
		, " signature."])

-- | Export the given functions under the given 'ObjectPath' and
-- 'InterfaceName'. The functions may accept/return any types that are
-- instances of 'IsValue'; see 'AutoSignature'.
--
-- @
--sayHello :: Text -> IO Text
--sayHello name = return ('Data.Text.concat' [\"Hello \", name, \"!\"])
--
--export client \"/hello_world\"
--    [ 'method' \"com.example.HelloWorld\" \"Hello\" sayHello
--    ]
-- @
export :: Client -> ObjectPath -> [Method] -> IO ()
export = DBus.Client.export
