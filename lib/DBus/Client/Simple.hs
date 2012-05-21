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

module DBus.Client.Simple
	(
	-- * Main module
	  module DBus
	
	-- * Clients
	, Client
	, disconnect
	, emit
	
	-- * Proxies
	, Proxy
	, proxy
	, call
	, DBus.Client.Simple.listen
	) where

import           Control.Exception (throwIO)
import qualified Data.Text -- for haddock
import qualified Data.Set

import           DBus
import           DBus.Client hiding (call, method, emit, export)
import qualified DBus.Client

data Proxy = Proxy Client BusName ObjectPath

proxy :: Client -> BusName -> ObjectPath -> IO Proxy
proxy client dest path = return (Proxy client dest path)

call_ :: Client -> MethodCall -> IO MethodReturn
call_ client msg = do
	result <- DBus.Client.call client msg
	case result of
		Left err -> throwIO (clientError ("Call failed: " ++ Data.Text.unpack (methodErrorMessage err)))
		Right ret -> return ret

call :: Proxy -> InterfaceName -> MemberName -> [Variant] -> IO [Variant]
call (Proxy client dest path) iface member body = do
	reply <- call_ client $ MethodCall
		{ methodCallPath = path
		, methodCallMember = member
		, methodCallInterface = Just iface
		, methodCallSender = Nothing
		, methodCallDestination = Just dest
		, methodCallFlags = Data.Set.empty
		, methodCallBody = body
		}
	return (methodReturnBody reply)

emit :: Client -> ObjectPath -> InterfaceName -> MemberName -> [Variant] -> IO ()
emit client path iface member body = DBus.Client.emit client $ Signal
	{ signalPath = path
	, signalMember = member
	, signalInterface = iface
	, signalSender = Nothing
	, signalDestination = Nothing
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
