% Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
% 
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\ignore{
\begin{code}
module DBus.Bus
	( getSystemBus
	, getSessionBus
	, getFirstBus
	, getBus
	) where

import qualified Control.Exception as E
import Control.Monad (when)

import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set

import System.Environment (getEnv)
import qualified DBus.Bus.Address as A
import qualified DBus.Bus.Connection as C
import qualified DBus.Message as M
import qualified DBus.Types as T
\end{code}
}

\section{Connecting to a message bus}

Connecting to a message bus is a bit more involved than just connecting
over an app-to-app connection: the bus must be notified of the new client,
using a "hello message", before it will begin forwarding messages.

\begin{code}
getBus :: A.Address -> IO (C.Connection, T.BusName)
getBus addr = do
	c <- C.connect addr
	name <- sendHello c
	return (c, name)
\end{code}

Optionally, multiple addresses may be provided. The first successfully
connected bus will be returned.

\begin{code}
getFirstBus :: [A.Address] -> IO (C.Connection, T.BusName)
getFirstBus as = getFirstBus' as as

getFirstBus' :: [A.Address] -> [A.Address] -> IO (C.Connection, T.BusName)
getFirstBus' orig     [] = E.throwIO $ C.NoWorkingAddress orig
getFirstBus' orig (a:as) = E.catch (getBus a) onError where
	onError :: E.SomeException -> IO (C.Connection, T.BusName)
	onError _ = getFirstBus' orig as
\end{code}

\subsection{Default connections}

Two default buses are defined, the ``system'' and ``session'' buses. The system
bus is global for the OS, while the session bus runs only for the duration
of the user's session.

\begin{code}
getSystemBus :: IO (C.Connection, T.BusName)
getSystemBus = getBus addr where
	systemBusPath = "unix:path=/var/run/dbus/system_bus_socket"
	Just [addr] = A.parseAddresses systemBusPath
\end{code}

\begin{code}
getSessionBus :: IO (C.Connection, T.BusName)
getSessionBus = do
	env <- getEnv "DBUS_SESSION_BUS_ADDRESS"
	
	case A.parseAddresses env of
		Just [x] -> getBus x
		Just  x  -> getFirstBus x
		_        -> E.throwIO $ C.InvalidAddress env
\end{code}

\subsection{Sending the ``hello'' message}

\begin{code}
hello :: M.MethodCall
hello = M.MethodCall
	(T.mkObjectPath' "/org/freedesktop/DBus")
	(T.mkMemberName' "Hello")
	(T.mkInterfaceName "org.freedesktop.DBus")
	(T.mkBusName "org.freedesktop.DBus")
	Set.empty
	[]
\end{code}

\begin{code}
sendHello :: C.Connection -> IO T.BusName
sendHello c = do
	serial <- C.send c return hello
	reply <- waitForReply c serial
	let name = case M.methodReturnBody reply of
		[x] -> T.fromVariant x
		_   -> Nothing
	
	when (isNothing name)
		(E.throwIO . C.ProtocolException $
		 "Received inappropriate reply to Hello().")
	
	return . fromJust $ name
\end{code}

\begin{code}
waitForReply :: C.Connection -> T.Serial -> IO M.MethodReturn
waitForReply c serial = do
	msg <- C.receive c
	case msg of
		(M.ReceivedMethodReturn _ _ reply) ->
			if M.methodReturnSerial reply == serial
				then return reply
				else waitForReply c serial
		_ -> waitForReply c serial
\end{code}

