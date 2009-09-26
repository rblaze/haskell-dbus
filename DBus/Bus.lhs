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
{-# LANGUAGE DeriveDataTypeable #-}
module DBus.Bus (
	 getSystemBus
	,getSessionBus
	) where
import Data.Typeable (Typeable, cast)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import System.Environment (getEnv)
import qualified Control.Exception as E
import qualified DBus.Bus.Address as A
import qualified DBus.Bus.Connection as C
import qualified DBus.Message as M
import qualified DBus.Types as T
\end{code}
}

\begin{code}
getBus :: A.Address -> IO (C.Connection, T.BusName)
getBus addr = do
	c <- C.connect . C.findTransport $ addr
	C.send c return hello
	reply <- C.recv c >>= \x -> case x of
		Right x'  -> return x'
		Left  err -> E.throwIO . E.AssertionFailed $ err
	case findBusName reply of
		Just x -> return (c, x)
		Nothing -> E.throwIO . E.AssertionFailed $ "Received inappropriate reply to Hello()."

findBusName :: M.ReceivedMessage -> Maybe T.BusName
findBusName (M.ReceivedMethodReturn _ _ msg) = name where
	name = case M.methodReturnBody msg of
		[x] -> T.fromVariant x
		_   -> Nothing
findBusname _ = Nothing
\end{code}

\section{Default connections}

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
		Just (addr:_) -> getBus addr
		Nothing -> E.throwIO $ BadAddress env

data BadAddress = BadAddress String
	deriving (Show, Typeable)

instance E.Exception BadAddress where
	toException = E.SomeException
	fromException = cast
\end{code}

\begin{code}
hello :: M.MethodCall
hello = M.MethodCall
	(fromJust . T.mkObjectPath $ "/org/freedesktop/DBus")
	(fromJust . T.mkMemberName $ "Hello")
	(T.mkInterfaceName "org.freedesktop.DBus")
	(T.mkBusName "org.freedesktop.DBus")
	(Set.empty)
	[]
\end{code}
