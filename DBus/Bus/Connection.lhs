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
module DBus.Bus.Connection
	( Connection
	, ConnectionException (..)
	, ProtocolException (..)
	, connect
	, send
	, receive
	) where

import qualified Control.Concurrent as C
import qualified Control.Exception as E

import Data.Word (Word32)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import qualified Data.Map as Map
import Data.Typeable (Typeable)

import qualified Network as N
import qualified System.IO as I

import qualified DBus.Types as T
import DBus.Message (Message, ReceivedMessage, marshal, unmarshal)
import qualified DBus.Bus.Address as A
import DBus.Internal.Authentication (authenticate)
\end{code}
}

\section{Connections}

A {\tt Connection} is an opaque handle to an open DBus channel, with
an internal state for maintaining the current message serial.

\begin{code}
data Connection = Connection A.Address Transport (C.MVar T.Serial)
\end{code}

While not particularly useful for other functions, being able to {\tt show}
a {\tt Connection} is useful when debugging.

\begin{code}
instance Show Connection where
	showsPrec d (Connection a _ _) = showParen (d > 10) $
		showString' ["<connection \"", A.strAddress a, "\">"] where
		showString' = foldr (.) id . map showString
\end{code}

A connection can be opened to any valid address, though actually connecting
might fail due to external factors.

\begin{code}
connect :: A.Address -> IO Connection
connect a = do
	t <- connectTransport a
	let putS = transportSend t . fromString
	let getS = fmap toString . transportRecv t
	authenticate putS getS
	serialMVar <- C.newMVar T.firstSerial
	return $ Connection a t serialMVar
\end{code}

Sending a message will increment the connection's internal serial state.
The second parameter is present to allow registration of a callback before
the message has actually been sent, which avoids race conditions in
multi-threaded clients.

\begin{code}
send :: Message a => Connection -> (T.Serial -> IO b) -> a -> IO b
send (Connection _ t mvar) io msg = withSerial mvar $ \serial -> do
	x <- io serial
	transportSend t . marshal T.LittleEndian serial $ msg
	return x

withSerial :: C.MVar T.Serial -> (T.Serial -> IO a) -> IO a
withSerial m io = E.block $ do
	s <- C.takeMVar m
	let s' = T.nextSerial s
	x <- E.unblock (io s) `E.onException` C.putMVar m s'
	C.putMVar m s'
	return x
\end{code}

Messages are received wrapped in a {\tt ReceivedMessage} value. If an error
is encountered while unmarshaling, an exception will be thrown.

\begin{code}
receive :: Connection -> IO ReceivedMessage
receive (Connection _ t _) = do
	either' <- unmarshal $ transportRecv t
	case either' of
		Right x -> return x
		Left err -> E.throwIO . ProtocolException $ err
\end{code}

\section{Transports}

A transport is anything which can send and receive bytestrings, typically
over a socket.

\begin{code}
data Transport = Transport
	{ transportSend :: L.ByteString -> IO ()
	, transportRecv :: Word32 -> IO L.ByteString
	}
\end{code}

\begin{code}
connectTransport :: A.Address -> IO Transport
connectTransport a = transport' (A.addressMethod a) a where
	transport' "unix" = unix
	transport' _      = unknownTransport
\end{code}

\subsection{UNIX}

The {\sc unix} transport accepts two parameters: {\tt path}, which is a
simple filesystem path, and {\tt abstract}, which is a path in the
Linux-specific abstract domain. One, and only one, of these parameters must
be specified.

\begin{code}
unix :: A.Address -> IO Transport
unix a = handleTransport . N.connectTo "localhost" =<< port where
	params = A.addressParameters a
	path = Map.lookup "path" params
	abstract = Map.lookup "abstract" params
	
	tooMany = "Only one of `path' or `abstract' may be specified for the"
	          ++ " `unix' method."
	tooFew = "One of `path' or `abstract' must be specified for the"
	         ++ " `unix' transport."
	
	port = fmap N.UnixSocket path'
	path' = case (path, abstract) of
		(Just _, Just _) -> E.throwIO $ BadParameters a tooMany
		(Nothing, Nothing) -> E.throwIO $ BadParameters a tooFew
		(Just x, Nothing) -> return x
		(Nothing, Just x) -> return $ '\x00':x
\end{code}

\subsection{TCP}

known parameters:

\begin{itemize}
\item {\tt host} (optional, default "{\tt localhost}")
\item {\tt port}
\item {\tt family} (optional, choices are "{\tt ipv4}" or "{\tt ipv6}"
\end{itemize}

TCP support is TODO

\begin{otherCode}
tcp :: A.Address -> IO Transport
tcp a@(A.Address _ params) = handleTransport a connect' where
	host = lookup "host" params
	port = parsePort =<< lookup "post" params
	family = parseFamily =<< lookup "family" params
	connect' = do
		-- check host
		-- check port
		-- check family
		-- return handle
parsePort :: String -> Maybe PortNumber

parseFamily :: String -> Maybe Family
\end{otherCode}

\subsection{Generic handle-based transport}

Both UNIX and TCP are backed by standard handles, and can therefore use
a shared handle-based transport backend.

\begin{code}
handleTransport :: IO I.Handle -> IO Transport
handleTransport io = do
	h <- io
	I.hSetBuffering h I.NoBuffering
	I.hSetBinaryMode h True
	return $ Transport (L.hPut h) (L.hGet h . fromIntegral)
\end{code}

\subsection{Unknown transports}

If a method has no known transport, attempting to connect using it will
just result in an exception.

\begin{code}
unknownTransport :: A.Address -> IO Transport
unknownTransport = E.throwIO . UnknownMethod
\end{code}

\subsection{Errors}

If connecting to DBus fails, a {\tt ConnectionException} will be thrown.
The constructor describes which exception occurred.

\begin{code}
data ConnectionException =
	  InvalidAddress String
	| BadParameters A.Address String
	| UnknownMethod A.Address
	| NoWorkingAddress [A.Address]
	deriving (Show, Typeable)

instance E.Exception ConnectionException
\end{code}

If a message cannot be unmarshaled --- for example, due to malformed or
truncated input --- a {\tt ProtocolException} will be thrown.

\begin{code}
data ProtocolException = ProtocolException String
	deriving (Show, Typeable)

instance E.Exception ProtocolException
\end{code}

