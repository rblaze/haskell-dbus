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
	( Transport (..)
	, findTransport
	, Connection
	, connect
	, send
	, recv
	) where

import qualified Control.Concurrent as C
import qualified Control.Exception as E
import Data.Word (Word32)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import qualified Data.Map as Map
import Data.Typeable (Typeable, cast)
import qualified Network as N
import qualified System.IO as I
import qualified DBus.Types as T
import DBus.Message (Message, ReceivedMessage, marshal, unmarshal)
import qualified DBus.Bus.Address as A
import DBus.Protocol.Authentication (authenticate)
\end{code}
}

\section{Transports}

A transport is anything which can be used to construct a (send, recv)
computation pair.

\begin{code}
data Transport = Transport
	{ transportAddress :: A.Address
	, transportConnect :: IO
		( L.ByteString -> IO ()
		, Word32 -> IO L.ByteString
		)
	}
\end{code}

\begin{code}
instance Show Transport where
	showsPrec d (Transport a _) = showParen (d > 10) $
		showString' ["<transport \"", A.strAddress a, "\">"] where
		showString' = foldr (.) id . map showString
\end{code}

\begin{code}
findTransport :: A.Address -> Transport
findTransport a = transport' (A.addressMethod a) a where
	transport' "unix" = unix
	transport' _      = unknownTransport
\end{code}

\subsection{UNIX}

known parameters:
 * path
 * abstract
 * tmpdir (only for listening)

If an invalid set of parameters is provided, the transport will still
be built, but will not be able to connect.

\begin{code}
data BadParameters = BadParameters String
	deriving (Show, Typeable)

instance E.Exception BadParameters where
	toException = E.SomeException
	fromException = cast

unix :: A.Address -> Transport
unix a = handleTransport a connect' where
	params = A.addressParameters a
	path = Map.lookup "path" params
	abstract = Map.lookup "abstract" params
	path' = case (path, abstract) of
		(Just _, Just _) -> E.throwIO (BadParameters "got path and abstract")
		(Nothing, Nothing) -> E.throwIO (BadParameters "need path or abstract")
		(Just x, Nothing) -> return x
		(Nothing, Just x) -> return $ '\x00':x
	connect' = N.connectTo "localhost" =<< fmap N.UnixSocket path'
\end{code}

\subsection{TCP}

known parameters:
 * host (optional, default "localhost")
 * port
 * family (optional, choices are "ipv4" or "ipv6"

TCP support is TODO

\begin{otherCode}
tcp :: A.Address -> Transport
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

both UNIX and TCP use generic handle-based transport:

\begin{code}
handleTransport :: A.Address -> IO I.Handle -> Transport
handleTransport addr io = Transport addr $ do
	h <- io
	I.hSetBuffering h I.NoBuffering
	I.hSetBinaryMode h True
	let get' = L.hGet h .fromIntegral
	return (L.hPut h, get')
\end{code}

\subsection{Unknown transports}

\begin{code}
unknownTransport :: A.Address -> Transport
unknownTransport a = Transport a $ do
	let m = A.addressMethod a
	let err = "Unknown method " ++ show m ++
	          " in address " ++ (show . A.strAddress) a ++ "."
	
	E.throwIO . E.AssertionFailed $ err
\end{code}

\section{Connections}

\begin{code}
data Connection = Connection Transport (C.MVar T.Serial)
                             (L.ByteString -> IO ())
                             (Word32 -> IO L.ByteString)

instance Show Connection where
	showsPrec d (Connection (Transport a _) _ _ _) = showParen (d > 10) $
		showString' ["<connection \"", A.strAddress a, "\">"] where
		showString' = foldr (.) id . map showString
\end{code}

\begin{code}
connect :: Transport -> IO Connection
connect t@(Transport _ connect') = do
	(put, get) <- connect'
	serialMVar <- C.newMVar T.firstSerial
	let c = Connection t serialMVar put get
	let put' = put . fromString
	let get' = fmap toString . get
	authenticate put' get'
	return c
\end{code}

\begin{code}
send :: Message a => Connection -> (T.Serial -> IO b) -> a -> IO b
send (Connection _ mvar put _) io msg = withSerial mvar $ \serial -> do
	x <- io serial
	put . marshal T.LittleEndian serial $ msg
	return x

withSerial :: C.MVar T.Serial -> (T.Serial -> IO a) -> IO a
withSerial m io = E.block $ do
	s <- C.takeMVar m
	let s' = T.nextSerial s
	x <- E.unblock (io s) `E.onException` C.putMVar m s'
	C.putMVar m s'
	return x
\end{code}

\begin{code}
recv :: Connection -> IO (Either String ReceivedMessage)
recv (Connection _ _ _ get) = unmarshal get
\end{code}
