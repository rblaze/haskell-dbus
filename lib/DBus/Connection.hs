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

module DBus.Connection
	( Connection
	, ConnectionError
	, connect
	, disconnect
	, send
	, receive
	
	-- * Authentication
	, Mechanism
	, mechanism
	, external
	
	-- * Transports
	, Transport
	, Socket
	, transport
	, socket
	, socketPut
	, socketGet
	
	, unix
	, tcp
	) where

import           Control.Concurrent
import           Control.Monad (when)
import           Data.IORef

import           DBus.Address
import           DBus.Connection.Authentication
import           DBus.Connection.Error
import           DBus.Connection.Transport
import           DBus.Message
import           DBus.Wire

-- | A 'Connection' is an opaque handle to an open D&#8208;Bus channel, with
-- an internal state for maintaining the current message serial.
data Connection = Connection
	{ connectionAddress :: Address
	, connectionSocket :: Socket
	, connectionSerial :: IORef Serial
	, connectionReadLock :: MVar ()
	, connectionWriteLock :: MVar ()
	}

-- | Open a connection to some address, using a given authentication
-- mechanism. If the connection fails, a 'ConnectionError' will be thrown.
connect :: [Transport] -> [Mechanism] -> Address -> IO Connection
connect transports mechanisms addr = do
	msock <- connectTransport transports addr
	sock <- case msock of
		Just s -> return s
		Nothing -> connectionError (concat
			[ "Unknown address method: "
			, show (addressMethod addr)
			])
	authed <- authenticate sock mechanisms
	when (not authed)
		(connectionError "Authentication failed")
	
	serial <- newIORef (Serial 1)
	readLock <- newMVar ()
	writeLock <- newMVar ()
	return (Connection addr sock serial
	                   readLock writeLock)

-- | Close an open connection. Once closed, the 'Connection' is no longer
-- valid and must not be used.
disconnect :: Connection -> IO ()
disconnect = socketClose . connectionSocket

instance Show Connection where
	showsPrec _ x =
		showString "<Connection " .
		shows (connectionAddress x) .
		showString ">"

-- | Send a single message, with a generated 'Serial'. The second parameter
-- exists to prevent race conditions when registering a reply handler; it
-- receives the serial the message /will/ be sent with, before it&#8217;s
-- actually sent.
--
-- Only one message may be sent at a time; if multiple threads attempt to
-- send messages in parallel, one will block until after the other has
-- finished.
send :: Message msg => Connection -> msg -> (Serial -> IO a) -> IO (Either MarshalError a)
send connection msg io = do
	serial <- nextSerial connection
	case marshalMessage LittleEndian serial msg of
		Right bytes -> do
			let sock = connectionSocket connection
			let lock = connectionWriteLock connection
			result <- io serial
			withMVar lock (\_ -> socketPut sock bytes)
			return (Right result)
		Left err -> return (Left err)

nextSerial :: Connection -> IO Serial
nextSerial connection = atomicModifyIORef
	(connectionSerial connection)
	(\serial@(Serial x) -> (Serial (x + 1), serial))

-- | Receive the next message from the connection, blocking until one is
-- available.
--
-- Only one message may be received at a time; if multiple threads attempt
-- to receive messages in parallel, one will block until after the other has
-- finished.
receive :: Connection -> IO (Either UnmarshalError ReceivedMessage)
receive connection = do
	let sock = connectionSocket connection
	let lock = connectionReadLock connection
	withMVar lock (\_ -> unmarshalMessageM (socketGet sock))
