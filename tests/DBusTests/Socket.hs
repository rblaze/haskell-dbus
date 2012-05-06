{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
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

module DBusTests.Socket (test_Socket) where

import           Test.Chell

import           Control.Concurrent
import           Control.Exception (finally)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Network as N
import           System.IO

import           DBus.Socket
import           DBus.Util (readUntil)

import           DBusTests.Util (listenRandomIPv4)

test_Socket :: Suite
test_Socket = suite "Socket"
	[ test_Open
	]

test_Open :: Suite
test_Open = assertions "open" $ do
	(addr, networkSocket) <- listenRandomIPv4
	opened <- withDummyServer networkSocket (DBus.Socket.open addr)
	$assert (right opened)
	let Right s = opened
	
	$expect (equal (socketAddress s) addr)

readChar8 :: Handle -> IO Char
readChar8 h = fmap (Char8.head) (Data.ByteString.hGet h 1)

newtype DummyServer = DummyServer (MVar ())

withDummyServer :: MonadIO m => N.Socket -> IO a -> m a
withDummyServer sock io = liftIO go where
	go = do
		mvar <- newEmptyMVar
		_ <- forkIO (run mvar)
		finally io (putMVar mvar ())
	
	run mvar = do
		(h, _, _) <- N.accept sock
		hSetBuffering h NoBuffering
		
		-- authentication sends '\x00', then AUTH EXTERNAL ...
		_ <- readChar8 h
		_ <- readUntil "\r\n" (readChar8 h)
		Data.ByteString.hPut h "OK blah blah blah\r\n"
		
		_ <- readMVar mvar
		hClose h
		N.sClose sock
