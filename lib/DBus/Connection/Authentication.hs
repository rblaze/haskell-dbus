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

module DBus.Connection.Authentication
	( Mechanism
	, mechanism
	, authenticate
	, external
	) where

import           Prelude hiding (getLine, head)
import           Control.Monad (liftM)
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import           Data.Char (ord)
import qualified System.Posix.User
import           Text.Printf (printf)

import           DBus.Connection.Transport
import           DBus.Util (readUntil, dropEnd)

authenticate :: Socket
             -> [Mechanism]
             -> IO Bool
authenticate s mechanisms = do
	socketPut s (Data.ByteString.pack [0])
	let loop [] = return False
	    loop ((Mechanism m):next) = do
	    	success <- m s
	    	if success
	    		then return True
	    		else loop next
	loop mechanisms

newtype Mechanism = Mechanism (Socket -> IO Bool)

mechanism :: (Socket -> IO Bool) -> Mechanism
mechanism = Mechanism

data Auth a = Auth
	{ unAuth :: Socket -> IO a
	}

instance Monad Auth where
	return a = Auth (\_ -> return a)
	m >>= k = Auth $ \s -> do
		x <- unAuth m s
		unAuth (k x) s

liftIO :: IO a -> Auth a
liftIO io = Auth (\_ -> io)

putLine :: String -> Auth ()
putLine line = Auth $ \s -> do
	let pack = Data.ByteString.Char8.pack
	socketPut s (pack (line ++ "\r\n"))

getLine :: Auth String
getLine = Auth $ \s -> do
	let head = Data.ByteString.Char8.head
	let getchr = liftM head (socketGet s 1)
	raw <- readUntil "\r\n" getchr
	return (dropEnd 2 raw)

external :: Mechanism
external = Mechanism $ unAuth $ do
	uid <- liftIO System.Posix.User.getRealUserID
	let token = concatMap (printf "%02X" . ord) (show uid)
	putLine ("AUTH EXTERNAL " ++ token)
	resp <- getLine
	case takeWhile (/= ' ') resp of
		"OK" -> do
			putLine "BEGIN"
			return True
		_ -> return False
