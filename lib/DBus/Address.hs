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

module DBus.Address where

import qualified Control.Exception
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import           Data.Char (ord, chr)
import qualified Data.Map
import           Data.Map (Map)
import qualified System.Environment
import           Text.Printf (printf)
import           Text.ParserCombinators.Parsec hiding (runParser)

import           DBus.Util (hexToInt, void, parseBytes)

-- | When a D-Bus server must listen for connections, or a client must connect
-- to a server, the listening socket's configuration is specified with an
-- /address/. An address contains the /method/, which determines the
-- protocol and transport mechanism, and /parameters/, which provide
-- additional method-specific information about the address.
data Address = Address ByteString (Map ByteString ByteString)
	deriving (Eq)

addressMethod :: Address -> ByteString
addressMethod (Address x _ ) = x

addressParameters :: Address -> Map ByteString ByteString
addressParameters (Address _ x) = x

address :: ByteString -> Map ByteString ByteString -> Maybe Address
address method params = if validMethod method && validParams params
	then if ByteString.null method && Data.Map.null params
		then Nothing
		else Just (Address method params)
	else Nothing

validMethod :: ByteString -> Bool
validMethod = Char8.all validChar where
	validChar c = c /= ';' && c /= ':'

validParams :: Map ByteString ByteString -> Bool
validParams = all validItem . Data.Map.toList where
	validItem (k, v) = notNull k && notNull v && validKey k
	validKey = Char8.all validChar
	validChar c = c /= ';' && c /= ',' && c /= '='

notNull :: ByteString -> Bool
notNull = not . ByteString.null

optionallyEncoded :: [Char]
optionallyEncoded = concat
	[ ['0'..'9']
	, ['a'..'z']
	, ['A'..'Z']
	, ['-', '_', '/', '\\', '*', '.']
	]

formatAddress :: Address -> ByteString
formatAddress (Address method params) = bytes where
	bytes = ByteString.concat
		[ method, ":", csvParams]
	
	csvParams = ByteString.intercalate "," $ do
		(k, v) <- Data.Map.toList params
		let v' = Char8.concatMap escape v
		return (ByteString.concat [k, "=", v'])
	
	escape c = Char8.pack $ if elem c optionallyEncoded
		then [c]
		else printf "%%%02X" (ord c)

formatAddresses :: [Address] -> ByteString
formatAddresses = ByteString.intercalate ";" . map formatAddress

instance Show Address where
	showsPrec d x = showParen (d > 10) $
		showString "Address " .
		shows (formatAddress x)

parseAddress :: ByteString -> Maybe Address
parseAddress = parseBytes $ do
	addr <- parsecAddress
	eof
	return addr

parseAddresses :: ByteString -> Maybe [Address]
parseAddresses = parseBytes $ do
	addrs <- sepEndBy parsecAddress (char ';')
	eof
	return addrs

parsecAddress :: Parser Address
parsecAddress = p where
	p = do
		method <- many (noneOf ":;")
		void (char ':')
		params <- sepEndBy param (char ',')
		return (Address
			(Char8.pack method)
			(Data.Map.fromList params))
	
	param = do
		key <- many1 (noneOf "=;,")
		void (char '=')
		value <- many1 valueChar
		return (Char8.pack key, Char8.pack value)
	
	valueChar = encoded <|> unencoded
	encoded = do
		void (char '%')
		hex <- count 2 hexDigit
		return (chr (hexToInt hex))
	unencoded = oneOf optionallyEncoded

getSystemAddress :: IO (Maybe Address)
getSystemAddress = do
	let system = "unix:path=/var/run/dbus/system_bus_socket"
	env <- getenv "DBUS_SYSTEM_BUS_ADDRESS"
	return (parseAddress (maybe system id env))

getSessionAddress :: IO (Maybe Address)
getSessionAddress = do
	env <- getenv "DBUS_SESSION_BUS_ADDRESS"
	return (env >>= parseAddress)

getStarterAddress :: IO (Maybe Address)
getStarterAddress = do
	env <- getenv "DBUS_STARTER_ADDRESS"
	return (env >>= parseAddress)

getenv :: String -> IO (Maybe ByteString)
getenv name = Control.Exception.catch
	(fmap (Just . Char8.pack) (System.Environment.getEnv name))
	(\(Control.Exception.SomeException _) -> return Nothing)
