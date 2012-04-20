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

module DBus.Address
	( Address
	, addressMethod
	, addressParameters
	, address
	, addresses
	, addressText
	
	-- * Environmental addresses
	, getSystem
	, getSession
	, getStarter
	) where

import qualified Control.Exception
import           Data.Char (ord, chr)
import qualified Data.Map
import           Data.Map (Map)
import qualified Data.Text
import           Data.Text (Text)
import qualified System.Environment
import           Text.Printf (printf)
import           Text.ParserCombinators.Parsec hiding (runParser)

import           DBus.Util (hexToInt, void)
import           DBus.Types.Internal (runParser)

data Address = Address Text (Map Text Text)
	deriving (Eq)

addressMethod :: Address -> Text
addressMethod (Address x _ ) = x

addressParameters :: Address -> Map Text Text
addressParameters (Address _ x) = x

address :: Text -> Maybe Address
address = runParser $ do
	addr <- parseAddress
	eof
	return addr

parseAddress :: Parser Address
parseAddress = parser where
	parser = do
		method <- many (noneOf ":;")
		void (char ':')
		params <- sepEndBy param (char ',')
		return (Address
			(Data.Text.pack method)
			(Data.Map.fromList params))
	
	param = do
		key <- many1 (noneOf "=;,")
		void (char '=')
		value <- many1 valueChar
		let pack = Data.Text.pack
		return (pack key, pack value)
	
	valueChar = encoded <|> unencoded
	encoded = do
		void (char '%')
		hex <- count 2 hexDigit
		return (chr (hexToInt hex))
	unencoded = oneOf optionallyEncoded

optionallyEncoded :: [Char]
optionallyEncoded = concat
	[ ['0'..'9']
	, ['a'..'z']
	, ['A'..'Z']
	, "-_/\\*."
	]

addresses :: Text -> Maybe [Address]
addresses = runParser $ do
	xs <- sepEndBy1 parseAddress (char ';')
	eof
	return xs

instance Show Address where
	showsPrec d x = showParen (d > 10) $
		showString "Address " .
		shows (addressText x)

addressText :: Address -> Text
addressText addr = Data.Text.concat chunks where
	chunks = [ addressMethod addr, ":"
	         , paramsText]
	
	params = addressParameters addr
	paramsText = Data.Text.intercalate "," $ do
		(k, v) <- Data.Map.toList params
		let k' = Data.Text.unpack k
		let v' = Data.Text.unpack v
		let encoded = concatMap encode v'
		let str = concat [k', "=", encoded]
		return (Data.Text.pack str)
	
	encode c = if elem c optionallyEncoded
		then [c]
		else printf "%%%02X" (ord c)

getenv :: String -> IO (Maybe Text)
getenv name = Control.Exception.catch
	(fmap (Just . Data.Text.pack) (System.Environment.getEnv name))
	(\(Control.Exception.SomeException _) -> return Nothing)

getSystem :: IO (Maybe [Address])
getSystem = do
	let system = "unix:path=/var/run/dbus/system_bus_socket"
	env <- getenv "DBUS_SYSTEM_BUS_ADDRESS"
	return (addresses (maybe system id env))

getSession :: IO (Maybe [Address])
getSession = do
	env <- getenv "DBUS_SESSION_BUS_ADDRESS"
	return (env >>= addresses)

getStarter :: IO (Maybe [Address])
getStarter = do
	env <- getenv "DBUS_STARTER_BUS_ADDRESS"
	return (env >>= addresses)
