{-# Language LambdaCase #-}
-- Copyright (C) 2009-2012 John Millikin <john@john-millikin.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module DBus.Internal.Address where

import           Data.Char (digitToInt, ord, chr)
import           Data.Maybe (listToMaybe, fromMaybe)
import           Data.List (intercalate)
import qualified Data.Map
import           Data.Map (Map)
import           System.Environment (lookupEnv)
import           Text.Printf (printf)

import           Text.ParserCombinators.Parsec

-- | When a D-Bus server must listen for connections, or a client must connect
-- to a server, the listening socket's configuration is specified with an
-- /address/. An address contains the /method/, which determines the
-- protocol and transport mechanism, and /parameters/, which provide
-- additional method-specific information about the address.
data Address = Address String (Map String String)
    deriving (Eq)

addressMethod :: Address -> String
addressMethod (Address x _ ) = x

addressParameters :: Address -> Map String String
addressParameters (Address _ x) = x

-- | Try to convert a method string and parameter map to an 'Address'.
--
-- Returns 'Nothing' if the method or parameters are invalid.
address :: String -> Map String String -> Maybe Address
address method params = if validMethod method && validParams params
    then if null method && Data.Map.null params
        then Nothing
        else Just (Address method params)
    else Nothing

validMethod :: String -> Bool
validMethod = all validChar where
    validChar c = c /= ';' && c /= ':'

validParams :: Map String String -> Bool
validParams = all validItem . Data.Map.toList where
    validItem (k, v) = notNull k && notNull v && validKey k
    validKey = all validChar
    validChar c = c /= ';' && c /= ',' && c /= '='
    notNull = not . null

optionallyEncoded :: [Char]
optionallyEncoded = concat
    [ ['0'..'9']
    , ['a'..'z']
    , ['A'..'Z']
    , ['-', '_', '/', '\\', '*', '.']
    ]

-- | Convert an address to a string in the format expected by 'parseAddress'.
formatAddress :: Address -> String
formatAddress (Address method params) = concat [method, ":", csvParams] where
    csvParams = intercalate "," $ do
        (k, v) <- Data.Map.toList params
        let v' = concatMap escape v
        return (concat [k, "=", v'])

    escape c = if elem c optionallyEncoded
        then [c]
        else printf "%%%02X" (ord c)

-- | Convert a list of addresses to a string in the format expected by
-- 'parseAddresses'.
formatAddresses :: [Address] -> String
formatAddresses = intercalate ";" . map formatAddress

instance Show Address where
    showsPrec d x = showParen (d > 10) $
        showString "Address " .
        shows (formatAddress x)

-- | Try to parse a string containing one valid address.
--
-- An address string is in the format @method:key1=val1,key2=val2@. There
-- are some limitations on the characters allowed within methods and
-- parameters; see the D-Bus specification for full details.
parseAddress :: String -> Maybe Address
parseAddress = maybeParseString $ do
    addr <- parsecAddress
    eof
    return addr

-- | Try to parse a string containing one or more valid addresses.
--
-- Addresses are separated by semicolons. See 'parseAddress' for the format
-- of addresses.
parseAddresses :: String -> Maybe [Address]
parseAddresses = maybeParseString $ do
    addrs <- sepEndBy parsecAddress (char ';')
    eof
    return addrs

parsecAddress :: Parser Address
parsecAddress = p where
    p = do
        method <- many (noneOf ":;")
        _ <- char ':'
        params <- sepEndBy param (char ',')
        return (Address method (Data.Map.fromList params))

    param = do
        key <- many1 (noneOf "=;,")
        _ <- char '='
        value <- many1 valueChar
        return (key, value)

    valueChar = encoded <|> unencoded
    encoded = do
        _ <- char '%'
        hex <- count 2 hexDigit
        return (chr (hexToInt hex))
    unencoded = oneOf optionallyEncoded

-- | Returns the address in the environment variable
-- @DBUS_SYSTEM_BUS_ADDRESS@, or
-- @unix:path=\/var\/run\/dbus\/system_bus_socket@ if @DBUS_SYSTEM_BUS_ADDRESS@
-- is not set.
--
-- Returns 'Nothing' if @DBUS_SYSTEM_BUS_ADDRESS@ contains an invalid address.
getSystemAddress :: IO (Maybe Address)
getSystemAddress = do
    let system = "unix:path=/var/run/dbus/system_bus_socket"
    env <- lookupEnv "DBUS_SYSTEM_BUS_ADDRESS"
    return (parseAddress (fromMaybe system env))

-- | Returns the first address in the environment variable
-- @DBUS_SESSION_BUS_ADDRESS@, which must be set.
--
-- Returns 'Nothing' if @DBUS_SYSTEM_BUS_ADDRESS@ contains an invalid address
-- or @DBUS_SESSION_BUS_ADDRESS@ is unset @XDG_RUNTIME_DIR@ doesn't have @/bus@.
getSessionAddress :: IO (Maybe Address)
getSessionAddress = lookupEnv "DBUS_SESSION_BUS_ADDRESS" >>= \case
    Just addrs -> pure (parseAddresses addrs >>= listToMaybe)
    Nothing -> (>>= parseFallback) <$> lookupEnv "XDG_RUNTIME_DIR"
  where
    parseFallback dir = parseAddress ("unix:path=" ++ dir ++ "/bus")

-- | Returns the address in the environment variable
-- @DBUS_STARTER_ADDRESS@, which must be set.
--
-- Returns 'Nothing' if @DBUS_STARTER_ADDRESS@ is unset or contains an
-- invalid address.
getStarterAddress :: IO (Maybe Address)
getStarterAddress = do
    env <- lookupEnv "DBUS_STARTER_ADDRESS"
    return (env >>= parseAddress)

hexToInt :: String -> Int
hexToInt = foldl ((+) . (16 *)) 0 . map digitToInt

maybeParseString :: Parser a -> String -> Maybe a
maybeParseString p str = case runParser p () "" str of
    Left _ -> Nothing
    Right a -> Just a
