{-
  Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE OverloadedStrings #-}

import DBus.Address
import DBus.Bus
import DBus.Connection
import DBus.Constants
import DBus.Message
import DBus.Types

import Control.Monad
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Int
import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System
import System.IO
import System.Console.GetOpt

data Bus = Session | System
	deriving (Show)

data Option = BusOption Bus | AddressOption String
	deriving (Show)

optionInfo :: [OptDescr Option]
optionInfo = [
	  Option [] ["session"] (NoArg (BusOption Session))
	         "Monitor the session message bus. (default)"
	, Option [] ["system"] (NoArg (BusOption System))
	         "Monitor the system message bus."
	, Option [] ["address"] (ReqArg AddressOption "ADDRESS")
	         "Connect to a particular bus address."
	]

usage :: String -> String
usage name = "Usage: " ++ name ++ " [OPTION...]"

findBus :: [Option] -> IO (Connection, BusName)
findBus []    = getSessionBus
findBus (o:_) = case o of
	BusOption Session -> getSessionBus
	BusOption System  -> getSystemBus
	AddressOption addr -> case mkAddresses (TL.pack addr) of
			Just [x] -> getBus x
			Just  x  -> getFirstBus x
			_        -> error $ "Invalid address: " ++ show addr

addMatchMsg :: String -> MethodCall
addMatchMsg match = MethodCall
	dbusPath
	(mkMemberName' "AddMatch")
	(Just dbusInterface)
	(Just dbusName)
	Set.empty
	[toVariant match]

addMatch :: Connection -> String -> IO ()
addMatch c s = send c return (addMatchMsg s) >> return ()

defaultFilters :: [String]
defaultFilters =
	[ "type='signal'"
	, "type='method_call'"
	, "type='method_return'"
	, "type='error'"
	]

onMessage :: ReceivedMessage -> IO ()
onMessage msg = putStrLn $ (TL.unpack $ formatMessage msg) ++ "\n"

main :: IO ()
main = do
	args <- getArgs
	let (options, userFilters, errors) = getOpt Permute optionInfo args
	unless (null errors) $ do
		name <- getProgName
		hPutStrLn stderr $ concat errors
		hPutStrLn stderr $ usageInfo (usage name) optionInfo
		exitFailure
	
	(bus, _) <- findBus options
	
	let filters = if null userFilters
		then defaultFilters
		else userFilters
	
	mapM_ (addMatch bus) filters
	
	forever (receive bus >>= onMessage)

-- Message formatting is verbose and mostly uninteresting, except as an
-- excersise in string manipulation.

formatMessage :: ReceivedMessage -> Text

-- Method call
formatMessage (ReceivedMethodCall serial sender msg) = TL.concat
	[ "method call"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . methodCallDestination $ msg
	, " serial="
	, TL.pack . show $ serial
	, " path="
	, strObjectPath . methodCallPath $ msg
	, "; interface="
	, fromMaybe "(null)" . fmap strInterfaceName . methodCallInterface $ msg
	, "; member="
	, strMemberName . methodCallMember $ msg
	, formatBody msg
	]

-- Method return
formatMessage (ReceivedMethodReturn _ sender msg) = TL.concat
	[ "method return"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . methodReturnDestination $ msg
	, " reply_serial="
	, TL.pack . show . methodReturnSerial $ msg
	, formatBody msg
	]

-- Error
formatMessage (ReceivedError _ sender msg) = TL.concat
	[ "error"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . errorDestination $ msg
	, " error_name="
	, strErrorName . errorName $ msg
	, " reply_serial="
	, TL.pack . show . errorSerial $ msg
	, formatBody msg
	]

-- Signal
formatMessage (ReceivedSignal serial sender msg) = TL.concat
	[ "signal"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . signalDestination $ msg
	, " serial="
	, TL.pack . show $ serial
	, " path="
	, strObjectPath . signalPath $ msg
	, "; interface="
	, strInterfaceName . signalInterface $ msg
	, "; member="
	, strMemberName . signalMember $ msg
	, formatBody msg
	]

formatMessage (ReceivedUnknown serial sender _) = TL.concat
	[ "unknown"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " serial="
	, TL.pack .  show $ serial
	]

formatBody :: Message a => a -> Text
formatBody msg = formatted where
	tree = Children $ map formatVariant body
	body = messageBody msg
	formatted = TL.intercalate "\n" (TL.empty : collapseTree 1 tree)

-- A string tree allows easy indentation of nested structures
data StringTree = Line Text | MultiLine [StringTree] | Children [StringTree]
	deriving (Show)

collapseTree :: Int64 -> StringTree -> [Text]
collapseTree d (Line x)       = [TL.append (TL.replicate d "   ") x]
collapseTree d (MultiLine xs) = concatMap (collapseTree d) xs
collapseTree d (Children xs)  = concatMap (collapseTree (d + 1)) xs

-- Formatting for various kinds of variants, keyed to their signature type.
formatVariant :: Variant -> StringTree
formatVariant v = formatVariant' (variantType v) v where

showT :: Show a => a -> Text
showT = TL.pack . show

formatVariant' :: Type -> Variant -> StringTree

formatVariant' DBusBoolean x = Line $ TL.append "boolean " strX where
	x' = fromJust . fromVariant $ x :: Bool
	strX = if x' then "true" else "false"

formatVariant' DBusByte x = Line $ TL.append "byte " $ showT x' where
	x' = fromJust . fromVariant $ x :: Word8

formatVariant' DBusInt16 x = Line $ TL.append "int16 " $ showT x' where
	x' = fromJust . fromVariant $ x :: Int16

formatVariant' DBusInt32 x = Line $ TL.append "int32 " $ showT x' where
	x' = fromJust . fromVariant $ x :: Int32

formatVariant' DBusInt64 x = Line $ TL.append "int64 " $ showT x' where
	x' = fromJust . fromVariant $ x :: Int64

formatVariant' DBusWord16 x = Line $ TL.append "uint16 " $ showT x' where
	x' = fromJust . fromVariant $ x :: Word16

formatVariant' DBusWord32 x = Line $ TL.append "uint32 " $ showT x' where
	x' = fromJust . fromVariant $ x :: Word32

formatVariant' DBusWord64 x = Line $ TL.append "uint64 " $ showT x' where
	x' = fromJust . fromVariant $ x :: Word64

formatVariant' DBusDouble x = Line $ TL.append "double " $ showT x' where
	x' = fromJust . fromVariant $ x :: Double

formatVariant' DBusString x = Line $ TL.append "string " $ showT x' where
	x' = fromJust . fromVariant $ x :: String

formatVariant' DBusObjectPath x = Line $ TL.append "object path " $ showT x' where
	x' = strObjectPath . fromJust . fromVariant $ x

formatVariant' DBusSignature x = Line $ TL.append "signature " $ showT x' where
	x' = strSignature . fromJust . fromVariant $ x

formatVariant' (DBusArray _) x = MultiLine lines' where
	items = arrayItems . fromJust . fromVariant $ x
	lines' =
		[ Line "array ["
		, Children . map formatVariant $ items
		, Line "]"
		]

formatVariant' (DBusDictionary _ _) x = MultiLine lines' where
	items = dictionaryItems . fromJust . fromVariant $ x
	lines' = [ Line "dictionary {"
		, Children . map formatItem $ items
		, Line "}"
		]
	formatItem (k, v) = MultiLine $ firstLine : vTail where
		Line k' = formatVariant k
		v' = collapseTree 0 (formatVariant v)
		vHead = head v'
		vTail = map Line $ tail v'
		firstLine = Line $ TL.concat [k', " -> ", vHead]

formatVariant' (DBusStructure _) x = MultiLine lines' where
	Structure items = fromJust . fromVariant $ x
	lines' =
		[ Line "struct ("
		, Children . map formatVariant $ items
		, Line ")"
		]

formatVariant' DBusVariant x = formatVariant . fromJust . fromVariant $ x
