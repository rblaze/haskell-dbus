{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2009-2011 John Millikin <jmillikin@gmail.com>
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

module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Data.Maybe
import           Data.Int
import           Data.Word
import           Data.Text (Text)
import qualified Data.Text
import           System
import           System.IO
import           System.Console.GetOpt

import           DBus.Address
import           DBus.Client (connect, setMessageProcessor)
import           DBus.Client.Simple
import           DBus.Message

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

findClient :: [Option] -> IO Client
findClient []    = connectSession
findClient (o:_) = case o of
	BusOption Session -> connectSession
	BusOption System  -> connectSystem
	AddressOption addr -> case address (Data.Text.pack addr) of
			Just addr' -> connect addr'
			_ -> error ("Invalid address: " ++ show addr)

addMatch :: Proxy -> Text -> IO ()
addMatch bus match = do
	_ <- call bus "org.freedesktop.DBus" "AddMatch" [toVariant match]
	return ()

defaultFilters :: [Text]
defaultFilters =
	[ "type='signal'"
	, "type='method_call'"
	, "type='method_return'"
	, "type='error'"
	]

main :: IO ()
main = do
	args <- getArgs
	let (options, userFilters, errors) = getOpt Permute optionInfo args
	unless (null errors) $ do
		name <- getProgName
		hPutStrLn stderr (concat errors)
		hPutStrLn stderr (usageInfo (usage name) optionInfo)
		exitFailure
	
	client <- findClient options
	setMessageProcessor client (\msg -> do
		putStrLn (Data.Text.unpack (formatMessage msg) ++ "\n")
		return False)
	
	let filters = if null userFilters
		then defaultFilters
		else map Data.Text.pack userFilters
	
	bus <- proxy client "org.freedesktop.DBus" "/org/freedesktop/DBus"
	mapM_ (addMatch bus) filters
	
	-- wait forever
	forever (threadDelay 50000)

-- Message formatting is verbose and mostly uninteresting, except as an
-- excersise in string manipulation.

showT :: Show a => a -> Text
showT = Data.Text.pack . show

formatMessage :: ReceivedMessage -> Text

-- Method call
formatMessage (ReceivedMethodCall serial sender msg) = Data.Text.concat
	[ "method call"
	, " sender="
	, fromMaybe "(null)" (fmap busNameText sender)
	, " -> dest="
	, fromMaybe "(null)" (fmap busNameText (methodCallDestination msg))
	, " serial="
	, showT (serialValue serial)
	, " path="
	, objectPathText (methodCallPath msg)
	, "; interface="
	, fromMaybe "(null)" (fmap interfaceNameText (methodCallInterface msg))
	, "; member="
	, memberNameText (methodCallMember msg)
	, formatBody msg
	]

-- Method return
formatMessage (ReceivedMethodReturn _ sender msg) = Data.Text.concat
	[ "method return"
	, " sender="
	, fromMaybe "(null)" (fmap busNameText sender)
	, " -> dest="
	, fromMaybe "(null)" (fmap busNameText (methodReturnDestination msg))
	, " reply_serial="
	, showT (serialValue (methodReturnSerial msg))
	, formatBody msg
	]

-- Error
formatMessage (ReceivedError _ sender msg) = Data.Text.concat
	[ "error"
	, " sender="
	, fromMaybe "(null)" (fmap busNameText sender)
	, " -> dest="
	, fromMaybe "(null)" (fmap busNameText (errorDestination msg))
	, " error_name="
	, errorNameText (DBus.Message.errorName msg)
	, " reply_serial="
	, showT (serialValue (errorSerial msg))
	, formatBody msg
	]

-- Signal
formatMessage (ReceivedSignal serial sender msg) = Data.Text.concat
	[ "signal"
	, " sender="
	, fromMaybe "(null)" (fmap busNameText sender)
	, " -> dest="
	, fromMaybe "(null)" (fmap busNameText (signalDestination msg))
	, " serial="
	, showT (serialValue serial)
	, " path="
	, objectPathText (signalPath msg)
	, "; interface="
	, interfaceNameText (signalInterface msg)
	, "; member="
	, memberNameText (signalMember msg)
	, formatBody msg
	]

formatMessage (ReceivedUnknown serial sender _) = Data.Text.concat
	[ "unknown"
	, " sender="
	, fromMaybe "(null)" (fmap busNameText sender)
	, " serial="
	, showT (serialValue serial)
	]

formatBody :: Message a => a -> Text
formatBody msg = formatted where
	tree = Children (map formatVariant body)
	body = messageBody msg
	formatted = Data.Text.intercalate "\n" ("" : collapseTree 1 tree)

-- A string tree allows easy indentation of nested structures
data StringTree = Line Text | MultiLine [StringTree] | Children [StringTree]
	deriving (Show)

collapseTree :: Int -> StringTree -> [Text]
collapseTree d (Line x)       = [Data.Text.append (Data.Text.replicate d "   ") x]
collapseTree d (MultiLine xs) = concatMap (collapseTree d) xs
collapseTree d (Children xs)  = concatMap (collapseTree (d + 1)) xs

-- Formatting for various kinds of variants, keyed to their signature type.
formatVariant :: Variant -> StringTree
formatVariant x = case variantType x of
	
	TypeBoolean -> Line $ let
		Just x' = fromVariant x
		strX = if x' then "true" else "false"
		in Data.Text.append "boolean " strX
	
	TypeWord8 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Word8)
		in Data.Text.append "byte " strX
	
	TypeWord16 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Word16)
		in Data.Text.append "uint16 " strX
	
	TypeWord32 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Word32)
		in Data.Text.append "uint32 " strX
	
	TypeWord64 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Word64)
		in Data.Text.append "uint64 " strX
	
	TypeInt16 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Int16)
		in Data.Text.append "int16 " strX
	
	TypeInt32 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Int32)
		in Data.Text.append "int32 " strX
	
	TypeInt64 -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Int64)
		in Data.Text.append "int64 " strX
	
	TypeDouble -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Double)
		in Data.Text.append "double " strX
	
	TypeString -> Line $ let
		Just x' = fromVariant x
		strX = showT (x' :: Text)
		in Data.Text.append "string " strX
	
	TypeObjectPath -> Line $ let
		Just x' = fromVariant x
		strX = showT (objectPathText x')
		in Data.Text.append "object path " strX
	
	TypeSignature -> Line $ let
		Just x' = fromVariant x
		strX = showT (signatureText x')
		in Data.Text.append "signature " strX
	
	TypeArray _ -> MultiLine $ let
		Just x' = fromVariant x
		items = arrayItems x'
		lines' = [ Line "array ["
		         , Children (map formatVariant items)
		         , Line "]"
		         ]
		in lines'
	
	TypeDictionary _ _ -> MultiLine $ let
		Just x' = fromVariant x
		items = dictionaryItems x'
		lines' = [ Line "dictionary {"
		         , Children (map formatItem items)
		         , Line "}"
		         ]
		formatItem (k, v) = MultiLine (firstLine : vTail) where
			Line k' = formatVariant k
			v' = collapseTree 0 (formatVariant v)
			vHead = head v'
			vTail = map Line (tail v')
			firstLine = Line (Data.Text.concat [k', " -> ", vHead])
		in lines'
	
	TypeStructure _ -> MultiLine $ let
		Just x' = fromVariant x
		items = structureItems x'
		lines' = [ Line "struct ("
		         , Children (map formatVariant items)
		         , Line ")"
		         ]
		in lines'
	
	TypeVariant -> let
		Just x' = fromVariant x
		in formatVariant x'
