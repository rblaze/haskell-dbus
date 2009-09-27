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

import DBus.Bus
import DBus.Bus.Address
import DBus.Bus.Connection
import DBus.Message
import DBus.Types

import Control.Monad
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Data.Int
import Data.Word
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
	AddressOption addr -> do
		addr' <- case parseAddresses addr of
			Just (x:_) -> return x
			_          -> error $ "Invalid address: " ++ show addr
		c <- connect . findTransport $ addr'
		name <- register c
		return (c, name)

addMatchMsg :: String -> MethodCall
addMatchMsg match = MethodCall
	(fromJust . mkObjectPath $ "/org/freedesktop/DBus")
	(fromJust . mkMemberName $ "AddMatch")
	(mkInterfaceName "org.freedesktop.DBus")
	(mkBusName "org.freedesktop.DBus")
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

onMessage :: Either String ReceivedMessage -> IO ()
onMessage (Left err) = do
	hPutStrLn stderr err
	exitFailure

onMessage (Right msg) = putStrLn (formatMessage msg ++ "\n")

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
	
	forever (recv bus >>= onMessage)

-- Message formatting is verbose and mostly uninteresting, except as an
-- excersise in string manipulation.

formatMessage :: ReceivedMessage -> String

-- Method call
formatMessage (ReceivedMethodCall serial sender msg) = concat
	[ "method call"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . methodCallDestination $ msg
	, " serial="
	, show serial
	, " path="
	, strObjectPath . methodCallPath $ msg
	, "; interface="
	, fromMaybe "(null)" . fmap strInterfaceName . methodCallInterface $ msg
	, "; member="
	, strMemberName . methodCallMember $ msg
	, formatBody msg
	]

-- Method return
formatMessage (ReceivedMethodReturn _ sender msg) = concat
	[ "method return"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . methodReturnDestination $ msg
	, " reply_serial="
	, show . methodReturnSerial $ msg
	, formatBody msg
	]

-- Error
formatMessage (ReceivedError _ sender msg) = concat
	[ "error"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " -> dest="
	, fromMaybe "(null)" . fmap strBusName . errorDestination $ msg
	, " error_name="
	, strErrorName . errorName $ msg
	, " reply_serial="
	, show . errorSerial $ msg
	, formatBody msg
	]

-- Signal
formatMessage (ReceivedSignal serial sender msg) = concat
	[ "signal"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " serial="
	, show serial
	, " path="
	, strObjectPath . signalPath $ msg
	, "; interface="
	, strInterfaceName . signalInterface $ msg
	, "; member="
	, strMemberName . signalMember $ msg
	, formatBody msg
	]

formatMessage (ReceivedUnknown serial sender) = concat
	[ "unknown"
	, " sender="
	, fromMaybe "(null)" . fmap strBusName $ sender
	, " serial="
	, show serial
	]

formatBody :: Message a => a -> String
formatBody msg = formatted where
	tree = Children $ map formatVariant body
	body = messageBody msg
	formatted = intercalate "\n" ([] : collapseTree 1 tree)

-- A string tree allows easy indentation of nested structures
data StringTree = Line String | MultiLine [StringTree] | Children [StringTree]
	deriving (Show)

collapseTree :: Int -> StringTree -> [String]
collapseTree d (Line x)       = [concat (replicate d "   ") ++ x]
collapseTree d (MultiLine xs) = concatMap (collapseTree d) xs
collapseTree d (Children xs)  = concatMap (collapseTree (d + 1)) xs

-- Formatting for various kinds of variants, keyed to their signature type.
formatVariant :: Variant -> StringTree
formatVariant v = formatVariant' type' v where
	[type'] = signatureTypes . variantSignature $ v

formatVariant' :: Type -> Variant -> StringTree

formatVariant' BooleanT x = Line $ "boolean " ++ strX where
	x' = fromJust . fromVariant $ x :: Bool
	strX = if x' then "true" else "false"

formatVariant' ByteT x = Line $ "byte " ++ show x' where
	x' = fromJust . fromVariant $ x :: Word8

formatVariant' Int16T x = Line $ "int16 " ++ show x' where
	x' = fromJust . fromVariant $ x :: Int16

formatVariant' Int32T x = Line $ "int32 " ++ show x' where
	x' = fromJust . fromVariant $ x :: Int32

formatVariant' Int64T x = Line $ "int64 " ++ show x' where
	x' = fromJust . fromVariant $ x :: Int64

formatVariant' UInt16T x = Line $ "uint16 " ++ show x' where
	x' = fromJust . fromVariant $ x :: Word16

formatVariant' UInt32T x = Line $ "uint32 " ++ show x' where
	x' = fromJust . fromVariant $ x :: Word32

formatVariant' UInt64T x = Line $ "uint64 " ++ show x' where
	x' = fromJust . fromVariant $ x :: Word64

formatVariant' DoubleT x = Line $ "double " ++ show x' where
	x' = fromJust . fromVariant $ x :: Double

formatVariant' StringT x = Line $ "string " ++ show x' where
	x' = fromJust . fromVariant $ x :: String

formatVariant' ObjectPathT x = Line $ "object path " ++ show x' where
	x' = strObjectPath . fromJust . fromVariant $ x

formatVariant' SignatureT x = Line $ "signature " ++ show x' where
	x' = strSignature . fromJust . fromVariant $ x

formatVariant' (ArrayT _) x = MultiLine lines' where
	items = arrayItems . fromJust . fromVariant $ x
	lines' =
		[ Line "array ["
		, Children . map formatVariant $ items
		, Line "]"
		]

formatVariant' (DictT _ _) x = MultiLine lines' where
	items = dictionaryItems . fromJust . fromVariant $ x
	lines' = [ Line "dictionary {"
		, Children . map formatItem $ items
		, Line "}"
		]
	formatItem (k, v) = MultiLine $ [Line $ k' ++ " -> " ++ vHead] ++ vTail where
		Line k' = formatVariant (atomToVariant k)
		v' = collapseTree 0 (formatVariant v)
		vHead = head v'
		vTail = map Line $ tail v'

formatVariant' (StructT _) x = MultiLine lines' where
	Structure items = fromJust . fromVariant $ x
	lines' =
		[ Line "struct ("
		, Children . map formatVariant $ items
		, Line ")"
		]

formatVariant' VariantT x = formatVariant . fromJust . fromVariant $ x
