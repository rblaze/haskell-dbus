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

module DBus.Wire
	( Endianness(..)
	, MarshalError
	, marshalErrorMessage
	
	, UnmarshalError
	, unmarshalErrorMessage
	
	, marshalMessage
	, unmarshalMessage
	, unmarshalMessageM
	) where

import           Control.Monad (liftM, when, unless)
import qualified Data.ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8
import           Data.Int (Int16, Int32, Int64)
import qualified Data.Map
import           Data.Map (Map)
import           Data.Maybe (fromJust, listToMaybe, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text.Encoding
import qualified Data.Vector
import           Data.Vector (Vector)
import           Data.Word (Word8, Word16, Word32, Word64)

import qualified Data.Serialize.Builder as Builder
import qualified Data.Serialize.Get as Get
import           Data.Serialize.IEEE754 (getFloat64be, getFloat64le, putFloat64be, putFloat64le)
import           Data.Serialize.Put (runPut)

import           DBus.Message
import           DBus.Types

data Endianness = LittleEndian | BigEndian
	deriving (Show, Eq)

encodeEndianness :: Endianness -> Word8
encodeEndianness LittleEndian = 0x6C
encodeEndianness BigEndian    = 0x42

decodeEndianness :: Word8 -> Maybe Endianness
decodeEndianness 0x6C = Just LittleEndian
decodeEndianness 0x42 = Just BigEndian
decodeEndianness _    = Nothing

alignment :: Type -> Word8
alignment TypeBoolean = 4
alignment TypeWord8 = 1
alignment TypeWord16 = 2
alignment TypeWord32 = 4
alignment TypeWord64 = 8
alignment TypeInt16 = 2
alignment TypeInt32 = 4
alignment TypeInt64 = 8
alignment TypeDouble = 8
alignment TypeString = 4
alignment TypeObjectPath = 4
alignment TypeSignature = 1
alignment (TypeArray _) = 4
alignment (TypeDictionary _ _) = 4
alignment (TypeStructure _) = 8
alignment TypeVariant = 1

{-# INLINE padding #-}
padding :: Word64 -> Word8 -> Word64
padding current count = required where
	count' = fromIntegral count
	missing = mod current count'
	required = if missing > 0
		then count' - missing
		else 0

data WireR s a
	= WireRL String
	| WireRR a !s

newtype Wire s a = Wire
	{ unWire :: Endianness -> s -> WireR s a
	}

instance Monad (Wire s) where
	{-# INLINE return #-}
	return a = Wire (\_ s -> WireRR a s)
	
	{-# INLINE (>>=) #-}
	m >>= k = Wire $ \e s -> case unWire m e s of
		WireRL err -> WireRL err
		WireRR a s' -> unWire (k a) e s'
	
	{-# INLINE (>>) #-}
	m >> k = Wire $ \e s -> case unWire m e s of
		WireRL err -> WireRL err
		WireRR _ s' -> unWire k e s'

throwError :: String -> Wire s a
throwError err = Wire (\_ _ -> WireRL err)

{-# INLINE getState #-}
getState :: Wire s s
getState = Wire (\_ s -> WireRR s s)

{-# INLINE putState #-}
putState :: s -> Wire s ()
putState s = Wire (\_ _ -> WireRR () s)

{-# INLINE chooseEndian #-}
chooseEndian :: a -> a -> Wire s a
chooseEndian big little = Wire (\e s -> case e of
	BigEndian -> WireRR big s
	LittleEndian -> WireRR little s)

type Marshal = Wire MarshalState

newtype MarshalError = MarshalError String
	deriving (Show, Eq)

marshalErrorMessage :: MarshalError -> String
marshalErrorMessage (MarshalError s) = s

data MarshalState = MarshalState
	!Builder.Builder
	{-# UNPACK #-} !Word64

marshal :: Value -> Marshal ()
marshal (ValueAtom x) = marshalAtom x
marshal (ValueBytes xs) = marshalStrictBytes xs
marshal (ValueVector t xs) = marshalVector t xs
marshal (ValueMap kt vt xs) = marshalMap kt vt xs
marshal (ValueStructure xs) = marshalStructure xs
marshal (ValueVariant x) = marshalVariant x

marshalAtom :: Atom -> Marshal ()
marshalAtom (AtomWord8 x) = marshalWord8 x
marshalAtom (AtomWord16 x) = marshalWord16 x
marshalAtom (AtomWord32 x) = marshalWord32 x
marshalAtom (AtomWord64 x) = marshalWord64 x
marshalAtom (AtomInt16 x) = marshalInt16 x
marshalAtom (AtomInt32 x) = marshalInt32 x
marshalAtom (AtomInt64 x) = marshalInt64 x
marshalAtom (AtomDouble x) = marshalDouble x
marshalAtom (AtomBool x) = marshalBool x
marshalAtom (AtomText x) = marshalText x
marshalAtom (AtomObjectPath x) = marshalObjectPath x
marshalAtom (AtomSignature x) = marshalSignature x

appendB :: Word64 -> Builder.Builder -> Marshal ()
appendB size bytes = Wire (\_ (MarshalState builder count) -> let
	builder' = Builder.append builder bytes
	count' = count + size
	in WireRR () (MarshalState builder' count'))

appendS :: ByteString -> Marshal ()
appendS bytes = appendB
	(fromIntegral (Data.ByteString.length bytes))
	(Builder.fromByteString bytes)

pad :: Word8 -> Marshal ()
pad count = do
	(MarshalState _ existing) <- getState
	let padding' = fromIntegral (padding existing count)
	appendS (Data.ByteString.replicate padding' 0)

marshalBuilder :: Word8
               -> (a -> Builder.Builder)
               -> (a -> Builder.Builder)
               -> a -> Marshal ()
marshalBuilder size be le x = do
	builder <- chooseEndian (be x) (le x)
	pad size
	appendB (fromIntegral size) builder

type Unmarshal = Wire UnmarshalState

newtype UnmarshalError = UnmarshalError String
	deriving (Show, Eq)

unmarshalErrorMessage :: UnmarshalError -> String
unmarshalErrorMessage (UnmarshalError s) = s

data UnmarshalState = UnmarshalState
	{-# UNPACK #-} !ByteString
	{-# UNPACK #-} !Word64

unmarshal :: Type -> Unmarshal Value
unmarshal TypeWord8 = liftM toValue unmarshalWord8
unmarshal TypeWord16 = liftM toValue unmarshalWord16
unmarshal TypeWord32 = liftM toValue unmarshalWord32
unmarshal TypeWord64 = liftM toValue unmarshalWord64
unmarshal TypeInt16 = liftM toValue unmarshalInt16
unmarshal TypeInt32 = liftM toValue unmarshalInt32
unmarshal TypeInt64 = liftM toValue unmarshalInt64
unmarshal TypeDouble = liftM toValue unmarshalDouble
unmarshal TypeBoolean = liftM toValue unmarshalBool
unmarshal TypeString = liftM toValue unmarshalText
unmarshal TypeObjectPath = liftM toValue unmarshalObjectPath
unmarshal TypeSignature = liftM toValue unmarshalSignature
unmarshal (TypeArray TypeWord8) = liftM toValue unmarshalByteArray
unmarshal (TypeArray t) = liftM (ValueVector t) (unmarshalArray t)
unmarshal (TypeDictionary kt vt) = unmarshalDictionary kt vt
unmarshal (TypeStructure ts) = unmarshalStructure ts
unmarshal TypeVariant = unmarshalVariant

{-# INLINE consume #-}
consume :: Word64 -> Unmarshal ByteString
consume count = do
	(UnmarshalState bytes offset) <- getState
	let count' = fromIntegral count
	let (x, bytes') = Data.ByteString.splitAt count' bytes
	let lenConsumed = Data.ByteString.length x
	if lenConsumed == count'
		then do
			putState (UnmarshalState bytes' (offset + count))
			return x
		else throwError ("Unexpected EOF at offset " ++ show (offset + fromIntegral lenConsumed))

skipPadding :: Word8 -> Unmarshal ()
skipPadding count = do
	(UnmarshalState _ offset) <- getState
	bytes <- consume (padding offset count)
	unless (Data.ByteString.all (== 0) bytes)
		(throwError ("Value padding " ++ show bytes ++ " contains invalid bytes."))

skipTerminator :: Unmarshal ()
skipTerminator = do
	byte <- unmarshalWord8
	when (byte /= 0) (throwError "Textual value is not NUL-terminated.")

fromMaybeU :: Show a => String -> (a -> Maybe b) -> a -> Unmarshal b
fromMaybeU label f x = case f x of
	Just x' -> return x'
	Nothing -> throwError ("Invalid " ++ label ++ ": " ++ show x)

unmarshalGet :: Word8 -> Get.Get a -> Get.Get a -> Unmarshal a
unmarshalGet count be le = do
	skipPadding count
	bytes <- consume (fromIntegral count)
	get <- chooseEndian be le
	let Right ret = Get.runGet get bytes
	return ret

marshalWord8 :: Word8 -> Marshal ()
marshalWord8 x = appendB 1 (Builder.singleton x)

unmarshalWord8 :: Unmarshal Word8
unmarshalWord8 = liftM Data.ByteString.head (consume 1)

marshalWord16 :: Word16 -> Marshal ()
marshalWord16 = marshalBuilder 2
	Builder.putWord16be
	Builder.putWord16le

marshalWord32 :: Word32 -> Marshal ()
marshalWord32 = marshalBuilder 4
	Builder.putWord32be
	Builder.putWord32le

marshalWord64 :: Word64 -> Marshal ()
marshalWord64 = marshalBuilder 8
	Builder.putWord64be
	Builder.putWord64le

marshalInt16 :: Int16 -> Marshal ()
marshalInt16 = marshalWord16 . fromIntegral

marshalInt32 :: Int32 -> Marshal ()
marshalInt32 = marshalWord32 . fromIntegral

marshalInt64 :: Int64 -> Marshal ()
marshalInt64 = marshalWord64 . fromIntegral

unmarshalWord16 :: Unmarshal Word16
unmarshalWord16 = unmarshalGet 2
	Get.getWord16be
	Get.getWord16le

unmarshalWord32 :: Unmarshal Word32
unmarshalWord32 = unmarshalGet 4
	Get.getWord32be
	Get.getWord32le

unmarshalWord64 :: Unmarshal Word64
unmarshalWord64 = unmarshalGet 8
	Get.getWord64be
	Get.getWord64le

unmarshalInt16 :: Unmarshal Int16
unmarshalInt16 = liftM fromIntegral unmarshalWord16

unmarshalInt32 :: Unmarshal Int32
unmarshalInt32 = liftM fromIntegral unmarshalWord32

unmarshalInt64 :: Unmarshal Int64
unmarshalInt64 = liftM fromIntegral unmarshalWord64

marshalDouble :: Double -> Marshal ()
marshalDouble x = do
	put <- chooseEndian putFloat64be putFloat64le
	pad 8
	appendS (runPut (put x))

unmarshalDouble :: Unmarshal Double
unmarshalDouble = unmarshalGet 8
	getFloat64be
	getFloat64le

marshalBool :: Bool -> Marshal ()
marshalBool False = marshalWord32 0
marshalBool True  = marshalWord32 1

unmarshalBool :: Unmarshal Bool
unmarshalBool = do
	word <- unmarshalWord32
	case word of
		0 -> return False
		1 -> return True
		_ -> throwError ("Invalid boolean: " ++ show word)

marshalText :: Text -> Marshal ()
marshalText text = do
	let bytes = Data.Text.Encoding.encodeUtf8 text
	when (Data.ByteString.any (== 0) bytes)
		(throwError ("String " ++ show text ++ " contained forbidden character: '\\x00'"))
	marshalWord32 (fromIntegral (Data.ByteString.length bytes))
	appendS bytes
	marshalWord8 0

unmarshalText :: Unmarshal Text
unmarshalText = do
	byteCount <- unmarshalWord32
	bytes <- consume (fromIntegral byteCount)
	skipTerminator
	fromMaybeU "text" maybeDecodeUtf8 bytes

maybeDecodeUtf8 :: ByteString -> Maybe Text
maybeDecodeUtf8 bs = case Data.Text.Encoding.decodeUtf8' bs of
	Right text -> Just text
	_ -> Nothing

marshalObjectPath :: ObjectPath -> Marshal ()
marshalObjectPath p = do
	let bytes = Data.ByteString.Char8.pack (formatObjectPath p)
	marshalWord32 (fromIntegral (Data.ByteString.length bytes))
	appendS bytes
	marshalWord8 0

unmarshalObjectPath :: Unmarshal ObjectPath
unmarshalObjectPath = do
	byteCount <- unmarshalWord32
	bytes <- consume (fromIntegral byteCount)
	skipTerminator
	fromMaybeU "object path" parseObjectPath (Data.ByteString.Char8.unpack bytes)

signatureBytes :: Signature -> ByteString
signatureBytes (Signature ts) = Data.ByteString.Char8.pack (concatMap typeCode ts)

marshalSignature :: Signature -> Marshal ()
marshalSignature x = do
	let bytes = signatureBytes x
	marshalWord8 (fromIntegral (Data.ByteString.length bytes))
	appendS bytes
	marshalWord8 0

unmarshalSignature :: Unmarshal Signature
unmarshalSignature = do
	byteCount <- unmarshalWord8
	bytes <- consume (fromIntegral byteCount)
	skipTerminator
	fromMaybeU "signature" parseSignatureBytes bytes

arrayMaximumLength :: Int
arrayMaximumLength = 67108864

marshalVector :: Type -> Vector Value -> Marshal ()
marshalVector t x = do
	(arrayPadding, arrayBytes) <- getArrayBytes t x
	let arrayLen = Data.ByteString.length arrayBytes
	when (arrayLen > arrayMaximumLength) (throwError ("Marshaled array size (" ++ show arrayLen ++ " bytes) exceeds maximum limit of (" ++ show arrayMaximumLength ++ " bytes)."))
	marshalWord32 (fromIntegral arrayLen)
	appendS (Data.ByteString.replicate arrayPadding 0)
	appendS arrayBytes

marshalStrictBytes :: ByteString -> Marshal ()
marshalStrictBytes bytes = do
	let arrayLen = Data.ByteString.length bytes
	when (fromIntegral arrayLen > arrayMaximumLength) (throwError ("Marshaled array size (" ++ show arrayLen ++ " bytes) exceeds maximum limit of (" ++ show arrayMaximumLength ++ " bytes)."))
	marshalWord32 (fromIntegral arrayLen)
	appendS bytes

getArrayBytes :: Type -> Vector Value -> Marshal (Int, ByteString)
getArrayBytes itemType vs = do
	s <- getState
	(MarshalState _ afterLength) <- marshalWord32 0 >> getState
	(MarshalState _ afterPadding) <- pad (alignment itemType) >> getState
	
	putState (MarshalState Builder.empty afterPadding)
	(MarshalState itemBuilder _) <- Data.Vector.mapM_ marshal vs >> getState
	
	let itemBytes = Builder.toByteString itemBuilder
	    paddingSize = fromIntegral (afterPadding - afterLength)
	
	putState s
	return (paddingSize, itemBytes)

unmarshalByteArray :: Unmarshal ByteString
unmarshalByteArray = do
	byteCount <- unmarshalWord32
	consume (fromIntegral byteCount)

unmarshalArray :: Type -> Unmarshal (Vector Value)
unmarshalArray itemType = do
	let getOffset = do
		(UnmarshalState _ o) <- getState
		return o
	byteCount <- unmarshalWord32
	skipPadding (alignment itemType)
	start <- getOffset
	let end = start + fromIntegral byteCount
	vs <- untilM (liftM (>= end) getOffset) (unmarshal itemType)
	end' <- getOffset
	when (end' > end) (throwError ("Array data size exeeds array size of " ++ show end))
	return (Data.Vector.fromList vs)

dictionaryToArray :: Map Atom Value -> Vector Value
dictionaryToArray = Data.Vector.fromList . map step . Data.Map.toList where
	step (k, v) = ValueStructure [ValueAtom k, v]

arrayToDictionary :: Vector Value -> Map Atom Value
arrayToDictionary = Data.Map.fromList . map step . Data.Vector.toList where
	step (ValueStructure [ValueAtom k, v]) = (k, v)
	step _ = error "arrayToDictionary: internal error"

marshalMap :: Type -> Type -> Map Atom Value -> Marshal ()
marshalMap kt vt x = let
	structType = TypeStructure [kt, vt]
	array = dictionaryToArray x
	in marshalVector structType array

unmarshalDictionary :: Type -> Type -> Unmarshal Value
unmarshalDictionary kt vt = do
	let pairType = TypeStructure [kt, vt]
	array <- unmarshalArray pairType
	return (ValueMap kt vt (arrayToDictionary array))

marshalStructure :: [Value] -> Marshal ()
marshalStructure vs = do
	pad 8
	mapM_ marshal vs

unmarshalStructure :: [Type] -> Unmarshal Value
unmarshalStructure ts = do
	skipPadding 8
	liftM ValueStructure (mapM unmarshal ts)

marshalVariant :: Variant -> Marshal ()
marshalVariant var@(Variant val) = do
	sig <- case signature [valueType val] of
		Just x' -> return x'
		Nothing -> throwError ("Signature " ++ show (typeCode (valueType val)) ++ " for variant " ++ show var ++ " is malformed or too large.")
	marshalSignature sig
	marshal val

unmarshalVariant :: Unmarshal Value
unmarshalVariant = do
	let getType sig = case signatureTypes sig of
		[t] -> Just t
		_   -> Nothing
	
	t <- fromMaybeU "variant signature" getType =<< unmarshalSignature
	(toValue . Variant) `liftM` unmarshal t

protocolVersion :: Word8
protocolVersion = 1

messageMaximumLength :: Integer
messageMaximumLength = 134217728

encodeField :: HeaderField -> Value
encodeField (HeaderPath x)        = encodeField' 1 x
encodeField (HeaderInterface x)   = encodeField' 2 x
encodeField (HeaderMember x)      = encodeField' 3 x
encodeField (HeaderErrorName x)   = encodeField' 4 x
encodeField (HeaderReplySerial x) = encodeField' 5 x
encodeField (HeaderDestination x) = encodeField' 6 x
encodeField (HeaderSender x)      = encodeField' 7 x
encodeField (HeaderSignature x)   = encodeField' 8 x

encodeField' :: IsVariant a => Word8 -> a -> Value
encodeField' code x = toValue (code, toVariant x)

decodeField :: (Word8, Variant)
            -> ErrorM UnmarshalError [HeaderField]
decodeField struct = case struct of
	(1, x) -> decodeField' x HeaderPath "path"
	(2, x) -> decodeField' x HeaderInterface "interface"
	(3, x) -> decodeField' x HeaderMember "member"
	(4, x) -> decodeField' x HeaderErrorName "error name"
	(5, x) -> decodeField' x HeaderReplySerial "reply serial"
	(6, x) -> decodeField' x HeaderDestination "destination"
	(7, x) -> decodeField' x HeaderSender "sender"
	(8, x) -> decodeField' x HeaderSignature "signature"
	_      -> return []

decodeField' :: IsVariant a => Variant -> (a -> b) -> String
             -> ErrorM UnmarshalError [b]
decodeField' x f label = case fromVariant x of
	Just x' -> return [f x']
	Nothing -> throwErrorM (UnmarshalError ("Header field " ++ show label ++ " contains invalid value " ++ show x))

marshalMessage :: Message a => Endianness -> Serial -> a
               -> Either MarshalError Data.ByteString.ByteString
marshalMessage e serial msg = runMarshal where
	body = messageBody msg
	marshaler = do
		sig <- checkBodySig body
		empty <- getState
		mapM_ (marshal . (\(Variant x) -> x)) body
		(MarshalState bodyBytesB _) <- getState
		putState empty
		marshal (toValue (encodeEndianness e))
		let bodyBytes = Builder.toByteString bodyBytesB
		marshalHeader msg serial sig (fromIntegral (Data.ByteString.length bodyBytes))
		pad 8
		appendS bodyBytes
		checkMaximumSize
	emptyState = MarshalState Builder.empty 0
	runMarshal = case unWire marshaler e emptyState of
		WireRL err -> Left (MarshalError err)
		WireRR _ (MarshalState builder _) -> Right (Builder.toByteString builder)

checkBodySig :: [Variant] -> Marshal Signature
checkBodySig vs = case signature (map variantType vs) of
	Just x -> return x
	Nothing -> throwError ("Message body " ++ show vs ++ " has too many items")

marshalHeader :: Message a => a -> Serial -> Signature -> Word32
              -> Marshal ()
marshalHeader msg serial bodySig bodyLength = do
	let fields = HeaderSignature bodySig : messageHeaderFields msg
	marshalWord8 (messageTypeCode msg)
	marshalWord8 (messageFlags msg)
	marshalWord8 protocolVersion
	marshalWord32 bodyLength
	marshalWord32 (serialValue serial)
	let fieldType = TypeStructure [TypeWord8, TypeVariant]
	marshalVector fieldType (Data.Vector.fromList (map encodeField fields))

checkMaximumSize :: Marshal ()
checkMaximumSize = do
	(MarshalState _ messageLength) <- getState
	when (toInteger messageLength > messageMaximumLength)
		(throwError ("Marshaled message size (" ++ show messageLength ++ " bytes) exeeds maximum limit of (" ++ show messageMaximumLength ++ " bytes)."))

unmarshalMessageM :: Monad m => (Int -> m ByteString)
                  -> m (Either UnmarshalError ReceivedMessage)
unmarshalMessageM getBytes' = runErrorT $ do
	let getBytes count = do
		bytes <- ErrorT (liftM Right (getBytes' count))
		if Data.ByteString.length bytes < count
			then throwErrorT (UnmarshalError "Unexpected end of input while parsing message header.")
			else return bytes

	let Just fixedSig = parseSignature "yyyyuuu"
	fixedBytes <- getBytes 16

	let messageVersion = Data.ByteString.index fixedBytes 3
	when (messageVersion /= protocolVersion) (throwErrorT (UnmarshalError ("Unsupported protocol version: " ++ show messageVersion)))

	let eByte = Data.ByteString.index fixedBytes 0
	endianness <- case decodeEndianness eByte of
		Just x' -> return x'
		Nothing -> throwErrorT (UnmarshalError ("Invalid endianness: " ++ show eByte))

	let unmarshalSig = mapM unmarshal . signatureTypes
	let unmarshal' x bytes = case unWire (unmarshalSig x) endianness (UnmarshalState bytes 0) of
		WireRR x' _ -> return x'
		WireRL err  -> throwErrorT (UnmarshalError err)
	fixed <- unmarshal' fixedSig fixedBytes
	let messageType = fromJust (fromValue (fixed !! 1))
	let flags = fromJust (fromValue (fixed !! 2))
	let bodyLength = fromJust (fromValue (fixed !! 4)) :: Word32
	let serial = fromJust (fromVariant (Variant (fixed !! 5)))

	let fieldByteCount = fromJust (fromValue (fixed !! 6)) :: Word32
	let bodyPadding = padding (fromIntegral fieldByteCount + 16) 8

	-- Forbid messages larger than 'messageMaximumLength'
	let messageLength = 16 + toInteger fieldByteCount + toInteger bodyPadding + toInteger bodyLength
	when (messageLength > messageMaximumLength) $
		throwErrorT (UnmarshalError ("Message size " ++ show messageLength ++ " exceeds limit of " ++ show messageMaximumLength))

	let Just headerSig  = parseSignature "yyyyuua(yv)"
	fieldBytes <- getBytes (fromIntegral fieldByteCount)
	let headerBytes = Data.ByteString.append fixedBytes fieldBytes
	header <- unmarshal' headerSig headerBytes

	let fieldArray = Data.Vector.toList (fromJust (fromValue (header !! 6)))
	fields <- case runErrorM $ concat `liftM` mapM decodeField fieldArray of
		Left err -> throwErrorT err
		Right x -> return x
	_ <- getBytes (fromIntegral bodyPadding)
	let bodySig = findBodySignature fields
	bodyBytes <- getBytes (fromIntegral bodyLength)
	body <- unmarshal' bodySig bodyBytes
	y <- case runErrorM (buildReceivedMessage messageType fields) of
		Right x -> return x
		Left err -> throwErrorT (UnmarshalError ("Header field " ++ show err ++ " is required, but missing"))
	return (y serial flags (map Variant body))

findBodySignature :: [HeaderField] -> Signature
findBodySignature fields = fromMaybe (signature_ []) (listToMaybe [x | HeaderSignature x <- fields])

buildReceivedMessage :: Word8 -> [HeaderField] -> ErrorM String (Serial -> Word8 -> [Variant] -> ReceivedMessage)
buildReceivedMessage 1 fields = do
	path <- require "path" [x | HeaderPath x <- fields]
	member <- require "member name" [x | HeaderMember x <- fields]
	return $ \serial flags body -> let
		iface = listToMaybe [x | HeaderInterface x <- fields]
		dest = listToMaybe [x | HeaderDestination x <- fields]
		sender = listToMaybe [x | HeaderSender x <- fields]
		msg = MethodCall path iface member sender dest True True body
		in ReceivedMethodCall serial (setMethodCallFlags msg flags)

buildReceivedMessage 2 fields = do
	replySerial <- require "reply serial" [x | HeaderReplySerial x <- fields]
	return $ \serial _ body -> let
		dest = listToMaybe [x | HeaderDestination x <- fields]
		sender = listToMaybe [x | HeaderSender x <- fields]
		msg = MethodReturn replySerial sender dest body
		in ReceivedMethodReturn serial msg

buildReceivedMessage 3 fields = do
	name <- require "error name" [x | HeaderErrorName x <- fields]
	replySerial <- require "reply serial" [x | HeaderReplySerial x <- fields]
	return $ \serial _ body -> let
		dest = listToMaybe [x | HeaderDestination x <- fields]
		sender = listToMaybe [x | HeaderSender x <- fields]
		msg = MethodError name replySerial sender dest body
		in ReceivedMethodError serial msg

buildReceivedMessage 4 fields = do
	path <- require "path" [x | HeaderPath x <- fields]
	member <- require "member name" [x | HeaderMember x <- fields]
	iface <- require "interface" [x | HeaderInterface x <- fields]
	return $ \serial _ body -> let
		dest = listToMaybe [x | HeaderDestination x <- fields]
		sender = listToMaybe [x | HeaderSender x <- fields]
		msg = Signal path iface member sender dest body
		in ReceivedSignal serial msg

buildReceivedMessage messageType fields = return $ \serial _ body -> let
	sender = listToMaybe [x | HeaderSender x <- fields]
	msg = UnknownMessage messageType sender body
	in ReceivedUnknown serial msg

require :: String -> [a] -> ErrorM String a
require _     (x:_) = return x
require label _     = throwErrorM label

unmarshalMessage :: ByteString -> Either UnmarshalError ReceivedMessage
unmarshalMessage bytes = checkError (Get.runGet get bytes) where
	get = unmarshalMessageM getBytes
	
	-- wrap getByteString, so it will behave like transportGet and return
	-- a truncated result on EOF instead of throwing an exception.
	getBytes count = do
		remaining <- Get.remaining
		Get.getByteString (min remaining count)
	
	checkError (Left err) = Left (UnmarshalError err)
	checkError (Right x) = x

untilM :: Monad m => m Bool -> m a -> m [a]
untilM test comp = do
	done <- test
	if done
		then return []
		else do
			x <- comp
			xs <- untilM test comp
			return (x:xs)

-------------------------------------------------------------------------------
-- local ErrorT and MonadError, which don't have the silly Error => dependency
-- found in the "transformers" package.
-------------------------------------------------------------------------------

newtype ErrorM e a = ErrorM { runErrorM :: Either e a }

instance Functor (ErrorM e) where
	fmap f m = ErrorM $ case runErrorM m of
		Left err -> Left err
		Right x -> Right (f x)

instance Monad (ErrorM e) where
	return = ErrorM . Right
	(>>=) m k = case runErrorM m of
		Left err -> ErrorM (Left err)
		Right x -> k x

throwErrorM :: e -> ErrorM e a
throwErrorM = ErrorM . Left

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance Monad m => Functor (ErrorT e m) where
	fmap = liftM

instance Monad m => Monad (ErrorT e m) where
	return = ErrorT . return . Right
	(>>=) m k = ErrorT $ do
		x <- runErrorT m
		case x of
			Left l -> return (Left l)
			Right r -> runErrorT (k r)

throwErrorT :: Monad m => e -> ErrorT e m a
throwErrorT = ErrorT . return . Left
