% Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
% 
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\ignore{
\begin{code}
module DBus.Message
	( -- * Message structure and fields
	  Message (..)
	, Flag (..)
	, HeaderField (..)
	
	  -- * Message types
	  -- ** Method calls
	, MethodCall (..)
	
	  -- ** Method returns
	, MethodReturn (..)
	
	  -- ** Errors
	, Error (..)
	
	  -- ** Signals
	, Signal (..)
	
	  -- * Received messages
	, ReceivedMessage (..)
	, receivedSerial
	, receivedSender
	
	  -- * (Un)marshaling
	, U.UnmarshalError (..)
	, M.MarshalError (..)
	, marshal
	, unmarshal
	) where

import Control.Monad (unless, when)
import qualified Control.Monad.Error as E
import Data.Bits ((.|.), (.&.))
import Data.Word (Word8, Word32)
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromJust, fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as S

import qualified DBus.Marshal as M
import qualified DBus.Unmarshal as U
import DBus.Padding (padding)
import qualified DBus.Types as T
import DBus.Constants (protocolVersion)
\end{code}
}

\clearpage
\section{Messages}

A message represents a single message, with a header and body. Some parts
of the header, such as the serial, endianness, and body length, are not
included in the message --- instead, they are generated when a message is
marshalled.

\begin{code}
class Message a where
	messageTypeCode     :: a -> Word8
	messageHeaderFields :: a -> [HeaderField]
	messageFlags        :: a -> S.Set Flag
	messageBody         :: a -> [T.Variant]
\end{code}

\subsection{Flags}

Flags are represented as the integral value of each flag OR'd into a single
byte.

The instance of {\tt Ord} only exists for storing flags in a set. Flags have
no inherent ordering.

\begin{code}
data Flag = NoReplyExpected
          | NoAutoStart
	deriving (Show, Eq, Ord)
\end{code}

\begin{code}
encodeFlags :: [Flag] -> Word8
encodeFlags flags = foldr (.|.) 0 $ map flagValue flags where
	flagValue NoReplyExpected = 0x1
	flagValue NoAutoStart     = 0x2
\end{code}

\begin{code}
decodeFlags :: Word8 -> [Flag]
decodeFlags flagsByte = flags where
	flagSet = [(0x1, NoReplyExpected), (0x2, NoAutoStart)]
	flags = flagSet >>= \(x, y) -> [y | flagsByte .&. x > 0]
\end{code}

\subsection{Header fields}

\begin{code}
data HeaderField
	= Path        T.ObjectPath
	| Interface   T.InterfaceName
	| Member      T.MemberName
	| ErrorName   T.ErrorName
	| ReplySerial T.Serial
	| Destination T.BusName
	| Sender      T.BusName
	| Signature   T.Signature
	deriving (Show, Eq)
\end{code}

\begin{code}
header' :: T.Variable a => Word8 -> a -> T.Variant
header' code x = T.toVariant $ T.Structure
	[ T.toVariant code
	, T.toVariant $ T.toVariant x
	]

unheader :: T.Variant -> Maybe (Word8, T.Variant)
unheader structV = do
	struct <- T.fromVariant structV
	(c, v) <- case struct of
		T.Structure [x, y] -> return  (x, y)
		_                  -> Nothing
	c' <- T.fromVariant c
	v' <- T.fromVariant v
	return (c', v')

instance T.Variable HeaderField where
	defaultSignature _ = T.mkSignature' "(yv)"
	
	toVariant (Path x)        = header' 1 x
	toVariant (Interface x)   = header' 2 x
	toVariant (Member x)      = header' 3 x
	toVariant (ErrorName x)   = header' 4 x
	toVariant (ReplySerial x) = header' 5 x
	toVariant (Destination x) = header' 6 x
	toVariant (Sender x)      = header' 7 x
	toVariant (Signature x)   = header' 8 x
	
	fromVariant v = unheader v >>= \v' -> case v' of
		(1, x) -> fmap Path        $ T.fromVariant x
		(2, x) -> fmap Interface   $ T.fromVariant x
		(3, x) -> fmap Member      $ T.fromVariant x
		(4, x) -> fmap ErrorName   $ T.fromVariant x
		(5, x) -> fmap ReplySerial $ T.fromVariant x
		(6, x) -> fmap Destination $ T.fromVariant x
		(7, x) -> fmap Sender      $ T.fromVariant x
		(8, x) -> fmap Signature   $ T.fromVariant x
		_      -> Nothing
\end{code}

\subsection{Message types}

\subsubsection{Method calls}

\begin{code}
data MethodCall = MethodCall
	{ methodCallPath        :: T.ObjectPath
	, methodCallMember      :: T.MemberName
	, methodCallInterface   :: Maybe T.InterfaceName
	, methodCallDestination :: Maybe T.BusName
	, methodCallFlags       :: S.Set Flag
	, methodCallBody        :: [T.Variant]
	}
	deriving (Show, Eq)

instance Message MethodCall where
	messageTypeCode _ = 1
	messageFlags      = methodCallFlags
	messageBody       = methodCallBody
	messageHeaderFields m = concat
		[ [ Path    $ methodCallPath m
		  ,  Member $ methodCallMember m
		  ]
		, maybe' Interface . methodCallInterface $ m
		, maybe' Destination . methodCallDestination $ m
		]
\end{code}

\subsubsection{Method returns}

\begin{code}
data MethodReturn = MethodReturn
	{ methodReturnSerial      :: T.Serial
	, methodReturnDestination :: Maybe T.BusName
	, methodReturnFlags       :: S.Set Flag
	, methodReturnBody        :: [T.Variant]
	}
	deriving (Show, Eq)

instance Message MethodReturn where
	messageTypeCode _ = 2
	messageFlags      = methodReturnFlags
	messageBody       = methodReturnBody
	messageHeaderFields m = concat
		[ [ ReplySerial $ methodReturnSerial m
		  ]
		, maybe' Destination . methodReturnDestination $ m
		]
\end{code}

\subsubsection{Errors}

\begin{code}
data Error = Error
	{ errorName        :: T.ErrorName
	, errorSerial      :: T.Serial
	, errorDestination :: Maybe T.BusName
	, errorFlags       :: S.Set Flag
	, errorBody        :: [T.Variant]
	}
	deriving (Show, Eq)

instance Message Error where
	messageTypeCode _ = 3
	messageFlags      = errorFlags
	messageBody       = errorBody
	messageHeaderFields m = concat
		[ [ ErrorName   $ errorName m
		  , ReplySerial $ errorSerial m
		  ]
		, maybe' Destination . errorDestination $ m
		]
\end{code}

\subsubsection{Signals}

\begin{code}
data Signal = Signal
	{ signalPath        :: T.ObjectPath
	, signalMember      :: T.MemberName
	, signalInterface   :: T.InterfaceName
	, signalDestination :: Maybe T.BusName
	, signalFlags       :: S.Set Flag
	, signalBody        :: [T.Variant]
	}
	deriving (Show, Eq)

instance Message Signal where
	messageTypeCode _ = 4
	messageFlags      = signalFlags
	messageBody       = signalBody
	messageHeaderFields m = concat
		[ [ Path      $ signalPath m
		  , Member    $ signalMember m
		  , Interface $ signalInterface m
		  ]
		, maybe' Destination . signalDestination $ m
		]
\end{code}

\begin{code}
maybe' :: (a -> b) -> Maybe a -> [b]
maybe' f = maybe [] (\x' -> [f x'])
\end{code}

\subsection{Message headers}

\begin{code}
data MessageHeader = MessageHeader
	{ headerEndianness :: T.Endianness
	, headerTypeCode   :: Word8
	, headerFlags      :: S.Set Flag
	, headerProtocol   :: Word8
	, headerBodySize   :: Word32
	, headerSerial     :: T.Serial
	, headerFields     :: [HeaderField]
	} deriving (Show, Eq)
\end{code}

\begin{code}
buildHeader :: Message a => T.Endianness -> T.Serial -> a -> Word32
               -> Either M.MarshalError MessageHeader
buildHeader endianness serial m bodyLen = do
	let bodySig = concatMap (T.typeString . T.variantType) $ messageBody m
	bodySig' <- case T.mkSignature bodySig of
		Just x -> Right x
		Nothing -> Left $ M.InvalidBodySignature bodySig
	
	let fields = Signature bodySig' : messageHeaderFields m
	return $ MessageHeader
		endianness
		(messageTypeCode m)
		(messageFlags m)
		protocolVersion
		bodyLen
		serial
		fields
\end{code}

\subsection{Marshaling}

{\tt marshal} converts a message into a byte string, suitable for sending
over the bus.

\begin{code}
marshal :: Message a => T.Endianness -> T.Serial -> a
           -> Either M.MarshalError L.ByteString
marshal e s m = do
	bodyBytes <- M.marshal e $ messageBody m
	let bodyLength = fromIntegral . L.length $ bodyBytes
	header <- fmap marshalHeader $ buildHeader e s m bodyLength
	headerBytes <- M.marshal e $ header ++ [T.toVariant (T.Structure [])]
	let allBytes = L.append headerBytes bodyBytes
	let messageLength = fromIntegral . L.length $ allBytes
	when (messageLength > messageMaximumLength)
		(E.throwError $ M.MessageTooLong messageLength)
	return allBytes
\end{code}

\begin{code}
messageMaximumLength :: Word32
messageMaximumLength = 134217728
\end{code}

Build the header struct for a message. Endianness, the serial, and body
length are provided.

\begin{code}
marshalHeader :: MessageHeader -> [T.Variant]
marshalHeader h = map ($ h)
	[ T.toVariant . headerEndianness
	, T.toVariant . headerTypeCode
	, T.toVariant . encodeFlags . S.toList . headerFlags
	, T.toVariant . headerProtocol
	, T.toVariant . headerBodySize
	, T.toVariant . headerSerial
	, T.toVariant . fromJust . T.toArray . headerFields
	]
\end{code}

\subsection{Unmarshaling}

\subsubsection{Received messages}

Any messages parsed from the connection are stored in a
{\tt ReceivedMessage} value, so clients can know which type of message
was received, where it was sent from, and its serial.

Unknown message types are parsed, but only to get the serial and bus name
(which might be useful for returning an {\tt Error}). Clients should ignore
unknown messages.

\begin{code}
data ReceivedMessage
	= ReceivedMethodCall   T.Serial (Maybe T.BusName) MethodCall
	| ReceivedMethodReturn T.Serial (Maybe T.BusName) MethodReturn
	| ReceivedError        T.Serial (Maybe T.BusName) Error
	| ReceivedSignal       T.Serial (Maybe T.BusName) Signal
	| ReceivedUnknown      T.Serial (Maybe T.BusName)
	deriving (Show, Eq)

receivedSerial :: ReceivedMessage -> T.Serial
receivedSerial (ReceivedMethodCall   s _ _) = s
receivedSerial (ReceivedMethodReturn s _ _) = s
receivedSerial (ReceivedError        s _ _) = s
receivedSerial (ReceivedSignal       s _ _) = s
receivedSerial (ReceivedUnknown      s _) = s

receivedSender :: ReceivedMessage -> Maybe T.BusName
receivedSender (ReceivedMethodCall   _ s _) = s
receivedSender (ReceivedMethodReturn _ s _) = s
receivedSender (ReceivedError        _ s _) = s
receivedSender (ReceivedSignal       _ s _) = s
receivedSender (ReceivedUnknown      _ s) = s
\end{code}

Unmarshaling is a three-step process: retrieve the raw bytes, parse the
header, then parse the body. With the header and body, it is possible to
construct a {\tt ReceivedMessage} of the proper type and attributes.

\begin{code}
unmarshal :: Monad m => (Word32 -> m L.ByteString)
          -> m (Either U.UnmarshalError ReceivedMessage)
unmarshal getBytes = E.runErrorT $ do
	(endianness, headerBytes, bodyBytes) <- getRaw $ E.lift . getBytes
	header <- parseHeader endianness headerBytes
	let signature = findBodySignature . headerFields $ header
	body <- unmarshal' endianness signature bodyBytes
	
	mkReceived
		(headerTypeCode header)
		(headerSerial header)
		(headerFields header)
		(headerFlags header)
		body
\end{code}

\begin{code}
unmarshal' :: Monad m => T.Endianness -> T.Signature -> L.ByteString
              -> E.ErrorT U.UnmarshalError m [T.Variant]
unmarshal' e s bytes = case U.unmarshal e s bytes of
	Left x -> E.throwError x
	Right x -> return x
\end{code}

First, some minimal parsing is required to retrieve the full byte buffer
from the input. This depends on the fixed structure of the message header to
pull in every required byte without fully parsing the header fields.

\begin{code}
type RawMessage = (T.Endianness, L.ByteString, L.ByteString)
getRaw :: Monad m => (Word32 -> E.ErrorT U.UnmarshalError m L.ByteString)
          -> E.ErrorT U.UnmarshalError m RawMessage
getRaw get = do
	let fixed'Sig = T.mkSignature' "yyyy"
	let fixedSig  = T.mkSignature' "yyyyuuu"
	
	-- Protocol version
	fixedBytes <- get 16
	fixed' <- unmarshal' T.LittleEndian fixed'Sig fixedBytes
	checkMatchingVersion $ fixed' !! 3
	
	-- Endianness
	endianness <- getEndianness $ fixed' !! 0
	
	-- Header field bytes
	fixed <- unmarshal' endianness fixedSig fixedBytes
	let fieldByteCount = fromJust . T.fromVariant $ fixed !! 6
	fieldBytes <- get fieldByteCount
	
	-- Post-field padding
	let bodyPadding = padding (fromIntegral fieldByteCount + 16) 8
	get . fromIntegral $ bodyPadding
	
	-- Body bytes
	let bodyByteCount = fromJust . T.fromVariant $ fixed !! 4
	bodyBytes <- get bodyByteCount
	return (endianness, L.append fixedBytes fieldBytes, bodyBytes)
\end{code}

\begin{code}
checkMatchingVersion :: Monad m => T.Variant -> E.ErrorT U.UnmarshalError m ()
checkMatchingVersion v = unless (messageVersion == protocolVersion) $
	E.throwError $ U.ProtocolVersionMismatch messageVersion
	where messageVersion = fromJust . T.fromVariant $ v
\end{code}

\begin{code}
getEndianness :: Monad m => T.Variant -> E.ErrorT U.UnmarshalError m T.Endianness
getEndianness v = maybe bad return $ T.fromVariant v where
	word = fromJust . T.fromVariant $ v :: Word8
	bad = E.throwError $ U.Invalid "endianness code" (show word)
\end{code}

Next, the header is parsed into a proper {\tt MessageHeader}.

\begin{code}
parseHeader :: Monad m => T.Endianness -> L.ByteString
               -> E.ErrorT U.UnmarshalError m MessageHeader
parseHeader endianness bytes = do
	let signature = T.mkSignature' "yyyyuua(yv)"
	vs <- unmarshal' endianness signature bytes
	let fieldArray = fromJust . T.fromVariant $ vs !! 6
	let fields = mapMaybe T.fromVariant $ T.arrayItems fieldArray
	let flagByte = fromJust . T.fromVariant $ vs !! 2
	return $ MessageHeader
		endianness
		(fromJust . T.fromVariant $ vs !! 1)
		(S.fromList . decodeFlags $ flagByte)
		(fromJust . T.fromVariant $ vs !! 3)
		(fromJust . T.fromVariant $ vs !! 4)
		(fromJust . T.fromVariant $ vs !! 5)
		fields
\end{code}

If the header fields contain a body signature, it should be used for
unmarshaling the body. Otherwise, the body is assumed to be empty.

\begin{code}
findBodySignature :: [HeaderField] -> T.Signature
findBodySignature fields = fromMaybe empty signature where
	empty = T.mkSignature' ""
	signature = listToMaybe [x | Signature x <- fields]
\end{code}

\begin{code}
mkReceived 1 = mkReceived' ReceivedMethodCall mkMethodCall
mkReceived 2 = mkReceived' ReceivedMethodReturn mkMethodReturn
mkReceived 3 = mkReceived' ReceivedError mkError
mkReceived 4 = mkReceived' ReceivedSignal mkSignal
mkReceived _ = undefined -- mkReceived' ReceivedUnknown    mkUnknown
\end{code}

\begin{code}
mkReceived' mkRecv mkMsg serial fields flags body = do
	msg <- mkMsg fields flags body
	let sender = listToMaybe [x | Sender x <- fields]
	return $ mkRecv serial sender msg
\end{code}

\begin{code}
mkMethodCall :: Monad m => [HeaderField] -> S.Set Flag -> [T.Variant]
                -> E.ErrorT U.UnmarshalError m MethodCall
mkMethodCall fields flags body = do
	path <- require "path" [x | Path x <- fields]
	member <- require "member name" [x | Member x <- fields]
	let iface = listToMaybe [x | Interface x <- fields]
	let dest = listToMaybe [x | Destination x <- fields]
	return $ MethodCall path member iface dest flags body
\end{code}

\begin{code}
mkMethodReturn :: Monad m => [HeaderField] -> S.Set Flag -> [T.Variant]
                  -> E.ErrorT U.UnmarshalError m MethodReturn
mkMethodReturn fields flags body = do
	serial <- require "reply serial" [x | ReplySerial x <- fields]
	let dest = listToMaybe [x | Destination x <- fields]
	return $ MethodReturn serial dest flags body
\end{code}

\begin{code}
mkError :: Monad m => [HeaderField] -> S.Set Flag -> [T.Variant]
           -> E.ErrorT U.UnmarshalError m Error
mkError fields flags body = do
	name <- require "error name" [x | ErrorName x <- fields]
	serial <- require "reply serial" [x | ReplySerial x <- fields]
	let dest = listToMaybe [x | Destination x <- fields]
	return $ Error name serial dest flags body
\end{code}

\begin{code}
mkSignal :: Monad m => [HeaderField] -> S.Set Flag -> [T.Variant]
            -> E.ErrorT U.UnmarshalError m Signal
mkSignal fields flags body = do
	path <- require "path" [x | Path x <- fields]
	member <- require "member" [x | Member x <- fields]
	iface <- require "interface" [x | Interface x <- fields]
	let dest = listToMaybe [x | Destination x <- fields]
	return $ Signal path member iface dest flags body
\end{code}

\begin{code}
require :: Monad m => String -> [a] -> E.ErrorT U.UnmarshalError m a
require _     (x:_) = return x
require label _     = E.throwError $ U.RequiredHeaderFieldMissing label
\end{code}
