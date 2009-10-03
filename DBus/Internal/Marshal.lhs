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
{-# OPTIONS_HADDOCK hide #-}
module DBus.Internal.Marshal (marshal) where

import Control.Arrow (first)
import Control.Monad (msum)
import qualified Control.Monad.State as S
import Data.Maybe (fromJust)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Binary.Put as P
import qualified Data.Binary.IEEE754 as IEEE

import DBus.Internal.Padding (padding, alignment)
import qualified DBus.Types as T
\end{code}
}

\clearpage
\section{Marshaling}
\subsection{\tt marshal}

\begin{code}
marshal :: T.Endianness -> [T.Variant] -> L.ByteString
marshal e vs = runMarshal (mapM_ marshalAny vs) e
\end{code}

\begin{code}
marshalAny :: T.Variant -> Marshal
marshalAny x = marshal' (T.variantType x) x where
	v :: T.Variable a => T.Variant -> a
	v = fromJust . T.fromVariant
	
	marshal' T.BooleanT          = bool       . v
	marshal' T.ByteT             = word8      . v
	marshal' T.UInt16T           = word16     . v
	marshal' T.UInt32T           = word32     . v
	marshal' T.UInt64T           = word64     . v
	marshal' T.Int16T            = int16      . v
	marshal' T.Int32T            = int32      . v
	marshal' T.Int64T            = int64      . v
	marshal' T.DoubleT           = double     . v
	marshal' T.StringT           = string     . v
	marshal' T.ObjectPathT       = objectPath . v
	marshal' T.SignatureT        = signature  . v
	marshal' (T.ArrayT _)        = array      . v
	marshal' (T.DictionaryT _ _) = dictionary . v
	marshal' (T.StructureT _)    = structure  . v
	marshal' T.VariantT          = variant    . v
\end{code}

\subsection{Atoms}

\begin{code}
bool :: Bool -> Marshal
bool x = word32 (if x then 1 else 0)
\end{code}

\begin{code}
word8 :: Word8 -> Marshal
word8 x = append (L.pack [x])
\end{code}

\begin{code}
word16 :: Word16 -> Marshal
word16 = appendPut P.putWord16be
\end{code}

\begin{code}
word32 :: Word32 -> Marshal
word32 = appendPut P.putWord32be
\end{code}

\begin{code}
word64 :: Word64 -> Marshal
word64 = appendPut P.putWord64be
\end{code}

\begin{code}
int16 :: Int16 -> Marshal
int16 = appendPut P.putWord16be . fromIntegral
\end{code}

\begin{code}
int32 :: Int32 -> Marshal
int32 = appendPut P.putWord32be . fromIntegral
\end{code}

\begin{code}
int64 :: Int64 -> Marshal
int64 = appendPut P.putWord64be . fromIntegral
\end{code}

\begin{code}
double :: Double -> Marshal
double = appendPut IEEE.putFloat64be
\end{code}

\begin{code}
string :: String -> Marshal
string x = do
	let bytes = fromString x
	word32 . fromIntegral . L.length $ bytes
	append bytes
	append (L.pack [0])
\end{code}

\begin{code}
objectPath :: T.ObjectPath -> Marshal
objectPath = string . T.strObjectPath
\end{code}

\begin{code}
signature :: T.Signature -> Marshal
signature x = do
	let bytes = fromString . T.strSignature $ x
	word8 . fromIntegral . L.length $ bytes
	append bytes
	append (L.pack [0])
\end{code}

\subsection{Containers}

\subsubsection{Arrays}

Marshaling arrays is complicated, because the array body must be marshaled
\emph{first} to calculate the array length. This requires building a
temporary marshaler, to get the padding right.

\begin{code}
array :: T.Array -> Marshal
array x = do
	(arrayPadding, arrayBytes) <- getArrayBytes x
	word32 . fromIntegral . L.length $ arrayBytes
	append arrayPadding
	append arrayBytes
\end{code}

\begin{code}
getArrayBytes :: T.Array -> MarshalM (L.ByteString, L.ByteString)
getArrayBytes x = do
	let vs = T.arrayItems x
	let [T.ArrayT itemType] = T.signatureTypes . T.arraySignature $ x
	s <- S.get
	(MarshalState _ afterLength) <- word32 0 >> S.get
	(MarshalState _ afterPadding) <- pad (alignment itemType) >> S.get
	(MarshalState _ afterItems) <- mapM_ marshalAny vs >> S.get
	
	let paddingBytes = L.drop (L.length afterLength) afterPadding
	let itemBytes = L.drop (L.length afterPadding) afterItems
	
	S.put s
	return (paddingBytes, itemBytes)
\end{code}

\subsubsection{Dictionaries}

\begin{code}
dictionary :: T.Dictionary -> Marshal
dictionary x = array x' where
	pairs = map (first T.atomToVariant) (T.dictionaryItems x)
	structs = [T.Structure [k,v] | (k,v) <- pairs]
	x' = fromJust . T.toArray $ structs
\end{code}

\subsubsection{Structures}

\begin{code}
structure :: T.Structure -> Marshal
structure (T.Structure xs) = pad 8 >> mapM_ marshalAny xs
\end{code}

\subsubsection{Variants}

\begin{code}
variant :: T.Variant -> Marshal
variant x = signature (T.variantSignature x) >> marshalAny x
\end{code}

\subsection{The {\tt Marshal} monad}

{\tt Marshal} implements stateful marshaling, which is required for padding
to be calculated properly.

\begin{code}
data MarshalState = MarshalState T.Endianness L.ByteString
type MarshalM = S.State MarshalState
type Marshal = MarshalM ()
\end{code}

\begin{code}
runMarshal :: Marshal -> T.Endianness -> L.ByteString
runMarshal m e = bytes where
	initialState = MarshalState e L.empty
	(MarshalState _ bytes) = S.execState m initialState
\end{code}

\begin{code}
append :: L.ByteString -> Marshal
append bs = do
	(MarshalState e bs') <- S.get
	S.put $ MarshalState e (L.append bs' bs)
\end{code}

Add padding to the end of the marshaled bytes, until the length is a
multiple of {\tt count}.

\begin{code}
pad :: Word8 -> Marshal
pad count = do
	(MarshalState _ bytes) <- S.get
	let padding' = padding (fromIntegral . L.length $ bytes) count
	append $ L.replicate (fromIntegral padding') 0
\end{code}

\begin{code}
appendPut :: (a -> P.Put) -> a -> Marshal
appendPut put x = do
	let bytes = P.runPut $ put x
	(MarshalState e _) <- S.get
	pad . fromIntegral . L.length $ bytes
	append $ case e of
		T.BigEndian -> bytes
		T.LittleEndian -> L.reverse bytes
\end{code}
