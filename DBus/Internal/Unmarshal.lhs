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
{-# LANGUAGE ExistentialQuantification #-}
module DBus.Internal.Unmarshal (unmarshal) where

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import qualified Control.Monad.State as S
import qualified Control.Monad.Error as E
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Binary.Get as G
import qualified Data.Binary.IEEE754 as IEEE

import DBus.Internal.Padding (padding, alignment)
import qualified DBus.Types as T
\end{code}
}

\clearpage
\section{Unmarshaling}
\subsection{\tt unmarshal}

\begin{code}
unmarshal :: (E.Error e, E.MonadError e m)
             => T.Endianness -> T.Signature -> L.ByteString
             -> m [T.Variant]
unmarshal e sig bytes = either' where
	either' = case runUnmarshal x e bytes of
		Left  y -> E.throwError . E.strMsg . show $ y
		Right y -> return y
	x = mapM unmarshal' $ T.signatureTypes sig
\end{code}

\begin{code}
unmarshal' :: T.Type -> Unmarshal T.Variant
unmarshal' T.BooleanT            = fmap T.toVariant bool
unmarshal' T.ByteT               = fmap T.toVariant word8
unmarshal' T.UInt16T             = fmap T.toVariant word16
unmarshal' T.UInt32T             = fmap T.toVariant word32
unmarshal' T.UInt64T             = fmap T.toVariant word64
unmarshal' T.Int16T              = fmap T.toVariant int16
unmarshal' T.Int32T              = fmap T.toVariant int32
unmarshal' T.Int64T              = fmap T.toVariant int64
unmarshal' T.DoubleT             = fmap T.toVariant double
unmarshal' T.StringT             = fmap T.toVariant string
unmarshal' T.ObjectPathT         = fmap T.toVariant objectPath
unmarshal' T.SignatureT          = fmap T.toVariant signature
unmarshal' (T.ArrayT t)          = fmap T.toVariant $ array t
unmarshal' (T.DictionaryT kt vt) = fmap T.toVariant $ dictionary kt vt
unmarshal' (T.StructureT ts)     = fmap T.toVariant $ structure ts
unmarshal' T.VariantT            = fmap T.toVariant variant
\end{code}

\subsection{Atoms}

\begin{code}
bool :: Unmarshal Bool
bool = word32 >>= \x -> case x of
	0 -> return False
	1 -> return True
	_ -> E.throwError $ Invalid "boolean" x
\end{code}

\begin{code}
word8 :: Unmarshal Word8
word8 = do
	bs <- consume 1
	let [b] = L.unpack bs
	return b
\end{code}

\begin{code}
word16 :: Unmarshal Word16
word16 = eitherEndian 2 G.getWord16be G.getWord16le
\end{code}

\begin{code}
word32 :: Unmarshal Word32
word32 = eitherEndian 4 G.getWord32be G.getWord32le
\end{code}

\begin{code}
word64 :: Unmarshal Word64
word64 = eitherEndian 8 G.getWord64be G.getWord64le
\end{code}

\begin{code}
int16 :: Unmarshal Int16
int16 = fmap fromIntegral $ eitherEndian 2 G.getWord16be G.getWord16le
\end{code}

\begin{code}
int32 :: Unmarshal Int32
int32 = fmap fromIntegral $ eitherEndian 4 G.getWord32be G.getWord32le
\end{code}

\begin{code}
int64 :: Unmarshal Int64
int64 = fmap fromIntegral $ eitherEndian 8 G.getWord64be G.getWord64le
\end{code}

\begin{code}
double :: Unmarshal Double
double = eitherEndian 8 IEEE.getFloat64be IEEE.getFloat64le
\end{code}

\begin{code}
string :: Unmarshal String
string = do
	byteCount <- word32
	bytes <- consume . fromIntegral $ byteCount
	skipNulls 1
	return . toString $ bytes
\end{code}

\begin{code}
objectPath :: Unmarshal T.ObjectPath
objectPath = do
	s <- string
	fromMaybe T.mkObjectPath s "object path"
\end{code}

\begin{code}
signature :: Unmarshal T.Signature
signature = do
	byteCount <- word8
	bytes <- consume . fromIntegral $ byteCount
	skipNulls 1
	fromMaybe T.mkSignature (toString bytes) "signature"
\end{code}

\subsection{Containers}

\subsubsection{Arrays}

\begin{code}
array :: T.Type -> Unmarshal T.Array
array t = do
	let getOffset = do
		(UnmarshalState _ _ o) <- get
		return o
	let sig = T.mkSignature' . T.typeString $ t
	
	byteCount <- word32
	skipPadding (alignment t)
	start <- getOffset
	let end = start + fromIntegral byteCount
	vs <- untilM (fmap (>= end) getOffset) (unmarshal' t)
	end' <- getOffset
	assert (end == end') "Array contained fewer bytes than expected."
	fromMaybe (T.arrayFromItems sig) vs "array"
\end{code}

\subsubsection{Dictionaries}

\begin{code}
dictionary :: T.Type -> T.Type -> Unmarshal T.Dictionary
dictionary kt vt = do
	arr <- array $ T.StructureT [kt, vt]
	structs <- fromMaybe T.fromArray arr "dictionary"
	pairs <- mapM mkPair structs
	let kSig = T.mkSignature' . T.typeString $ kt
	let vSig = T.mkSignature' . T.typeString $ vt
	fromMaybe (T.dictionaryFromItems kSig vSig) pairs "dictionary"

mkPair :: T.Structure -> Unmarshal (T.Atom, T.Variant)
mkPair (T.Structure [k, v]) = do
	k' <- fromMaybe T.atomFromVariant k "dictionary key"
	return (k', v)
mkPair s = E.throwError $ Invalid "dictionary item" s
\end{code}

\subsubsection{Structures}

\begin{code}
structure :: [T.Type] -> Unmarshal T.Structure
structure ts = do
	skipPadding 8
	fmap T.Structure $ mapM unmarshal' ts
\end{code}

\subsubsection{Variants}

\begin{code}
variant :: Unmarshal T.Variant
variant = do
	sig <- signature
	t <- case T.signatureTypes sig of
		[t'] -> return t'
		_    -> E.throwError $ Invalid "variant signature"
		                     $ T.strSignature sig
	unmarshal' t
\end{code}

\subsection{The {\tt Unmarshal} monad}

\begin{code}
data UnmarshalState = UnmarshalState T.Endianness L.ByteString Word64
type Unmarshal = E.ErrorT UnmarshalError (S.State UnmarshalState)
\end{code}

\begin{code}
runUnmarshal :: Unmarshal a -> T.Endianness -> L.ByteString
                -> Either UnmarshalError a
runUnmarshal x e bytes = S.evalState (E.runErrorT x) state where
	state = UnmarshalState e bytes 0
\end{code}

\begin{code}
get :: Unmarshal UnmarshalState
get = E.lift S.get

put :: UnmarshalState -> Unmarshal ()
put = E.lift . S.put
\end{code}

\begin{code}
consume :: Word64 -> Unmarshal L.ByteString
consume count = do
	(UnmarshalState e bytes offset) <- get
	let bytes' = L.drop (fromIntegral offset) bytes
	let x = L.take (fromIntegral count) bytes'
	if L.length x == fromIntegral count
		then do
			let offset' = offset + count
			put $ UnmarshalState e bytes offset'
			return x
		else E.throwError $ UnexpectedEOF offset
\end{code}

\begin{code}
skipPadding :: Word8 -> Unmarshal ()
skipPadding count = do
	(UnmarshalState _ _ offset) <- get
	bytes <- consume $ padding offset count
	assert (L.all (== 0) bytes) "Non-zero bytes in padding."
\end{code}

\begin{code}
skipNulls :: Word8 -> Unmarshal ()
skipNulls count = do
	bytes <- consume $ fromIntegral count
	assert (L.all (== 0) bytes) "Non-zero bytes in padding."
\end{code}

\begin{code}
eitherEndian :: Word8 -> G.Get a -> G.Get a -> Unmarshal a
eitherEndian count be le = do
	skipPadding count
	(UnmarshalState e _ _) <- get
	bs <- consume . fromIntegral $ count
	let get' = case e of
		T.BigEndian -> be
		T.LittleEndian -> le
	return $ G.runGet get' bs
\end{code}

\begin{code}
untilM :: Monad m => m Bool -> m a -> m [a]
untilM test comp = do
	done <- test
	if done
		then return []
		else do
			x <- comp
			xs <- untilM test comp
			return $ x:xs
\end{code}

\subsection{Errors}

\begin{code}
data UnmarshalError = UnexpectedEOF Word64
                    | forall a. (Show a) => Invalid String a
                    | GenericError String

instance E.Error UnmarshalError where
	strMsg = GenericError

instance Show UnmarshalError where
	show (UnexpectedEOF pos) = "Unexpected EOF at position " ++ show pos
	show (Invalid label x)   = "Invalid " ++ label ++ ": " ++ show x
	show (GenericError msg)  = "Error unmarshaling: " ++ msg
\end{code}

\begin{code}
assert :: Bool -> String -> Unmarshal ()
assert True  _   = return ()
assert False msg = E.throwError $ E.strMsg msg
\end{code}

\begin{code}
fromMaybe :: Show a => (a -> Maybe b) -> a -> String -> Unmarshal b
fromMaybe f x s = maybe (E.throwError $ Invalid s x) return $ f x
\end{code}
