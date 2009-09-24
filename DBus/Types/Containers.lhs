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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
module DBus.Types.Containers (
	 Variant
	,Variable
	,toVariant
	,fromVariant
	,variantSignature
	
	,Array
	,toArray
	,fromArray
	,arrayItems
	,arrayFromItems
	,arraySignature
	
	,Dictionary
	,toDictionary
	,fromDictionary
	,dictionaryItems
	,dictionaryFromItems
	,dictionarySignature
	
	,Structure (..)
	,structureSignature
	) where
import Control.Arrow ((***))
import Data.Typeable (Typeable, cast)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import qualified DBus.Types.Signature as S
import qualified DBus.Types.ObjectPath as O
import qualified DBus.Types.Atom as A
\end{code}
}
\subsection{Containers}

\subsubsection{Variants}

Any value which can be converted to a {\tt Variant} can be stored in D-Bus
containers or marshaled. Additionally, external types may implement the
{\tt Variable} interface to provide custom conversion to/from built-in D-Bus
types.

\begin{code}
class Variable a where
	toVariant :: a -> Variant
	fromVariant :: Variant -> Maybe a
\end{code}

\begin{code}
data Variant = forall a. (Variable a, Typeable a, Show a) =>
               Variant S.Signature a
	deriving (Typeable)

instance Show Variant where
	showsPrec d (Variant sig x) = showParen (d > 10) $
		s "Variant " . s sigStr . s " " . showsPrec 11 x
		where sigStr = show . S.strSignature $ sig
		      s    = showString
\end{code}

Ghetto Eq instance -- the types contained in variants are known to have
fixed {\tt show} representations, so this works well enough.

It might not work right for {\tt Double}, though.

\begin{code}
instance Eq Variant where
	x == y = show x == show y
\end{code}

Variants are themselves variables.

\begin{code}
instance Variable Variant where
	toVariant = variant' "v"
	fromVariant = cast'
\end{code}

\begin{code}
variantSignature :: Variant -> S.Signature
variantSignature (Variant s _) = s
\end{code}

\begin{code}
instance Variable Bool where
	toVariant = variant' "b"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word8 where
	toVariant = variant' "y"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word16 where
	toVariant = variant' "q"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word32 where
	toVariant = variant' "u"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word64 where
	toVariant = variant' "t"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Int16 where
	toVariant = variant' "n"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Int32 where
	toVariant = variant' "i"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Int64 where
	toVariant = variant' "x"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Double where
	toVariant = variant' "d"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable String where
	toVariant = variant' "s"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable O.ObjectPath where
	toVariant = variant' "o"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable S.Signature where
	toVariant = variant' "g"
	fromVariant = cast'
\end{code}

\subsubsection{Arrays}

Arrays are homogeneous sequences of any valid type. They may be
converted to and from standard Haskell lists, where the list contains elements with
a valid type.

\begin{code}
data Array = Array [Variant]
	deriving (Show, Eq, Typeable)

instance Variable Array where
	toVariant x = Variant (arraySignature x) x
	fromVariant = cast'

toArray :: Variable a => [a] -> Maybe Array
toArray = arrayFromItems . map toVariant

fromArray :: Variable a => Array -> Maybe [a]
fromArray (Array vs) = mapM fromVariant vs

arrayItems :: Array -> [Variant]
arrayItems (Array vs) = vs

arrayFromItems :: [Variant] -> Maybe Array
arrayFromItems = fmap Array . hasSameSignature

arraySignature :: Array -> S.Signature
arraySignature (Array []) = sig' "ay"
arraySignature (Array ((Variant sig _):_)) = sig' ('a':S.strSignature sig)
\end{code}

\subsubsection{Dictionaries}

Dictionaries are a key $\rightarrow$ value mapping, where the keys must be of an
{\tt Atomic} type, and the values may be of any valid DBus type.

\begin{code}
data Dictionary = Dictionary [(A.Atom, Variant)]
	deriving (Show, Eq, Typeable)

instance Variable Dictionary where
	toVariant x = Variant (dictionarySignature x) x
	fromVariant = cast'

toDictionary :: (A.Atomic a, Variable b) => [(a, b)] -> Maybe Dictionary
toDictionary = dictionaryFromItems  . map (A.toAtom *** toVariant)

fromDictionary :: (A.Atomic a, Variable b) => Dictionary -> Maybe [(a, b)]
fromDictionary (Dictionary vs) = mapM fromVariant' vs where
	fromVariant' (k, v) = do
		k' <- A.fromAtom k
		v' <- fromVariant v
		return (k', v')

dictionaryItems :: Dictionary -> [(A.Atom, Variant)]
dictionaryItems (Dictionary vs) = vs

dictionaryFromItems :: [(A.Atom, Variant)] -> Maybe Dictionary
dictionaryFromItems pairs = do
	let ks = [A.atomToVariant k | (k,_) <- pairs]
	let vs = [v | (_,v) <- pairs]
	ks' <- mapM A.atomFromVariant =<< hasSameSignature ks
	vs' <- hasSameSignature vs
	return . Dictionary $ zip ks' vs'

dictionarySignature :: Dictionary -> S.Signature
dictionarySignature (Dictionary []) = sig' "a{yy}"
dictionarySignature (Dictionary ((k,v):_)) = case (A.atomToVariant k, v) of
	-- Use case here to allow unwrapping of existential constructors
	(Variant kSig _, Variant vSig _) -> let
		kSig' = S.strSignature kSig
		vSig' = S.strSignature vSig
		sig = sig' $ "a{" ++ kSig' ++ vSig' ++ "}"
		in sig
\end{code}

\subsubsection{Structures}

Structures contain a heterogenous list of DBus values. Any value may be
contained within a structure.

\begin{code}
data Structure = Structure [Variant]
	deriving (Show, Eq, Typeable)

instance Variable Structure where
	toVariant x = Variant (structureSignature x) x
	fromVariant = cast'

structureSignature :: Structure -> S.Signature
structureSignature (Structure vs) = sig where
	sigs = [s | (Variant s _) <- vs]
	sig = sig' $ "(" ++ concatMap S.strSignature sigs ++ ")"
\end{code}

\subsubsection*{Helper functions}

\begin{code}
sig' :: String -> S.Signature
sig' x = let Just sig = S.mkSignature x in sig

variant' :: (Variable a, Typeable a, Show a) => String -> a -> Variant
variant' = Variant . sig'

cast' :: Typeable a => Variant -> Maybe a
cast' (Variant _ x) = cast x

hasSameSignature :: [Variant] -> Maybe [Variant]
hasSameSignature vs = if allIdentical (map variantSignature vs)
	then Just vs
	else Nothing

allIdentical :: (Eq a) => [a] -> Bool
allIdentical     [] = True
allIdentical (x:xs) = all (x ==) xs
\end{code}
