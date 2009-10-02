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
module DBus.Types.Containers
	( Variant
	, Variable
	, toVariant
	, fromVariant
	, defaultSignature
	, variantSignature
	, variantType
	
	, Array
	, toArray
	, fromArray
	, arrayItems
	, arrayFromItems
	, arraySignature
	
	, Dictionary
	, toDictionary
	, fromDictionary
	, dictionaryItems
	, dictionaryFromItems
	, dictionarySignature
	
	, Structure (..)
	, structureSignature
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

The {\tt defaultSignature} function will be passed {\tt unknown} to determine
the signature an empty array or dictionary.

\begin{code}
class Variable a where
	defaultSignature :: a -> S.Signature
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
	defaultSignature _ = sig' "v"
	toVariant = variant' "v"
	fromVariant = cast'
\end{code}

\begin{code}
variantSignature :: Variant -> S.Signature
variantSignature (Variant s _) = s
\end{code}

\begin{code}
variantType :: Variant -> S.Type
variantType = head . S.signatureTypes . variantSignature
\end{code}

\begin{code}
instance Variable Bool where
	defaultSignature _ = sig' "b"
	toVariant = variant' "b"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word8 where
	defaultSignature _ = sig' "y"
	toVariant = variant' "y"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word16 where
	defaultSignature _ = sig' "q"
	toVariant = variant' "q"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word32 where
	defaultSignature _ = sig' "u"
	toVariant = variant' "u"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Word64 where
	defaultSignature _ = sig' "t"
	toVariant = variant' "t"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Int16 where
	defaultSignature _ = sig' "n"
	toVariant = variant' "n"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Int32 where
	defaultSignature _ = sig' "i"
	toVariant = variant' "i"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Int64 where
	defaultSignature _ = sig' "x"
	toVariant = variant' "x"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable Double where
	defaultSignature _ = sig' "d"
	toVariant = variant' "d"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable String where
	defaultSignature _ = sig' "s"
	toVariant = variant' "s"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable O.ObjectPath where
	defaultSignature _ = sig' "o"
	toVariant = variant' "o"
	fromVariant = cast'
\end{code}

\begin{code}
instance Variable S.Signature where
	defaultSignature _ = sig' "g"
	toVariant = variant' "g"
	fromVariant = cast'
\end{code}

\subsubsection{Arrays}

Arrays are homogeneous sequences of any valid type. They may be
converted to and from standard Haskell lists, where the list contains elements with
a valid type.

\begin{code}
data Array = Array S.Signature [Variant]
	deriving (Show, Eq, Typeable)

instance Variable Array where
	defaultSignature _ = sig' "ay"
	toVariant x = Variant (arraySignature x) x
	fromVariant = cast'

toArray :: Variable a => [a] -> Maybe Array
toArray vs@([]) = Just $ Array sig [] where
	itemSig = defaultSignature . head $ undefined:vs
	sig = sig' $ 'a' : S.strSignature itemSig

toArray vs = arrayFromItems sig variants where
	variants = map toVariant vs
	sig = variantSignature . head $ variants

fromArray :: Variable a => Array -> Maybe [a]
fromArray (Array _ vs) = mapM fromVariant vs

arrayItems :: Array -> [Variant]
arrayItems (Array _ vs) = vs

arrayFromItems :: S.Signature -> [Variant] -> Maybe Array
arrayFromItems itemSig vs = maybeArray where
	maybeArray = if hasSignature itemSig vs
		then Just (Array sig vs)
		else Nothing
	sig = sig' $ 'a' : S.strSignature itemSig

arraySignature :: Array -> S.Signature
arraySignature (Array s _) = s
\end{code}

\subsubsection{Dictionaries}

Dictionaries are a key $\rightarrow$ value mapping, where the keys must be of an
{\tt Atomic} type, and the values may be of any valid DBus type.

\begin{code}
data Dictionary = Dictionary S.Signature [(A.Atom, Variant)]
	deriving (Show, Eq, Typeable)

instance Variable Dictionary where
	defaultSignature _ = sig' "a{yy}"
	toVariant x = Variant (dictionarySignature x) x
	fromVariant = cast'

toDictionary :: (A.Atomic a, Variable b) => [(a, b)] -> Maybe Dictionary
toDictionary vs@([]) = Just $ Dictionary sig [] where
	fake = head $ (undefined, undefined) : vs
	kSig = S.strSignature . defaultSignature . fst $ fake
	vSig = S.strSignature . defaultSignature . snd $ fake
	sig = sig' $ "a{" ++ kSig ++ vSig ++ "}"

toDictionary pairs = dictionaryFromItems kSig vSig pairs' where
	pairs' = map (A.toAtom *** toVariant) pairs
	kSig = A.atomSignature  . fst . head $ pairs'
	vSig = variantSignature . snd . head $ pairs'

fromDictionary :: (A.Atomic a, Variable b) => Dictionary -> Maybe [(a, b)]
fromDictionary (Dictionary _ vs) = mapM fromVariant' vs where
	fromVariant' (k, v) = do
		k' <- A.fromAtom k
		v' <- fromVariant v
		return (k', v')

dictionaryItems :: Dictionary -> [(A.Atom, Variant)]
dictionaryItems (Dictionary _ vs) = vs

dictionaryFromItems :: S.Signature -> S.Signature -> [(A.Atom, Variant)]
                    -> Maybe Dictionary
dictionaryFromItems kSig vSig pairs = maybeDict where
	maybeDict = if hasSignature kSig ks && hasSignature vSig vs
		then Just (Dictionary sig pairs)
		else Nothing
	
	ks = map (A.atomToVariant . fst) pairs
	vs = map snd pairs
	
	kSig' = S.strSignature kSig
	vSig' = S.strSignature vSig
	sig = sig' $ "a{" ++ kSig' ++ vSig' ++ "}"

dictionarySignature :: Dictionary -> S.Signature
dictionarySignature (Dictionary s _) = s
\end{code}

\subsubsection{Structures}

Structures contain a heterogenous list of DBus values. Any value may be
contained within a structure.

\begin{code}
data Structure = Structure [Variant]
	deriving (Show, Eq, Typeable)

instance Variable Structure where
	defaultSignature _ = sig' "()"
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
sig' = S.mkSignature'

variant' :: (Variable a, Typeable a, Show a) => String -> a -> Variant
variant' = Variant . sig'

cast' :: Typeable a => Variant -> Maybe a
cast' (Variant _ x) = cast x

hasSignature :: S.Signature -> [Variant] -> Bool
hasSignature _   [] = True
hasSignature sig vs = all (== sig) . map variantSignature $ vs
\end{code}
