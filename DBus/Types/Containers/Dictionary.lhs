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
{-# LANGUAGE DeriveDataTypeable #-}
module DBus.Types.Containers.Dictionary
	( Dictionary
	, toDictionary
	, fromDictionary
	, dictionaryItems
	, dictionaryFromItems
	, dictionarySignature
	) where

import Control.Arrow ((***))
import Data.Typeable (Typeable, cast)
import qualified DBus.Types.Signature as S
import qualified DBus.Types.Atom as A
import qualified DBus.Types.Containers.Variant as V
\end{code}
}

\subsubsection{Dictionaries}

Dictionaries are a key $\rightarrow$ value mapping, where the keys must be of an
{\tt Atomic} type, and the values may be of any valid DBus type.

\begin{code}
data Dictionary = Dictionary S.Signature [(A.Atom, V.Variant)]
	deriving (Show, Eq, Typeable)
\end{code}

\begin{code}
instance V.Variable Dictionary where
	defaultSignature _ = S.mkSignature' "a{yy}"
	toVariant x = V.Variant (dictionarySignature x) x
	fromVariant (V.Variant _ x) = cast x
\end{code}

\begin{code}
toDictionary :: (A.Atomic a, V.Variable b) => [(a, b)] -> Maybe Dictionary
toDictionary pairs = dictionaryFromItems kSig vSig pairs' where
	fakePair = head $ (undefined, undefined) : pairs
	kSigFake = V.defaultSignature . fst $ fakePair
	vSigFake = V.defaultSignature . snd $ fakePair
	
	pairs' = map (A.toAtom *** V.toVariant) pairs
	kSigReal = A.atomSignature  . fst . head $ pairs'
	vSigReal = V.variantSignature . snd . head $ pairs'
	
	(kSig, vSig) = case pairs of
		[] -> (kSigFake, vSigFake)
		_  -> (kSigReal, vSigReal)
\end{code}

\begin{code}
dictionaryFromItems :: S.Signature -> S.Signature -> [(A.Atom, V.Variant)]
                    -> Maybe Dictionary
dictionaryFromItems kSig vSig pairs = maybeDict where
	maybeDict = if hasSignature kSig ks && hasSignature vSig vs
		then Just (Dictionary sig pairs)
		else Nothing
	
	ks = map (A.atomToVariant . fst) pairs
	vs = map snd pairs
	
	kSig' = S.strSignature kSig
	vSig' = S.strSignature vSig
	sig = S.mkSignature' $ "a{" ++ kSig' ++ vSig' ++ "}"
\end{code}

\begin{code}
fromDictionary :: (A.Atomic a, V.Variable b) => Dictionary -> Maybe [(a, b)]
fromDictionary (Dictionary _ vs) = mapM fromVariant' vs where
	fromVariant' (k, v) = do
		k' <- A.fromAtom k
		v' <- V.fromVariant v
		return (k', v')
\end{code}

\begin{code}
dictionaryItems :: Dictionary -> [(A.Atom, V.Variant)]
dictionaryItems (Dictionary _ vs) = vs
\end{code}

\begin{code}
dictionarySignature :: Dictionary -> S.Signature
dictionarySignature (Dictionary s _) = s
\end{code}

\subsubsection*{Helper functions}

\begin{code}
hasSignature :: S.Signature -> [V.Variant] -> Bool
hasSignature _   [] = True
hasSignature sig vs = all (== sig) . map V.variantSignature $ vs
\end{code}
