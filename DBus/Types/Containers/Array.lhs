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
module DBus.Types.Containers.Array
	( Array
	, toArray
	, fromArray
	, arrayItems
	, arrayFromItems
	, arraySignature
	) where

import Data.Typeable (Typeable, cast)
import qualified DBus.Types.Signature as S
import qualified DBus.Types.Containers.Variant as V
\end{code}
}

\subsubsection{Arrays}

Arrays are homogeneous sequences of any valid type. They may be
converted to and from standard Haskell lists, where the list contains elements with
a valid type.

\begin{code}
data Array = Array S.Signature [V.Variant]
	deriving (Show, Eq, Typeable)
\end{code}

\begin{code}
instance V.Variable Array where
	defaultSignature _ = S.mkSignature' "ay"
	toVariant x = V.Variant (arraySignature x) x
	fromVariant (V.Variant _ x) = cast x
\end{code}

\begin{code}
toArray :: V.Variable a => [a] -> Maybe Array
toArray vs = arrayFromItems sig variants where
	variants = map V.toVariant vs
	sig = case vs of
		[] -> V.defaultSignature . head $ undefined : vs
		_  -> V.variantSignature . head $ variants
\end{code}

\begin{code}
arrayFromItems :: S.Signature -> [V.Variant] -> Maybe Array
arrayFromItems itemSig vs = array where
	array = if sameSignature
		then Just (Array sig vs)
		else Nothing
	sig = S.mkSignature' $ 'a' : S.strSignature itemSig
	sameSignature = all (== itemSig) . map V.variantSignature $ vs
\end{code}

\begin{code}
fromArray :: V.Variable a => Array -> Maybe [a]
fromArray (Array _ vs) = mapM V.fromVariant vs
\end{code}

\begin{code}
arrayItems :: Array -> [V.Variant]
arrayItems (Array _ vs) = vs
\end{code}

\begin{code}
arraySignature :: Array -> S.Signature
arraySignature (Array s _) = s
\end{code}
