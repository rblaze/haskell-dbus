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

\subsection{Atoms}

\ignore{
\begin{code}
{-# LANGUAGE TypeSynonymInstances #-}
\end{code}
}
\begin{code}
module DBus.Types.Atom (
	 Atom
	,Atomic
	,toAtom
	,fromAtom
	,atomSignature
	,atomToVariant
	,atomFromVariant
	)
\end{code}
\ignore{
\begin{code}
where
import Control.Monad (msum)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import qualified DBus.Types.Signature as S
import qualified DBus.Types.ObjectPath as O
import {-# SOURCE #-} qualified DBus.Types.Containers as C
\end{code}
}

Any atomic value can be used as a dictionary key. Types which might be
used for dict keys should implement {\tt Atomic}.

\begin{code}
newtype Atom = Atom C.Variant
	deriving (Show, Eq)

class C.Variable a => Atomic a where
	toAtom :: a -> Atom

atomToVariant :: Atom -> C.Variant
atomToVariant (Atom x) = x

atomFromVariant :: C.Variant -> Maybe Atom
atomFromVariant v = msum as where
	v' :: C.Variable a => Maybe a
	v' = C.fromVariant v
	fa :: (Atomic a, Functor f) => f a -> f Atom
	fa = fmap toAtom
	
	as = [fa (v' :: Maybe Bool)
	     ,fa (v' :: Maybe Word8)
	     ,fa (v' :: Maybe Word16)
	     ,fa (v' :: Maybe Word32)
	     ,fa (v' :: Maybe Word64)
	     ,fa (v' :: Maybe Int16)
	     ,fa (v' :: Maybe Int32)
	     ,fa (v' :: Maybe Int64)
	     ,fa (v' :: Maybe Double)
	     ,fa (v' :: Maybe String)
	     ,fa (v' :: Maybe O.ObjectPath)
	     ,fa (v' :: Maybe S.Signature)
	     ]
\end{code}

\begin{code}
fromAtom :: C.Variable a => Atom -> Maybe a
fromAtom (Atom x) = C.fromVariant x
\end{code}

\begin{code}
atomSignature :: Atom -> S.Signature
atomSignature (Atom v) = C.variantSignature v
\end{code}

\subsubsection{Built-in atomic types}

\begin{code}
toAtom' :: Atomic a => a -> Atom
toAtom' = Atom . C.toVariant

instance Atomic Bool         where toAtom = toAtom'
instance Atomic Word8        where toAtom = toAtom'
instance Atomic Word16       where toAtom = toAtom'
instance Atomic Word32       where toAtom = toAtom'
instance Atomic Word64       where toAtom = toAtom'
instance Atomic Int16        where toAtom = toAtom'
instance Atomic Int32        where toAtom = toAtom'
instance Atomic Int64        where toAtom = toAtom'
instance Atomic Double       where toAtom = toAtom'
instance Atomic String       where toAtom = toAtom'
instance Atomic O.ObjectPath where toAtom = toAtom'
instance Atomic S.Signature  where toAtom = toAtom'
\end{code}
