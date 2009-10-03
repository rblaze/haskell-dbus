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
module DBus.Types.Containers.Structure
	( Structure (..)
	, structureSignature
	) where

import Data.Typeable (Typeable, cast)
import qualified DBus.Types.Signature as S
import qualified DBus.Types.Containers.Variant as V
\end{code}
}

\subsubsection{Structures}

Structures contain a heterogenous list of DBus values. Any value may be
contained within a structure.

\begin{code}
data Structure = Structure [V.Variant]
	deriving (Show, Eq, Typeable)

instance V.Variable Structure where
	defaultSignature _ = S.mkSignature' "()"
	toVariant x = V.Variant (structureSignature x) x
	fromVariant (V.Variant _ x) = cast x

structureSignature :: Structure -> S.Signature
structureSignature (Structure vs) = sig where
	sigs = [s | (V.Variant s _) <- vs]
	sig = S.mkSignature' $ "(" ++ concatMap S.strSignature sigs ++ ")"
\end{code}
