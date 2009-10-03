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
module DBus.Internal.Padding
	( padding
	, alignment
	) where

import Data.Word (Word8, Word64)
import qualified DBus.Types as T
\end{code}
}

\section{Value padding}

\begin{code}
padding :: Word64 -> Word8 -> Word64
padding current count = required where
	count' = fromIntegral count
	missing = mod current count'
	required = if missing > 0
		then count' - missing
		else 0
\end{code}

\begin{code}
alignment :: T.Type -> Word8
alignment T.BooleanT          = 4
alignment T.ByteT             = 1
alignment T.UInt16T           = 2
alignment T.UInt32T           = 4
alignment T.UInt64T           = 8
alignment T.Int16T            = 2
alignment T.Int32T            = 4
alignment T.Int64T            = 8
alignment T.DoubleT           = 8
alignment T.StringT           = 4
alignment T.ObjectPathT       = 4
alignment T.SignatureT        = 1
alignment (T.ArrayT _)        = 4
alignment (T.DictionaryT _ _) = 4
alignment (T.StructureT _)    = 8
alignment T.VariantT          = 1
\end{code}

