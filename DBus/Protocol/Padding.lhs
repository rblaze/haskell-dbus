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
module DBus.Protocol.Padding (
	 padding
	,padByType
	) where
import Data.Word (Word8, Word64)
import qualified DBus.Types as T
\end{code}
}

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
padByType :: T.Type -> Word8
padByType T.BooleanT    = 4
padByType T.ByteT       = 1
padByType T.UInt16T     = 2
padByType T.UInt32T     = 4
padByType T.UInt64T     = 8
padByType T.Int16T      = 2
padByType T.Int32T      = 4
padByType T.Int64T      = 8
padByType T.DoubleT     = 8
padByType T.StringT     = 4
padByType T.ObjectPathT = 4
padByType T.SignatureT  = 1
padByType T.VariantT    = 1
padByType (T.ArrayT _)  = 4
padByType (T.DictT _ _) = 4
padByType (T.StructT _) = 8
\end{code}

