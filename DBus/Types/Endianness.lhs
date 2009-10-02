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
module DBus.Types.Endianness (Endianness (..)) where

import Data.Word (Word8)
import qualified DBus.Types.Containers as C
import DBus.Types.Signature (mkSignature')
\end{code}
}

\subsubsection{Endianness}

\begin{code}
data Endianness = LittleEndian | BigEndian
	deriving (Show, Eq)
\end{code}

Endianness is encoded in the header as a single ASCII character, either
{\tt 'l'} ({\tt 0x6C}) for little-endian or {\tt 'B'} ({\tt 0x42}) for
big-endian.

\begin{code}
instance C.Variable Endianness where
	defaultSignature _ = mkSignature' "y"
	toVariant LittleEndian = C.toVariant (0x6C :: Word8)
	toVariant BigEndian = C.toVariant (0x42 :: Word8)
	fromVariant v = do
		b <- C.fromVariant v :: Maybe Word8
		case b of
			0x6C -> Just LittleEndian
			0x42 -> Just BigEndian
			_    -> Nothing
\end{code}
