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
module DBus.Types.Serial
	( Serial (..)
	, nextSerial
	, firstSerial
	) where

import Data.Word (Word32)
import Data.Maybe (fromJust)
import qualified DBus.Types.Atom as A
import qualified DBus.Types.Containers as C
import DBus.Types.Signature (mkSignature)
\end{code}
}

\subsubsection{Message Serials}

\begin{code}
newtype Serial = Serial Word32
	deriving (Eq, Ord)

instance Show Serial where
	show (Serial x) = show x

instance A.Atomic Serial where
	toAtom (Serial x) = A.toAtom x

instance C.Variable Serial where
	defaultSignature _ = fromJust . mkSignature $ "u"
	toVariant (Serial x) = C.toVariant x
	fromVariant = fmap Serial . C.fromVariant
\end{code}

\begin{code}
nextSerial :: Serial -> Serial
nextSerial (Serial x) = Serial (x + 1)
\end{code}

\begin{code}
firstSerial :: Serial
firstSerial = Serial 1
\end{code}
