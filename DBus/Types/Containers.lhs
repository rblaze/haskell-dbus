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
module DBus.Types.Containers
	( -- * Containers
	
	  -- ** Variants
	  Variant
	, Variable (..)
	, variantSignature
	, variantType
	
	  -- ** Arrays
	, module DBus.Types.Containers.Array
	
	  -- ** Dictionaries
	, module DBus.Types.Containers.Dictionary
	
	  -- ** Structures
	, module DBus.Types.Containers.Structure
	) where

import DBus.Types.Containers.Variant
import DBus.Types.Containers.Array
import DBus.Types.Containers.Dictionary
import DBus.Types.Containers.Structure
\end{code}
}

\subsection{Containers}

\input{DBus/Types/Containers/Array.lhs}
\input{DBus/Types/Containers/Dictionary.lhs}
\input{DBus/Types/Containers/Structure.lhs}
\input{DBus/Types/Containers/Variant.lhs}
