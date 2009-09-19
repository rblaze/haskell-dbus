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
module DBus.Types.ObjectPath (
	 ObjectPath
	,mkObjectPath
	,strObjectPath
	) where
import Data.Typeable (Typeable)
import qualified Text.Parsec as P
import DBus.Types.Util (parseMaybe)
\end{code}
}

\section{Object paths}

An object path may be one of

\begin{itemize}
\item The root path, {\tt "/"}.
\item {\tt '/'}, followed by one or more element names. Each element name
      contains characters in the set {\tt [a-zA-Z0-9\_]}, and must have at
      least one character.
\end{itemize}

Element names are separated by {\tt '/'}, and the path may not end in
{\tt '/'} unless it is the root path.

\begin{code}
newtype ObjectPath = ObjectPath String
	deriving (Show, Eq, Ord, Typeable)

mkObjectPath :: String -> Maybe ObjectPath
mkObjectPath s = parseMaybe path' s where
	c = P.oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
	path = P.char '/' >>= P.optional . P.sepBy (P.many1 c) . P.char
	path' = path >> P.eof >> return (ObjectPath s)

strObjectPath :: ObjectPath -> String
strObjectPath (ObjectPath x) = x
\end{code}
