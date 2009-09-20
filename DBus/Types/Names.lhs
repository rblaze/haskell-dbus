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
module DBus.Types.Names (
	 InterfaceName
	,mkInterfaceName
	,strInterfaceName
	
	,BusName
	,mkBusName
	,strBusName
	
	,MemberName
	,mkMemberName
	,strMemberName
	
	,ErrorName
	,mkErrorName
	,strErrorName
	) where
import Text.Parsec ((<|>))
import qualified Text.Parsec as P
import qualified DBus.Types.Atom as A
import qualified DBus.Types.Containers as C
import DBus.Types.Util (checkLength, parseMaybe)
\end{code}
}

\subsubsection{Names}

All names have a length limit of 255 characters.

\subsubsection{Interface names}

An interface name consists of two or more {\tt '.'}-separated elements. Each
element constists of characters from the set {\tt [a-zA-Z0-9\_]}, may not
start with a digit, and must have at least one character.

\begin{code}
newtype InterfaceName = InterfaceName String
	deriving (Show, Eq, Ord)
\end{code}

\begin{code}
instance C.Variable InterfaceName where
	toVariant = C.toVariant . A.toAtom
	fromVariant = (mkInterfaceName =<<) . C.fromVariant
instance A.Atomic InterfaceName where
	toAtom = A.toAtom . strInterfaceName
\end{code}

\begin{code}
mkInterfaceName :: String -> Maybe InterfaceName
mkInterfaceName s = checkLength 255 s >>= parseMaybe parser where
	c = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
	c' = c ++ ['0'..'9']
	element = P.oneOf c >> P.many (P.oneOf c')
	name = element >> P.many1 (P.char '.' >> element)
	parser = name >> P.eof >> return (InterfaceName s)
\end{code}

\begin{code}
strInterfaceName :: InterfaceName -> String
strInterfaceName (InterfaceName x) = x
\end{code}

\subsubsection{Error names}

Error names have the same rules as interface names, so just re-use that
validation logic.

\begin{code}
newtype ErrorName = ErrorName String
	deriving (Show, Eq, Ord)
\end{code}

\begin{code}
instance C.Variable ErrorName where
	toVariant = C.toVariant . A.toAtom
	fromVariant = (mkErrorName =<<) . C.fromVariant
instance A.Atomic ErrorName where
	toAtom = A.toAtom . strErrorName
\end{code}

\begin{code}
mkErrorName :: String -> Maybe ErrorName
mkErrorName = fmap (ErrorName . strInterfaceName) . mkInterfaceName
\end{code}

\begin{code}
strErrorName :: ErrorName -> String
strErrorName (ErrorName x) = x
\end{code}

\subsubsection{Bus names}

\begin{code}
newtype BusName = BusName String
	deriving (Show, Eq, Ord)
\end{code}

\begin{code}
instance C.Variable BusName where
	toVariant = C.toVariant . A.toAtom
	fromVariant = (mkBusName =<<) . C.fromVariant
instance A.Atomic BusName where
	toAtom = A.toAtom . strBusName
\end{code}

\begin{code}
mkBusName :: String -> Maybe BusName
mkBusName s = checkLength 255 s >>= parseMaybe parser where
	c = ['a'..'z'] ++ ['A'..'Z'] ++ "_-"
	c' = c ++ ['0'..'9']
	parser = (unique <|> wellKnown) >> P.eof >> return (BusName s)
	unique = P.char ':' >> elems c'
	wellKnown = elems c
	elems start = elem' start >> P.many1 (P.char '.' >> elem' start)
	elem' start = P.oneOf start >> P.many (P.oneOf c')
\end{code}

\begin{code}
strBusName :: BusName -> String
strBusName (BusName x) = x
\end{code}

\subsubsection{Member names}

\begin{code}
newtype MemberName = MemberName String
	deriving (Show, Eq, Ord)
\end{code}

\begin{code}
instance C.Variable MemberName where
	toVariant = C.toVariant . A.toAtom
	fromVariant = (mkMemberName =<<) . C.fromVariant
instance A.Atomic MemberName where
	toAtom = A.toAtom . strMemberName
\end{code}

\begin{code}
mkMemberName :: String -> Maybe MemberName
mkMemberName s = checkLength 255 s >>= parseMaybe parser where
	c = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
	c' = c ++ ['0'..'9']
	name = P.oneOf c >> P.many (P.oneOf c')
	parser = name >> P.eof >> return (MemberName s)
\end{code}

\begin{code}
strMemberName :: MemberName -> String
strMemberName (MemberName x) = x
\end{code}
