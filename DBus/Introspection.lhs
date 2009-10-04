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
module DBus.Introspection
	( Object (..)
	, Interface (..)
	, Method (..)
	, Parameter (..)
	, Property (..)
	, PropertyAccess (..)
	, fromXML
	) where

import Control.Arrow ((>>>), (&&&))
import Control.Monad (mapM)
import Data.Maybe (listToMaybe, maybeToList)
import qualified Text.XML.HXT.Arrow as A
import Text.XML.HXT.Arrow.ParserInterface (parseXmlDoc)
import qualified Text.XML.HXT.DOM.XmlNode as XN
import qualified DBus.Types as T
\end{code}
}

\clearpage
\section{Introspection}

\begin{code}
data Object = Object T.ObjectPath [Interface] [Object]
	deriving (Show)

data Interface = Interface T.InterfaceName [Method] [Signal] [Property]
	deriving (Show)

data Method = Method T.MemberName [Parameter] [Parameter]
	deriving (Show)

data Signal = Signal T.MemberName [Parameter]
	deriving (Show)

data Parameter = Parameter String T.Signature
	deriving (Show)

data Property = Property String T.Signature [PropertyAccess]
	deriving (Show)

data PropertyAccess = Read | Write
	deriving (Show)
\end{code}

\begin{code}
fromXML :: T.ObjectPath -> String -> Maybe Object
fromXML path text = listToMaybe $ (A.runLA arrow) ("", text) where
	arrow = parseXmlDoc
	        >>> A.removeAllWhiteSpace
	        >>> A.single (A.hasName "node")
	        >>> A.arrL (maybeToList . parseRoot path)
\end{code}

\begin{code}
parseRoot :: T.ObjectPath -> A.XmlTree -> Maybe Object
parseRoot defaultPath t = do
	path <- case getAttr "name" t of
		"" -> Just defaultPath
		x  -> T.mkObjectPath x
	parseObject' path t
\end{code}

\begin{code}
parseChild :: T.ObjectPath -> A.XmlTree -> Maybe Object
parseChild parentPath t = do
	let parentPath' = T.strObjectPath parentPath
	pathSegment <- case getAttr "name" t of
		"" -> Nothing
		x  -> Just x
	path <- T.mkObjectPath (parentPath' ++ "/" ++ pathSegment)
	parseObject' path t
\end{code}

\begin{code}
parseObject' :: T.ObjectPath -> A.XmlTree -> Maybe Object
parseObject' path t = do
	interfaces <- children parseInterface (A.hasName "interface") t
	children' <- children (parseChild path) (A.hasName "node") t
	return $ Object path interfaces children'
\end{code}

\begin{code}
parseInterface :: A.XmlTree -> Maybe Interface
parseInterface t = do
	name <- T.mkInterfaceName $ getAttr "name" t
	methods <- children parseMethod (A.hasName "method") t
	signals <- children parseSignal (A.hasName "signal") t
	properties <- children parseProperty (A.hasName "property") t
	return $ Interface name methods signals properties
\end{code}

\begin{code}
parseMethod :: A.XmlTree -> Maybe Method
parseMethod t = do
	name <- T.mkMemberName $ getAttr "name" t
	paramsIn <- children parseParameter (isParam ["in", ""]) t
	paramsOut <- children parseParameter (isParam ["out"]) t
	return $ Method name paramsIn paramsOut
\end{code}

\begin{code}
parseSignal :: A.XmlTree -> Maybe Signal
parseSignal t = do
	name <- T.mkMemberName $ getAttr "name" t
	params <- children parseParameter (isParam ["out", ""]) t
	return $ Signal name params
\end{code}

\begin{code}
parseParameter :: A.XmlTree -> Maybe Parameter
parseParameter t = do
	let name = getAttr "name" t
	sig <- parseType t
	return $ Parameter name sig
\end{code}

\begin{code}
parseProperty :: A.XmlTree -> Maybe Property
parseProperty t = do
	let name = getAttr "name" t
	sig <- parseType t
	access <- case getAttr "access" t of
		""          -> Just []
		"read"      -> Just [Read]
		"write"     -> Just [Write]
		"readwrite" -> Just [Read, Write]
		_           -> Nothing
	return $ Property name sig access
\end{code}

\begin{code}
parseType :: A.XmlTree -> Maybe T.Signature
parseType t = do
	sig <- T.mkSignature $ getAttr "type" t
	case T.signatureTypes sig of
		[t'] -> T.mkSignature (T.typeString t')
		_    -> Nothing
\end{code}

\begin{code}
getAttr :: String -> A.XmlTree -> String
getAttr name t = case A.runLA (A.getAttrValue name) t of
	(x:_) -> x
	_     -> ""
\end{code}

\begin{code}
children f a = mapM f . A.runLA (A.getChildren >>> a)
\end{code}

\begin{code}
isParam dirs = A.hasName "arg"
               >>> (A.arr id &&& A.getAttrValue "direction")
               >>> A.arrL (\(t, p) -> if elem p dirs then [t] else [])
\end{code}
