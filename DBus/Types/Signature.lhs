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
module DBus.Types.Signature
	( -- * Signatures
	  Signature
	, Type(..)
	, signatureTypes
	, mkSignature
	, mkSignature'
	, strSignature
	, typeString
	) where

import Data.Typeable (Typeable)
import Text.Parsec (char, (<|>), many, eof)
import DBus.Internal.Util (checkLength, parseMaybe, mkUnsafe)
\end{code}
}

\subsubsection{Signatures}

Valid DBus type signatures must obey certain rules, such as "dict entries
may only appear in arrays". A signature is guaranteed to be valid according
to these rules. Creating them requires using the {\tt parseSignature}
function, which will convert a valid DBus signature string into a
{\tt Signature}.

\begin{code}
data Signature = Signature { signatureTypes :: [Type] }
	deriving (Eq, Typeable)
\end{code}

Use a custom {\tt Show} instance to avoid displaying signature types directly.

\begin{code}
instance Show Signature where
	showsPrec d s = showParen (d > 10) $
		showString' ["Signature \"", strSignature s, "\""] where
		showString' = foldr (.) id . map showString
\end{code}

Parse a signature string such as {\tt "yya{is}"} to a signature. If the
string is invalid, {\tt Nothing} will be returned instead.

\begin{code}
mkSignature :: String -> Maybe Signature
mkSignature = (parseMaybe sigParser =<<) . checkLength 255 where
	sigParser = do
		types <- many parseType
		eof
		return $ Signature types
	parseType = parseAtom <|> parseContainer
	parseContainer =
		    parseArray
		<|> parseStruct
		<|> (char 'v' >> return VariantT)
	parseAtom =
		    (char 'b' >> return BooleanT)
		<|> (char 'y' >> return ByteT)
		<|> (char 'n' >> return Int16T)
		<|> (char 'q' >> return UInt16T)
		<|> (char 'i' >> return Int32T)
		<|> (char 'u' >> return UInt32T)
		<|> (char 'x' >> return Int64T)
		<|> (char 't' >> return UInt64T)
		<|> (char 'd' >> return DoubleT)
		<|> (char 's' >> return StringT)
		<|> (char 'o' >> return ObjectPathT)
		<|> (char 'g' >> return SignatureT)
	parseArray = do
		char 'a'
		parseDict <|> do
		t <- parseType
		return $ ArrayT t
	parseDict = do
		char '{'
		keyType <- parseAtom
		valueType <- parseType
		char '}'
		return $ DictionaryT keyType valueType
	parseStruct = do
		char '('
		types <- many parseType
		char ')'
		return $ StructureT types
\end{code}

\begin{code}
mkSignature' :: String -> Signature
mkSignature' = mkUnsafe "signature" mkSignature
\end{code}

Convert a signature to a string, for marshaling over the bus or display.

\begin{code}
strSignature :: Signature -> String
strSignature (Signature ts) = concatMap typeString ts
\end{code}

Data types which may be contained in a signature. No checks are performed to
ensure correct nesting when using these, so all generation of signatures
must go through {\tt parseSignature}.

\begin{code}
data Type
	= BooleanT
	| ByteT
	| Int16T
	| UInt16T
	| Int32T
	| UInt32T
	| Int64T
	| UInt64T
	| DoubleT
	| StringT
	| ObjectPathT
	| SignatureT
	| ArrayT Type
	| DictionaryT Type Type
	| StructureT [Type]
	| VariantT
	deriving (Show, Eq)

typeString :: Type -> String
typeString BooleanT            = "b"
typeString ByteT               = "y"
typeString Int16T              = "n"
typeString UInt16T             = "q"
typeString Int32T              = "i"
typeString UInt32T             = "u"
typeString Int64T              = "x"
typeString UInt64T             = "t"
typeString DoubleT             = "d"
typeString StringT             = "s"
typeString ObjectPathT         = "o"
typeString SignatureT          = "g"
typeString (ArrayT t)          = 'a' : typeString t
typeString (DictionaryT kt vt) = "a{" ++ typeString kt ++ typeString vt ++ "}"
typeString (StructureT ts)     = "(" ++ concatMap typeString ts ++ ")"
typeString VariantT            = "v"
\end{code}
