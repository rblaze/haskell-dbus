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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
module DBus.Types.Containers.Variant
	( Variant (..)
	, Variable (..)
	, variantSignature
	, variantType
	) where

import Data.Typeable (Typeable, cast)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32, Int64)
import qualified DBus.Types.Signature as S
import qualified DBus.Types.ObjectPath as O
\end{code}
}

\subsubsection{Variants}

Any value which can be converted to a {\tt Variant} can be stored in D-Bus
containers or marshaled. Additionally, external types may implement the
{\tt Variable} interface to provide custom conversion to/from built-in D-Bus
types.

The {\tt defaultSignature} function will be passed {\tt unknown} to determine
the signature an empty array or dictionary.

\begin{code}
class Variable a where
	defaultSignature :: a -> S.Signature
	toVariant :: a -> Variant
	fromVariant :: Variant -> Maybe a
\end{code}

\begin{code}
data Variant = forall a. (Variable a, Typeable a, Show a) =>
               Variant S.Signature a
	deriving (Typeable)

instance Show Variant where
	showsPrec d (Variant sig x) = showParen (d > 10) $
		s "Variant " . s sigStr . s " " . showsPrec 11 x
		where sigStr = show . S.strSignature $ sig
		      s    = showString
\end{code}

Ghetto Eq instance -- the types contained in variants are known to have
fixed {\tt show} representations, so this works well enough.

It might not work right for {\tt Double}, though.

\begin{code}
instance Eq Variant where
	x == y = show x == show y
\end{code}

Variants are themselves variables.

\begin{code}
instance Variable Variant where
	defaultSignature _ = S.mkSignature' "v"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
variantSignature :: Variant -> S.Signature
variantSignature (Variant s _) = s
\end{code}

\begin{code}
variantType :: Variant -> S.Type
variantType = head . S.signatureTypes . variantSignature
\end{code}

\begin{code}
instance Variable Bool where
	defaultSignature _ = S.mkSignature' "b"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Word8 where
	defaultSignature _ = S.mkSignature' "y"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Word16 where
	defaultSignature _ = S.mkSignature' "q"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Word32 where
	defaultSignature _ = S.mkSignature' "u"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Word64 where
	defaultSignature _ = S.mkSignature' "t"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Int16 where
	defaultSignature _ = S.mkSignature' "n"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Int32 where
	defaultSignature _ = S.mkSignature' "i"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Int64 where
	defaultSignature _ = S.mkSignature' "x"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable Double where
	defaultSignature _ = S.mkSignature' "d"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable String where
	defaultSignature _ = S.mkSignature' "s"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable O.ObjectPath where
	defaultSignature _ = S.mkSignature' "o"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\begin{code}
instance Variable S.Signature where
	defaultSignature _ = S.mkSignature' "g"
	toVariant = toVariant'
	fromVariant = fromVariant'
\end{code}

\subsubsection*{Helper functions}

\begin{code}
toVariant' :: (Variable a, Typeable a, Show a) => a -> Variant
toVariant' x = Variant (defaultSignature x) x

fromVariant' :: Typeable a => Variant -> Maybe a
fromVariant' (Variant _ x) = cast x
\end{code}
