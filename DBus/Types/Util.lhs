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

\clearpage
\section{Misc. utility functions}

\ignore{
\begin{code}
{-# OPTIONS_HADDOCK hide #-}
module DBus.Types.Util
	( checkLength
	, parseMaybe
	, mkUnsafe
	) where

import Text.Parsec (Parsec, parse)
\end{code}
}

\begin{code}
checkLength :: Int -> String -> Maybe String
checkLength length' s | length s <= length' = Just s
checkLength _ _ = Nothing
\end{code}

\begin{code}
parseMaybe :: Parsec String () a -> String -> Maybe a
parseMaybe p = either (const Nothing) Just . parse p ""
\end{code}

\begin{code}
mkUnsafe :: Show a => String -> (a -> Maybe b) -> a -> b
mkUnsafe label f x = case f x of
	Just x' -> x'
	Nothing -> error $ "Invalid " ++ label ++ ": " ++ show x
\end{code}
