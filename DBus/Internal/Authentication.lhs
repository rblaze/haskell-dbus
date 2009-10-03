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
module DBus.Internal.Authentication (authenticate) where

import Data.Char (ord)
import Data.Word (Word32)
import Data.List (isPrefixOf)
import System.Posix.User (getRealUserID)
import Text.Printf (printf)
\end{code}
}

\section{Authentication}

\begin{code}
authenticate :: (String -> IO ()) -> (Word32 -> IO String)
                -> IO ()
authenticate put get = do
	put "\x00"
\end{code}

{\tt EXTERNAL} authentication is performed using the process's real user
ID, converted to a string, and then hex-encoded.

\begin{code}
	uid <- getRealUserID
	let authToken = concatMap (printf "%02X" . ord) (show uid)
	put $ "AUTH EXTERNAL " ++ authToken ++ "\r\n"
\end{code}

If authentication was successful, the server responds with {\tt OK
<server GUID>}.  The GUID is intended to enable connection sharing, which
is currently unimplemented, so it's ignored.

\begin{code}
	response <- readUntil '\n' get
	if "OK" `isPrefixOf` response
		then put "BEGIN\r\n"
		else do
			putStrLn $ "response = " ++ show response
			error "Server rejected authentication token."
\end{code}

\begin{code}
readUntil :: Monad m => Char -> (Word32 -> m String) -> m String
readUntil = readUntil' ""

readUntil' :: Monad m => String -> Char -> (Word32 -> m String) -> m String
readUntil' xs c f = do
	[x] <- f 1
	let xs' = xs ++ [x]
	if x == c
		then return xs'
		else readUntil' xs' c f
\end{code}
