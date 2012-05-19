-- Copyright (C) 2009-2012 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module DBus.Util
	( parseBytes
	, randomUUID
	) where

import           Control.Monad (replicateM)
import qualified Data.ByteString.Char8 as Char8
import           Data.Word (Word16)
import           System.Random (randomRIO)
import           Text.Printf (printf)

import           Text.ParserCombinators.Parsec (Parser, runParser)

parseBytes :: Parser a -> Char8.ByteString -> Maybe a
parseBytes p bytes = case runParser p () "" (Char8.unpack bytes) of
	Left _ -> Nothing
	Right a -> Just a

-- | Generate a UUID, which is 128 bits of random data hex-encoded.
randomUUID :: IO String
randomUUID = do
	-- The version of System.Random bundled with ghc < 7.2 doesn't define
	-- instances for any of the fixed-length word types, so we imitate
	-- them using the instance for Int.
	--
	-- 128 bits is 8 16-bit integers. We use chunks of 16 instead of 32
	-- because Int is not guaranteed to be able to store a Word32.
	let hexInt16 i = printf "%04x" (i :: Int)
	int16s <- replicateM 8 (randomRIO (0, fromIntegral (maxBound :: Word16)))
	return (concatMap hexInt16 int16s)
