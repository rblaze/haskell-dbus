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
	) where

import qualified Data.ByteString.Char8 as Char8

import           Text.ParserCombinators.Parsec (Parser, runParser)

parseBytes :: Parser a -> Char8.ByteString -> Maybe a
parseBytes p bytes = case runParser p () "" (Char8.unpack bytes) of
	Left _ -> Nothing
	Right a -> Just a
