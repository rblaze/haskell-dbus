{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
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

module DBusTests.Wire (test_Wire) where

import           Test.Chell

import qualified Data.ByteString.Char8 ()

import           DBus

test_Wire :: Suite
test_Wire = suite "Wire"
	test_Unmarshal

test_Unmarshal :: Suite
test_Unmarshal = suite "unmarshal"
	test_UnmarshalUnexpectedEof

test_UnmarshalUnexpectedEof :: Test
test_UnmarshalUnexpectedEof = assertions "unexpected-eof" $ do
	let unmarshaled = unmarshal "0"
	$assert (left unmarshaled)
	
	let Left err = unmarshaled
	$assert (equal
		(unmarshalErrorMessage err)
		"Unexpected end of input while parsing message header.")
