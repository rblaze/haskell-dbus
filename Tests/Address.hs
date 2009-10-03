{-
  Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
  
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  any later version.
  
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Tests.Address (addressProperties) where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Map (toList)
import Data.Maybe (isJust, isNothing)
import Test.QuickCheck
import Tests.Instances (sized')
import DBus.Address

addressProperties = concat
	[ prop_Valid
	, prop_Invalid
	, [ property prop_Identity
	  , property prop_MultipleAddresses
	  , property prop_IgnoreTrailingSemicolon
	  , property prop_IgnoreTrailingComma
	  ]
	]

-- Addresses can be converted to strings and back safely
prop_Identity x = parseAddresses (strAddress x) == Just [x]

-- Multiple addresses may be parsed
prop_MultipleAddresses x y = parseAddresses joined == Just [x, y] where
	joined = concat [strAddress x, ";", strAddress y]

-- Trailing semicolons are ignored
prop_IgnoreTrailingSemicolon x = parseAddresses appended == Just [x] where
	appended = strAddress x ++ ";"

-- Trailing commas are ignored
prop_IgnoreTrailingComma x = hasParams ==> ignoresComma where
	hasParams = not . null $ params
	ignoresComma = parseAddresses (strAddress x ++ ",") == Just [x]
	params = toList . addressParameters $ x

prop_Valid = let valid = property . isJust . parseAddresses in
	[ valid ":"
	, valid "a:"
	, valid "a:b=c"
	, valid "a:;"
	, valid "a:;b:"
	]

prop_Invalid = let invalid = property . isNothing . parseAddresses in
	[ invalid ""
	, invalid "a"
	, invalid "a:b"
	, invalid "a:b="
	]

instance Arbitrary Address where
	coarbitrary = undefined
	arbitrary = genAddress where
		optional = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "-_/\\*."
		methodChars = filter (flip notElem ":;") ['!'..'~']
		keyChars = filter (flip notElem "=;,") ['!'..'~']
		
		genMethod = sized' 0 $ elements methodChars
		genParam = do
			key <- genKey
			value <- genValue
			return . concat $ [key, "=", value]
		
		genKey = sized' 1 $ elements keyChars
		genValue = oneof [encodedValue, plainValue]
		genHex = elements $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']
		encodedValue = do
			x1 <- genHex
			x2 <- genHex
			return ['%', x1, x2]
		plainValue = sized' 1 $ elements optional
		
		genParams = do
			params <- sized' 0 genParam
			let params' = intercalate "," params
			extraComma <- if null params
				then return ""
				else elements ["", ","]
			return $ concat [params', extraComma]
		
		genAddress = do
			m <- genMethod
			params <- genParams
			extraSemicolon <- elements ["", ";"]
			let addrStr = concat [m, ":", params, extraSemicolon]
			let Just [addr] = parseAddresses addrStr
			return addr

