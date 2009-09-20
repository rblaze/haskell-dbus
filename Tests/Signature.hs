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

module Tests.Signature (signatureProperties) where

import Data.Maybe (isJust, isNothing)
import Test.QuickCheck
import Tests.Instances ()
import DBus.Types.Signature

signatureProperties =
	[ property (prop_Equality :: Signature -> Bool)
	, property (prop_Equality :: Type -> Bool)
	, property prop_TypeToSig
	, property prop_Ident
	, property prop_Length0
	, property prop_Length254
	, property prop_Length255
	, property prop_Length256
	, property prop_Invalid0
	, property prop_Invalid1
	, property prop_Invalid2
	, property prop_show0
	, property prop_show1
	]

-- Signatures and type equality
prop_Equality x = x == x

-- Types can be converted to signatures via mkSignature
prop_TypeToSig x = typeString x == strSignature sig where
	Just sig = mkSignature (typeString x)

-- Signatures can be safely converted to strings and back
prop_Ident x = mkSignature (strSignature x) == Just x

-- Signatures may be no more than 255 characters long
prop_Length0   = isJust    . mkSignature $ ""
prop_Length254 = isJust    . mkSignature . replicate 254 $ 'y'
prop_Length255 = isJust    . mkSignature . replicate 255 $ 'y'
prop_Length256 = isNothing . mkSignature . replicate 256 $ 'y'

-- Invalid signatures are not parsed
prop_Invalid0 = isNothing . mkSignature $ "a"
prop_Invalid1 = isNothing . mkSignature $ "0"
prop_Invalid2 = isNothing . mkSignature $ "a{vy}"

-- Show is useful
prop_show0 x = showsPrec 10 x "" == "Signature \"" ++ strSignature x ++ "\""
prop_show1 x = showsPrec 11 x "" == "(Signature \"" ++ strSignature x ++ "\")"
