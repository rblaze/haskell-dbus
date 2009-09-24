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

import Test.QuickCheck.Batch
import Tests.Address (addressProperties)
import Tests.Signature (signatureProperties)
import Tests.Containers (containerProperties)
import Tests.Marshal (marshalProperties)
import Tests.MiscTypes (miscTypeProperties)

options = TestOptions
	{ no_of_tests     = 100
	, length_of_tests = 1
	, debug_tests     = False
	}

main = do
	runTests "simple" options . map run . concat $
		[ addressProperties
		, signatureProperties
		, marshalProperties
		, miscTypeProperties
		, containerProperties
		]
