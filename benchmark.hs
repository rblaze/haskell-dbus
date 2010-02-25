-- Copyright (C) 2010 John Millikin <jmillikin@gmail.com>
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

import qualified DBus.Message as D
import qualified DBus.Wire as D
import qualified Data.Binary.Get as G
import Criterion.Types
import Criterion.Config

import qualified Criterion.Main as C
import qualified Progression.Main as P

config = defaultConfig { cfgPerformGC = ljust True }

methodCall :: D.MethodCall
methodCall = undefined

methodReturn :: D.MethodReturn
methodReturn = D.MethodReturn D.firstSerial Nothing []

benchMarshal :: D.Message msg => String -> msg -> Benchmark
benchMarshal name msg = bench name $ whnf marshal msg where
	marshal = D.marshalMessage D.LittleEndian D.firstSerial

benchUnmarshal :: D.Message msg => String -> msg -> Benchmark
benchUnmarshal name msg = bench name $ whnf unmarshal bytes where
	unmarshal = G.runGet (D.unmarshalMessage getBytes)
	getBytes = G.getLazyByteString . fromIntegral
	Right bytes = D.marshalMessage D.LittleEndian D.firstSerial msg

benchmarks :: Benchmark
benchmarks = bgroup "All"
	[ bgroup "Marshal"
		[ benchMarshal "MethodReturn" methodReturn
		]
	, bgroup "Unmarshal"
		[ benchUnmarshal "MethodReturn" methodReturn
		]
	]

main :: IO ()
--main = C.defaultMainWith config (return ()) [benchmarks]
main = P.defaultMain benchmarks
