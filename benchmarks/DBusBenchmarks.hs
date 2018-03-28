{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010-2011 John Millikin <john@john-millikin.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Main (benchmarks, main) where

import           Control.DeepSeq
import           Criterion.Types
import           Criterion.Config
import qualified Criterion.Main
import           Data.Word (Word32)
import           Unsafe.Coerce (unsafeCoerce)

import           DBus

config :: Config
config = defaultConfig { cfgPerformGC = ljust True }

serial :: Word32 -> Serial
serial = unsafeCoerce -- FIXME: should the Serial constructor be exposed to
                      -- clients?

instance NFData Type

instance NFData Signature where
	rnf = rnf . signatureTypes

instance NFData ObjectPath

instance NFData InterfaceName

instance NFData MemberName

instance NFData ErrorName

instance NFData BusName

empty_MethodCall :: MethodCall
empty_MethodCall = methodCall "/" "org.i" "m"

empty_MethodReturn :: MethodReturn
empty_MethodReturn = methodReturn (serial 0)

benchMarshal :: Message msg => String -> msg -> Benchmark
benchMarshal name msg = bench name (whnf (marshal LittleEndian (serial 0)) msg)

benchUnmarshal :: Message msg => String -> msg -> Benchmark
benchUnmarshal name msg = bench name (whnf unmarshal bytes) where
	Right bytes = marshal LittleEndian (serial 0) msg

benchmarks :: [Benchmark]
benchmarks = 
	[  bgroup "Types"
		[ bgroup "Signature"
			[ bench "parseSignature/small" (nf parseSignature "y")
			, bench "parseSignature/medium" (nf parseSignature "yyyyuua(yv)")
			, bench "parseSignature/large" (nf parseSignature "a{s(asiiiiasa(siiia{s(iiiiv)}))}")
			]
		, bgroup "ObjectPath"
			[ bench "objectPath_/small" (nf objectPath_ "/")
			, bench "objectPath_/medium" (nf objectPath_ "/foo/bar")
			, bench "objectPath_/large" (nf objectPath_ "/f0OO/b4R/baz_qux/blahblahblah")
			]
		, bgroup "InterfaceName"
			[ bench "interfaceName_/small" (nf interfaceName_ "f.b")
			, bench "interfaceName_/medium" (nf interfaceName_ "foo.bar.baz")
			, bench "interfaceName_/large" (nf interfaceName_ "f0OOO.b4R.baz_qux.blahblahblah")
			]
		, bgroup "MemberName"
			[ bench "memberName_/small" (nf memberName_ "f")
			, bench "memberName_/medium" (nf memberName_ "FooBar")
			, bench "memberName_/large" (nf memberName_ "f0OOOb4RBazQuxBlahBlahBlah")
			]
		, bgroup "ErrorName"
			[ bench "errorName_/small" (nf errorName_ "f.b")
			, bench "errorName_/medium" (nf errorName_ "foo.bar.baz")
			, bench "errorName_/large" (nf errorName_ "f0OOO.b4R.baz_qux.blahblahblah")
			]
		, bgroup "BusName"
			[ bench "busName_/small" (nf busName_ "f.b")
			, bench "busName_/medium" (nf busName_ "foo.bar.baz")
			, bench "busName_/large" (nf busName_ "f0OOO.b4R.baz-qux.blahblahblah")
			]
		]
	,  bgroup "Marshal"
		[ benchMarshal "MethodCall/empty" empty_MethodCall
		, benchMarshal "MethodReturn/empty" empty_MethodReturn
		]
	, bgroup "Unmarshal"
		[ benchUnmarshal "MethodCall/empty" empty_MethodCall
		, benchUnmarshal "MethodReturn/empty" empty_MethodReturn
		]
	]

main :: IO ()
main = Criterion.Main.defaultMainWith config (return ()) benchmarks
