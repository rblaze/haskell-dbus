{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2010-2011 John Millikin <john@john-millikin.com>
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

module Main (benchmarks, main) where

import Criterion.Types
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)
import qualified Criterion.Main

import DBus

serial :: Word32 -> Serial
serial = unsafeCoerce -- FIXME: should the Serial constructor be exposed to
                      -- clients?

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
main = Criterion.Main.defaultMain benchmarks
