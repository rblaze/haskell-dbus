{-# LANGUAGE OverloadedStrings #-}

module DBusTests.Generation where

import           DBus.Client
import           DBus.Generation
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import           Data.Int
import           Data.Map as M
import           Test.Tasty
import           Test.Tasty.HUnit

sampleMethod1 :: String -> Int32 -> IO (M.Map String Int32)
sampleMethod1 a b = return $ M.insert a b M.empty

serviceArg = I.SignalArg { I.signalArgName = "service"
                         , I.signalArgType = T.TypeString
                         }

testSignals = [ I.Signal { I.signalName = "StatusNotifierItemRegistered"
                         , I.signalArgs = [serviceArg]
                         }
              ]

testInterface =
  defaultInterface { interfaceMethods =
                       [autoMethod "SampleMethod1" sampleMethod1]
                   , interfaceName = "org.TestInterface"
                   , interfaceSignals = testSignals
                   }
testIntrospectionInterface = buildIntrospectionInterface testInterface
