{-# LANGUAGE OverloadedStrings #-}

module DBusTests.Generation where

import           DBus.Client
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection.Types as I
import           Data.Int
import           Data.Map as M

sampleMethod1 :: String -> Int32 -> IO (M.Map String Int32)
sampleMethod1 a b = return $ M.insert a b M.empty

serviceArg :: I.SignalArg
serviceArg = I.SignalArg { I.signalArgName = "service"
                         , I.signalArgType = T.TypeString
                         }

testSignals :: [I.Signal]
testSignals = [ I.Signal { I.signalName = "StatusNotifierItemRegistered"
                         , I.signalArgs = [serviceArg]
                         }
              ]

testInterface :: Interface
testInterface =
  defaultInterface { interfaceMethods =
                       [autoMethod "SampleMethod1" sampleMethod1]
                   , interfaceProperties =
                     [autoProperty "SampleWriteProperty"
                                     (Just $ return (1 :: Int32))
                                     (Just $ const $ return ())
                     ]
                   , interfaceName = "org.TestInterface"
                   , interfaceSignals = testSignals
                   }

testIntrospectionInterface :: I.Interface
testIntrospectionInterface = buildIntrospectionInterface testInterface
