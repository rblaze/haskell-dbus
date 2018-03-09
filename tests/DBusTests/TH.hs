{-# LANGUAGE TemplateHaskell #-}

module DBusTests.TH where

import DBus.Client
import DBus.Generate
import DBusTests.Generation

generateClient defaultGenerationParams testIntrospectionInterface
generateSignalsFromInterface defaultGenerationParams testIntrospectionInterface
