{-# LANGUAGE TemplateHaskell #-}

module DBusTests.TH where

import DBus.Client
import DBus.Generation
import DBusTests.Generation

generateClient defaultGenerationParams testIntrospectionInterface
generateSignalsFromInterface defaultGenerationParams testIntrospectionInterface
