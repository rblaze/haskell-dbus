{-# LANGUAGE TemplateHaskell #-}
module DBus.TH where

import DBus.Client
import DBus.Generation

generateSignalsFromInterface defaultGenerationParams $
                             buildIntrospectionInterface $
                             buildPropertiesInterface undefined
