{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DBus.TH where

import           DBus.Client
import           DBus.Generation
import           System.FilePath


generateSignalsFromInterface defaultGenerationParams $
                             buildIntrospectionInterface $
                             buildPropertiesInterface undefined

generateFromFilePath defaultGenerationParams { genBusName = Just dbusName
                                             , genObjectPath = Just dbusPath
                                             } $ "idlxml" </> "dbus.xml"
