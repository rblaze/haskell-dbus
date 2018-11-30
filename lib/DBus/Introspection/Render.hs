{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module DBus.Introspection.Render
    ( formatXML
    ) where

import Conduit
import Control.Monad
import Control.Monad.ST
import Control.Monad.Catch.Pure
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML.Stream.Render as R

import DBus.Internal.Types
import DBus.Introspection.Types

instance PrimMonad m => PrimMonad (CatchT m) where
    type PrimState (CatchT m) = PrimState m
    primitive = lift . primitive

formatXML :: Object -> Maybe String
formatXML obj =
    let v = runST $ runCatchT $ runConduit $
            renderRoot obj .| R.renderText (R.def {R.rsPretty = True}) .| sinkLazy
     in case v of
            Left _ -> Nothing
            Right r -> Just $ TL.unpack r

renderRoot obj = renderObject (formatObjectPath $ objectPath obj) obj

renderObject path Object{..} = R.tag "node"
    (R.attr "name" (T.pack path)) $ do
    mapM_ renderInterface objectInterfaces
    mapM_ (renderChild objectPath) objectChildren

renderChild parentPath obj
    | not (parent' `isPrefixOf` path') =
        throwM $ userError "invalid child path"
    | parent' == "/" = renderObject (drop 1 path') obj
    | otherwise = renderObject (drop (length parent' + 1) path') obj
  where
    path' = formatObjectPath (objectPath obj)
    parent' = formatObjectPath parentPath

renderInterface Interface{..} = R.tag "interface"
    (R.attr "name" $ T.pack $ formatInterfaceName interfaceName) $ do
        mapM_ renderMethod interfaceMethods
        mapM_ renderSignal interfaceSignals
        mapM_ renderProperty interfaceProperties

renderMethod Method{..} = R.tag "method"
    (R.attr "name" $ T.pack $ formatMemberName methodName) $
        mapM_ renderMethodArg methodArgs

renderMethodArg MethodArg{..} = do
    typeStr <- formatType methodArgType
    let typeAttr = R.attr "type" $ T.pack typeStr
        nameAttr = R.attr "name" $ T.pack methodArgName
        dirAttr = R.attr "direction" $ case methodArgDirection of
            In -> "in"
            Out -> "out"
    R.tag "arg" (nameAttr <> typeAttr <> dirAttr) $ pure ()

renderSignal Signal{..} = R.tag "signal"
    (R.attr "name" $ T.pack $ formatMemberName signalName) $
        mapM_ renderSignalArg signalArgs

renderSignalArg SignalArg{..} = do
    typeStr <- formatType signalArgType
    let typeAttr = R.attr "type" $ T.pack typeStr
        nameAttr = R.attr "name" $ T.pack signalArgName
    R.tag "arg" (nameAttr <> typeAttr) $ pure ()

renderProperty Property{..} = do
    typeStr <- formatType propertyType
    let readStr = if propertyRead then "read" else ""
        writeStr = if propertyWrite then "write" else ""
        typeAttr = R.attr "type" $ T.pack typeStr
        nameAttr = R.attr "name" $ T.pack propertyName
        accessAttr = R.attr "access" $ T.pack (readStr ++ writeStr)
    R.tag "property" (nameAttr <> typeAttr <> accessAttr) $ pure ()

formatType t = formatSignature <$> signature [t]
