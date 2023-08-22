{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module DBus.Introspection.Render
    ( formatXML
    ) where

import Conduit
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import Data.List (isPrefixOf)
import Data.XML.Types (Event)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Text.XML.Stream.Render as R

import DBus.Internal.Types
import DBus.Introspection.Types

newtype Render s a = Render { runRender :: MaybeT (ST s) a }

deriving instance Functor (Render s)
deriving instance Applicative (Render s)
deriving instance Monad (Render s)

instance MonadThrow (Render s) where
    throwM _ = Render $ MaybeT $ pure Nothing

instance PrimMonad (Render s) where
    type PrimState (Render s) = s
    primitive f = Render $ lift $ primitive f

formatXML :: Object -> Maybe String
formatXML obj = do
    xml <- runST $ runMaybeT $ runRender $ runConduit $
        renderRoot obj .| R.renderText (R.def {R.rsPretty = True}) .| sinkLazy
    pure $ TL.unpack xml

renderRoot :: MonadThrow m => Object -> ConduitT i Event m ()
renderRoot obj = renderObject (formatObjectPath $ objectPath obj) obj

renderObject :: MonadThrow m => String -> Object -> ConduitT i Event m ()
renderObject path Object{..} = R.tag "node"
    (R.attr "name" (T.pack path)) $ do
    mapM_ renderInterface objectInterfaces
    mapM_ (renderChild objectPath) objectChildren

renderChild :: MonadThrow m => ObjectPath -> Object -> ConduitT i Event m ()
renderChild parentPath obj
    | not (parent' `isPrefixOf` path') =
        throwM $ userError "invalid child path"
    | parent' == "/" = renderObject (drop 1 path') obj
    | otherwise = renderObject (drop (length parent' + 1) path') obj
  where
    path' = formatObjectPath (objectPath obj)
    parent' = formatObjectPath parentPath

renderInterface :: MonadThrow m => Interface -> ConduitT i Event m ()
renderInterface Interface{..} = R.tag "interface"
    (R.attr "name" $ T.pack $ formatInterfaceName interfaceName) $ do
        mapM_ renderMethod interfaceMethods
        mapM_ renderSignal interfaceSignals
        mapM_ renderProperty interfaceProperties

renderMethod :: MonadThrow m => Method -> ConduitT i Event m ()
renderMethod Method{..} = R.tag "method"
    (R.attr "name" $ T.pack $ formatMemberName methodName) $
        mapM_ renderMethodArg methodArgs

renderMethodArg :: MonadThrow m => MethodArg -> ConduitT i Event m ()
renderMethodArg MethodArg{..} = do
    typeStr <- formatType methodArgType
    let typeAttr = R.attr "type" $ T.pack typeStr
        nameAttr = R.attr "name" $ T.pack methodArgName
        dirAttr = R.attr "direction" $ case methodArgDirection of
            In -> "in"
            Out -> "out"
    R.tag "arg" (nameAttr <> typeAttr <> dirAttr) $ pure ()

renderSignal :: MonadThrow m => Signal -> ConduitT i Event m ()
renderSignal Signal{..} = R.tag "signal"
    (R.attr "name" $ T.pack $ formatMemberName signalName) $
        mapM_ renderSignalArg signalArgs

renderSignalArg :: MonadThrow m => SignalArg -> ConduitT i Event m ()
renderSignalArg SignalArg{..} = do
    typeStr <- formatType signalArgType
    let typeAttr = R.attr "type" $ T.pack typeStr
        nameAttr = R.attr "name" $ T.pack signalArgName
    R.tag "arg" (nameAttr <> typeAttr) $ pure ()

renderProperty :: MonadThrow m => Property -> ConduitT i Event m ()
renderProperty Property{..} = do
    typeStr <- formatType propertyType
    let readStr = if propertyRead then "read" else ""
        writeStr = if propertyWrite then "write" else ""
        typeAttr = R.attr "type" $ T.pack typeStr
        nameAttr = R.attr "name" $ T.pack propertyName
        accessAttr = R.attr "access" $ T.pack (readStr ++ writeStr)
    R.tag "property" (nameAttr <> typeAttr <> accessAttr) $ pure ()

formatType :: MonadThrow f => Type -> f String
formatType t = formatSignature <$> signature [t]
