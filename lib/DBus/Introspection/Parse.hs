{-# LANGUAGE OverloadedStrings #-}

module DBus.Introspection.Parse
    ( parseXML
    ) where

import Conduit
import Data.Maybe
import Data.XML.Types
import qualified Data.Text as T
import qualified Text.XML.Stream.Parse as X

import DBus.Internal.Types
import DBus.Introspection.Types

data ObjectChildren
    = InterfaceDefinition Interface
    | SubNode Object

data InterfaceChildren
    = MethodDefinition Method
    | SignalDefinition Signal
    | PropertyDefinition Property

parseXML :: ObjectPath -> T.Text -> Maybe Object
parseXML path xml =
    runConduit $ yieldMany [xml] .| X.parseText X.def .| X.force "parse error" (parseObject $ getRootName path)

getRootName :: ObjectPath -> X.AttrParser ObjectPath
getRootName defaultPath = do
    nodeName <- X.attr "name"
    pure $ maybe defaultPath (objectPath_ . T.unpack) nodeName

getChildName :: ObjectPath -> X.AttrParser ObjectPath
getChildName parentPath = do
    nodeName <- X.requireAttr "name"
    let parentPath' = case formatObjectPath parentPath of
            "/" -> "/"
            x   -> x ++ "/"
    pure $ objectPath_ (parentPath' ++ T.unpack nodeName)

parseObject
    :: X.AttrParser ObjectPath
    -> ConduitT Event o Maybe (Maybe Object)
parseObject getPath = X.tag' "node" getPath parseContent
  where
    parseContent objPath = do
        elems <- X.many' $ X.choose
            [ fmap SubNode <$> parseObject (getChildName objPath)
            , fmap InterfaceDefinition <$> parseInterface
            ]
        let base = Object objPath [] []
            addElem e (Object p is cs) = case e of
                InterfaceDefinition i -> Object p (i:is) cs
                SubNode c -> Object p is (c:cs)
        pure $ foldr addElem base elems

parseInterface
    :: ConduitT Event o Maybe (Maybe Interface)
parseInterface = X.tag' "interface" getName parseContent
  where
    getName = do
        ifName <- X.requireAttr "name"
        pure $ interfaceName_ (T.unpack ifName)
    parseContent ifName = do
        elems <- X.many' $ do
            X.many_ $ X.ignoreTreeContent "annotation" X.ignoreAttrs
            X.choose
                [ parseMethod
                , parseSignal
                , parseProperty
                ]
        X.many_ $ X.ignoreTreeContent "annotation" X.ignoreAttrs
        let base = Interface ifName [] [] []
            addElem e (Interface n ms ss ps) = case e of
                MethodDefinition m -> Interface n (m:ms) ss ps
                SignalDefinition s -> Interface n ms (s:ss) ps
                PropertyDefinition p -> Interface n ms ss (p:ps)
        pure $ foldr addElem base elems

parseMethod :: ConduitT Event o Maybe (Maybe InterfaceChildren)
parseMethod = X.tag' "method" getName parseArgs
  where
    getName = do
        ifName <- X.requireAttr "name"
        parseMemberName (T.unpack ifName)
    parseArgs name = do
        args <- X.many' $ do
            X.many_ $ X.ignoreTreeContent "annotation" X.ignoreAttrs
            X.tag' "arg" getArg pure
        X.many_ $ X.ignoreTreeContent "annotation" X.ignoreAttrs
        pure $ MethodDefinition $ Method name args
    getArg = do
        name <- fromMaybe "" <$> X.attr "name"
        typeStr <- X.requireAttr "type"
        dirStr <- fromMaybe "in" <$> X.attr "direction"
        X.ignoreAttrs
        typ <- parseType typeStr
        let dir = if dirStr == "in" then In else Out
        pure $ MethodArg (T.unpack name) typ dir

parseSignal :: ConduitT Event o Maybe (Maybe InterfaceChildren)
parseSignal = X.tag' "signal" getName parseArgs
  where
    getName = do
        ifName <- X.requireAttr "name"
        parseMemberName (T.unpack ifName)
    parseArgs name = do
        args <- X.many' $ do
            X.many_ $ X.ignoreTreeContent "annotation" X.ignoreAttrs
            X.tag' "arg" getArg pure
        X.many_ $ X.ignoreTreeContent "annotation" X.ignoreAttrs
        pure $ SignalDefinition $ Signal name args
    getArg = do
        name <- fromMaybe "" <$> X.attr "name"
        typeStr <- X.requireAttr "type"
        X.ignoreAttrs
        typ <- parseType typeStr
        pure $ SignalArg (T.unpack name) typ

parseProperty :: ConduitT Event o Maybe (Maybe InterfaceChildren)
parseProperty = X.tag' "property" getProp $ \p -> do
    X.many_ X.ignoreAnyTreeContent
    pure p
  where
    getProp = do
        name <- T.unpack <$> X.requireAttr "name"
        typeStr <- X.requireAttr "type"
        accessStr <- fromMaybe "" <$> X.attr "access"
        X.ignoreAttrs
        typ <- parseType typeStr
        (canRead, canWrite) <- case accessStr of
            ""          -> pure (False, False)
            "read"      -> pure (True, False)
            "write"     -> pure (False, True)
            "readwrite" -> pure (True, True)
            _           -> throwM $ userError "invalid access value"

        pure $ PropertyDefinition $ Property name typ canRead canWrite

parseType :: MonadThrow m => T.Text -> m Type
parseType typeStr = do
    typ <- parseSignature (T.unpack typeStr)
    case signatureTypes typ of
        [t] -> pure t
        _ -> throwM $ userError "invalid type sig"
