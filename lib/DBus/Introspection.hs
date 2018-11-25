{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2009-2012 John Millikin <john@john-millikin.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module DBus.Introspection
    (
    -- * XML conversion
      parseXML
    , formatXML
    , Object(..)
    , Interface(..)
    , Method(..)
    , MethodArg(..)
    , Direction(..)
    , Signal(..)
    , SignalArg(..)
    , Property(..)
    ) where

import           Conduit
import qualified Control.Applicative
import           Control.Monad (ap, liftM)
import           Data.List (isPrefixOf)
import           Data.Maybe
import qualified Data.Text as Text
import           Data.XML.Types
import qualified Text.XML.Stream.Parse as X

import qualified DBus as T

data Object = Object
    { objectPath :: T.ObjectPath
    , objectInterfaces :: [Interface]
    , objectChildren :: [Object]
    }
    deriving (Show, Eq)

data Interface = Interface
    { interfaceName :: T.InterfaceName
    , interfaceMethods :: [Method]
    , interfaceSignals :: [Signal]
    , interfaceProperties :: [Property]
    }
    deriving (Show, Eq)

data Method = Method
    { methodName :: T.MemberName
    , methodArgs :: [MethodArg]
    }
    deriving (Show, Eq)

data MethodArg = MethodArg
    { methodArgName :: String
    , methodArgType :: T.Type
    , methodArgDirection :: Direction
    }
    deriving (Show, Eq)

data Direction = In | Out
    deriving (Show, Eq)

data Signal = Signal
    { signalName :: T.MemberName
    , signalArgs :: [SignalArg]
    }
    deriving (Show, Eq)

data SignalArg = SignalArg
    { signalArgName :: String
    , signalArgType :: T.Type
    }
    deriving (Show, Eq)

data Property = Property
    { propertyName :: String
    , propertyType :: T.Type
    , propertyRead :: Bool
    , propertyWrite :: Bool
    }
    deriving (Show, Eq)

data ObjectChildren
    = InterfaceDefinition Interface
    | SubNode Object

data InterfaceChildren
    = MethodDefinition Method
    | SignalDefinition Signal
    | PropertyDefinition Property

parseXML :: T.ObjectPath -> Text.Text -> Maybe Object
parseXML path xml =
    runConduit $ yieldMany [xml] .| X.parseText' X.def .| X.force "parse error" (parseObject $ getRootName path)

getRootName :: T.ObjectPath -> X.AttrParser T.ObjectPath
getRootName defaultPath = do
    nodeName <- X.attr "name"
    pure $ maybe defaultPath (T.objectPath_ . Text.unpack) nodeName

getChildName :: T.ObjectPath -> X.AttrParser T.ObjectPath
getChildName parentPath = do
    nodeName <- X.requireAttr "name"
    let parentPath' = case T.formatObjectPath parentPath of
            "/" -> "/"
            x   -> x ++ "/"
    pure $ T.objectPath_ (parentPath' ++ Text.unpack nodeName)

parseObject
    :: X.AttrParser T.ObjectPath
    -> ConduitT Event o Maybe (Maybe Object)
parseObject getPath = X.tag' "node" getPath parseContent
  where
    parseContent objPath = do
        elems <- X.many $ X.choose
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
        pure $ T.interfaceName_ (Text.unpack ifName)
    parseContent ifName = do
        elems <- X.many $ X.choose
            [ parseMethod
            , parseSignal
            , parseProperty
            ]
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
        T.parseMemberName (Text.unpack ifName)
    parseArgs name = do
        args <- X.many $
            X.tag' "arg" getArg pure
        pure $ MethodDefinition $ Method name args
    getArg = do
        name <- fromMaybe "" <$> X.attr "name"
        typeStr <- X.requireAttr "type"
        dirStr <- fromMaybe "in" <$> X.attr "direction"
        X.ignoreAttrs
        typ <- parseType typeStr
        let dir = if dirStr == "in" then In else Out
        pure $ MethodArg (Text.unpack name) typ dir

parseSignal :: ConduitT Event o Maybe (Maybe InterfaceChildren)
parseSignal = X.tag' "signal" getName parseArgs
  where
    getName = do
        ifName <- X.requireAttr "name"
        T.parseMemberName (Text.unpack ifName)
    parseArgs name = do
        args <- X.many $
            X.tag' "arg" getArg pure
        pure $ SignalDefinition $ Signal name args
    getArg = do
        name <- fromMaybe "" <$> X.attr "name"
        typeStr <- X.requireAttr "type"
        X.ignoreAttrs
        typ <- parseType typeStr
        pure $ SignalArg (Text.unpack name) typ

parseProperty :: ConduitT Event o Maybe (Maybe InterfaceChildren)
parseProperty = X.tag' "property" getProp $ \p -> do
    X.many_ X.ignoreAnyTreeContent
    pure p
  where
    getProp = do
        name <- Text.unpack <$> X.requireAttr "name"
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

parseType :: MonadThrow m => Text.Text -> m T.Type
parseType typeStr = do
    typ <- T.parseSignature (Text.unpack typeStr)
    case T.signatureTypes typ of
        [t] -> pure t
        _ -> throwM $ userError "invalid type sig"

newtype XmlWriter a = XmlWriter { runXmlWriter :: Maybe (a, String) }

instance Functor XmlWriter where
    fmap = liftM

instance Control.Applicative.Applicative XmlWriter where
    pure = return
    (<*>) = ap

instance Monad XmlWriter where
    return a = XmlWriter $ Just (a, "")
    m >>= f = XmlWriter $ do
        (a, w) <- runXmlWriter m
        (b, w') <- runXmlWriter (f a)
        return (b, w ++ w')

tell :: String -> XmlWriter ()
tell s = XmlWriter (Just ((), s))

formatXML :: Object -> Maybe String
formatXML obj = do
    (_, xml) <- runXmlWriter (writeRoot obj)
    return xml

writeRoot :: Object -> XmlWriter ()
writeRoot obj@(Object path _ _) = do
    tell "<!DOCTYPE node PUBLIC '-//freedesktop//DTD D-BUS Object Introspection 1.0//EN'"
    tell " 'http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd'>\n"
    writeObject (T.formatObjectPath path) obj

writeChild :: T.ObjectPath -> Object -> XmlWriter ()
writeChild parentPath obj@(Object path _ _) = write where
    path' = T.formatObjectPath path
    parent' = T.formatObjectPath  parentPath
    relpathM = if parent' `isPrefixOf` path'
        then Just $ if parent' == "/"
            then drop 1 path'
            else drop (length parent' + 1) path'
        else Nothing

    write = case relpathM of
        Just relpath -> writeObject relpath obj
        Nothing -> XmlWriter Nothing

writeObject :: String -> Object -> XmlWriter ()
writeObject path (Object fullPath interfaces children') = writeElement "node"
    [("name", path)] $ do
        mapM_ writeInterface interfaces
        mapM_ (writeChild fullPath) children'

writeInterface :: Interface -> XmlWriter ()
writeInterface (Interface name methods signals properties) = writeElement "interface"
    [("name", T.formatInterfaceName name)] $ do
        mapM_ writeMethod methods
        mapM_ writeSignal signals
        mapM_ writeProperty properties

writeMethod :: Method -> XmlWriter ()
writeMethod (Method name args) = writeElement "method"
    [("name", T.formatMemberName name)] $
        mapM_ writeMethodArg args

writeSignal :: Signal -> XmlWriter ()
writeSignal (Signal name args) = writeElement "signal"
    [("name", T.formatMemberName name)] $
        mapM_ writeSignalArg args

formatType :: T.Type -> XmlWriter String
formatType t = do
    sig <- case T.signature [t] of
        Just x -> return x
        Nothing -> XmlWriter Nothing
    return (T.formatSignature sig)

writeMethodArg :: MethodArg -> XmlWriter ()
writeMethodArg (MethodArg name t dir) = do
    typeStr <- formatType t
    let dirAttr = case dir of
            In -> "in"
            Out -> "out"
    writeEmptyElement "arg"
        [ ("name", name)
        , ("type", typeStr)
        , ("direction", dirAttr)
        ]

writeSignalArg :: SignalArg -> XmlWriter ()
writeSignalArg (SignalArg name t) = do
    typeStr <- formatType t
    writeEmptyElement "arg"
        [ ("name", name)
        , ("type", typeStr)
        ]

writeProperty :: Property -> XmlWriter ()
writeProperty (Property name t canRead canWrite) = do
    typeStr <- formatType t
    let readS = if canRead then "read" else ""
    let writeS = if canWrite then "write" else ""
    writeEmptyElement "property"
        [ ("name", name)
        , ("type", typeStr)
        , ("access", readS ++ writeS)
        ]

--attributeString :: X.Name -> X.Element -> Maybe String
--attributeString name e = fmap Data.Text.unpack (X.attributeText name e)

writeElement :: String -> [(String, String)] -> XmlWriter () -> XmlWriter ()
writeElement name attrs content = do
    tell "<"
    tell name
    mapM_ writeAttribute attrs
    tell ">"
    content
    tell "</"
    tell name
    tell ">"

writeEmptyElement :: String -> [(String, String)] -> XmlWriter ()
writeEmptyElement name attrs = do
    tell "<"
    tell name
    mapM_ writeAttribute attrs
    tell "/>"

writeAttribute :: (String, String) -> XmlWriter ()
writeAttribute (name, content) = do
    tell " "
    tell name
    tell "='"
    tell (escape content)
    tell "'"

escape :: String -> String
escape = concatMap $ \c -> case c of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    '\'' -> "&apos;"
    _ -> [c]
