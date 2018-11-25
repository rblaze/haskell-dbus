{-# LANGUAGE OverloadedStrings #-}

module DBus.Introspection.Render
    ( formatXML
    ) where

import Control.Monad
import Data.List (isPrefixOf)

import DBus.Internal.Types
import DBus.Introspection.Types

{-
formatXML :: Object -> Maybe String
formatXML obj = fmap TL.unpack $ runST $ runMaybeT $ runConduit $ renderObject obj .| R.renderText R.def .| sinkLazy

renderObject _ = R.tag "node" mempty (pure ())
-}

newtype XmlWriter a = XmlWriter { runXmlWriter :: Maybe (a, String) }

instance Functor XmlWriter where
    fmap = liftM

instance Applicative XmlWriter where
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
    writeObject (formatObjectPath path) obj

writeChild :: ObjectPath -> Object -> XmlWriter ()
writeChild parentPath obj@(Object path _ _) = write where
    path' = formatObjectPath path
    parent' = formatObjectPath  parentPath
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
    [("name", formatInterfaceName name)] $ do
        mapM_ writeMethod methods
        mapM_ writeSignal signals
        mapM_ writeProperty properties

writeMethod :: Method -> XmlWriter ()
writeMethod (Method name args) = writeElement "method"
    [("name", formatMemberName name)] $
        mapM_ writeMethodArg args

writeSignal :: Signal -> XmlWriter ()
writeSignal (Signal name args) = writeElement "signal"
    [("name", formatMemberName name)] $
        mapM_ writeSignalArg args

formatType :: Type -> XmlWriter String
formatType t = do
    sig <- case signature [t] of
        Just x -> return x
        Nothing -> XmlWriter Nothing
    return (formatSignature sig)

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
