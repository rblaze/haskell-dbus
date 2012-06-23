{-# LANGUAGE OverloadedStrings #-}

-- Copyright (C) 2009-2012 John Millikin <jmillikin@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module DBus.Introspection
	(
	-- * XML conversion
	  parseXML
	, formatXML
	
	-- * Objects
	, Object
	, object
	, objectPath
	, objectInterfaces
	, objectChildren
	
	-- * Interfaces
	, Interface
	, interface
	, interfaceName
	, interfaceMethods
	, interfaceSignals
	, interfaceProperties
	
	-- * Methods
	, Method
	, method
	, methodName
	, methodArgs
	
	-- ** Method arguments
	, MethodArg
	, methodArg
	, methodArgName
	, methodArgType
	, methodArgDirection
	
	, Direction
	, directionIn
	, directionOut
	
	-- * Signals
	, Signal
	, signal
	, signalName
	, signalArgs
	
	-- ** Signal arguments
	, SignalArg
	, signalArg
	, signalArgName
	, signalArgType
	
	-- * Properties
	, Property
	, property
	, propertyName
	, propertyType
	, propertyRead
	, propertyWrite
	) where

import           Control.Monad ((>=>))
import           Control.Monad.ST (runST)
import           Data.List (isPrefixOf)
import qualified Data.STRef as ST
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.Encoding
import qualified Data.XML.Types as X
import qualified Text.XML.LibXML.SAX as SAX

import qualified DBus as T

data Object = Object
	{ objectPath :: T.ObjectPath
	, objectInterfaces :: [Interface]
	, objectChildren :: [Object]
	}
	deriving (Show, Eq)

object :: T.ObjectPath -> Object
object path = Object path [] []

data Interface = Interface
	{ interfaceName :: T.InterfaceName
	, interfaceMethods :: [Method]
	, interfaceSignals :: [Signal]
	, interfaceProperties :: [Property]
	}
	deriving (Show, Eq)

interface :: T.InterfaceName -> Interface
interface name = Interface name [] [] []

data Method = Method
	{ methodName :: T.MemberName
	, methodArgs :: [MethodArg]
	}
	deriving (Show, Eq)

method :: T.MemberName -> Method
method name = Method name []

data MethodArg = MethodArg
	{ methodArgName :: String
	, methodArgType :: T.Type
	, methodArgDirection :: Direction
	}
	deriving (Show, Eq)

methodArg :: String -> T.Type -> Direction -> MethodArg
methodArg = MethodArg

data Direction = In | Out
	deriving (Show, Eq)

directionIn :: Direction
directionIn = In

directionOut :: Direction
directionOut = Out

data Signal = Signal
	{ signalName :: T.MemberName
	, signalArgs :: [SignalArg]
	}
	deriving (Show, Eq)

signal :: T.MemberName -> Signal
signal name = Signal name []

data SignalArg = SignalArg
	{ signalArgName :: String
	, signalArgType :: T.Type
	}
	deriving (Show, Eq)

signalArg :: String -> T.Type -> SignalArg
signalArg = SignalArg

data Property = Property
	{ propertyName :: String
	, propertyType :: T.Type
	, propertyRead :: Bool
	, propertyWrite :: Bool
	}
	deriving (Show, Eq)

property :: String -> T.Type -> Property
property name t = Property name t False False

parseXML :: T.ObjectPath -> String -> Maybe Object
parseXML path xml = do
	root <- parseElement (Data.Text.pack xml)
	parseRoot path root

parseElement :: Text -> Maybe X.Element
parseElement xml = runST $ do
	stackRef <- ST.newSTRef [([], [])]
	let onError _ = do
		ST.writeSTRef stackRef []
		return False
	let onBegin _ attrs = do
		ST.modifySTRef stackRef ((attrs, []):)
		return True
	let onEnd name = do
		stack <- ST.readSTRef stackRef
		let (attrs, children'):stack' = stack
		let e = X.Element name attrs (map X.NodeElement (reverse children'))
		let (pAttrs, pChildren):stack'' = stack'
		let parent = (pAttrs, e:pChildren)
		ST.writeSTRef stackRef (parent:stack'')
		return True
	
	p <- SAX.newParserST Nothing
	SAX.setCallback p SAX.parsedBeginElement onBegin
	SAX.setCallback p SAX.parsedEndElement onEnd
	SAX.setCallback p SAX.reportError onError
	SAX.parseBytes p (Data.Text.Encoding.encodeUtf8 xml)
	SAX.parseComplete p
	stack <- ST.readSTRef stackRef
	return $ case stack of
		[] -> Nothing
		(_, children'):_ -> Just (head children')

parseRoot :: T.ObjectPath -> X.Element -> Maybe Object
parseRoot defaultPath e = do
	path <- case X.attributeText "name" e of
		Nothing -> Just defaultPath
		Just x  -> T.parseObjectPath (Data.Text.unpack x)
	parseObject path e

parseChild :: T.ObjectPath -> X.Element -> Maybe Object
parseChild parentPath e = do
	let parentPath' = case T.formatObjectPath parentPath of
		"/" -> "/"
		x   -> x ++ "/"
	pathSegment <- X.attributeText "name" e
	path <- T.parseObjectPath (parentPath' ++ Data.Text.unpack pathSegment)
	parseObject path e

parseObject :: T.ObjectPath -> X.Element -> Maybe Object
parseObject path e | X.elementName e == "node" = do
	interfaces <- children parseInterface (X.isNamed "interface") e
	children' <- children (parseChild path) (X.isNamed "node") e
	return (Object path interfaces children')
parseObject _ _ = Nothing

parseInterface :: X.Element -> Maybe Interface
parseInterface e = do
	name <- T.parseInterfaceName =<< attributeString "name" e
	methods <- children parseMethod (X.isNamed "method") e
	signals <- children parseSignal (X.isNamed "signal") e
	properties <- children parseProperty (X.isNamed "property") e
	return (Interface name methods signals properties)

parseMethod :: X.Element -> Maybe Method
parseMethod e = do
	name <- T.parseMemberName =<< attributeString "name" e
	args <- children parseMethodArg (isArg ["in", "out", ""]) e
	return (Method name args)

parseSignal :: X.Element -> Maybe Signal
parseSignal e = do
	name <- T.parseMemberName =<< attributeString "name" e
	args <- children parseSignalArg (isArg ["out", ""]) e
	return (Signal name args)

parseType :: X.Element -> Maybe T.Type
parseType e = do
	typeStr <- attributeString "type" e
	sig <- T.parseSignature typeStr
	case T.signatureTypes sig of
		[t] -> Just t
		_ -> Nothing

parseMethodArg :: X.Element -> Maybe MethodArg
parseMethodArg e = do
	t <- parseType e
	let dir = case getattr "direction" e of
		"out" -> Out
		_ -> In
	Just (MethodArg (getattr "name" e) t dir)

parseSignalArg :: X.Element -> Maybe SignalArg
parseSignalArg e = do
	t <- parseType e
	Just (SignalArg (getattr "name" e) t)

isArg :: [String] -> X.Element -> [X.Element]
isArg dirs = X.isNamed "arg" >=> checkDir where
	checkDir e = [e | getattr "direction" e `elem` dirs]

parseProperty :: X.Element -> Maybe Property
parseProperty e = do
	t <- parseType e
	(canRead, canWrite) <- case getattr "access" e of
		""          -> Just (False, False)
		"read"      -> Just (True, False)
		"write"     -> Just (False, True)
		"readwrite" -> Just (True, True)
		_           -> Nothing
	Just (Property (getattr "name" e) t canRead canWrite)

getattr :: X.Name -> X.Element -> String
getattr name e = maybe "" Data.Text.unpack (X.attributeText name e)

children :: Monad m => (X.Element -> m b) -> (X.Element -> [X.Element]) -> X.Element -> m [b]
children f p = mapM f . concatMap p . X.elementChildren

newtype XmlWriter a = XmlWriter { runXmlWriter :: Maybe (a, String) }

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
	[("name", T.formatMemberName name)] $ do
		mapM_ writeMethodArg args

writeSignal :: Signal -> XmlWriter ()
writeSignal (Signal name args) = writeElement "signal"
	[("name", T.formatMemberName name)] $ do
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
	writeEmptyElement "arg" $
		[ ("name", name)
		, ("type", typeStr)
		, ("direction", dirAttr)
		]

writeSignalArg :: SignalArg -> XmlWriter ()
writeSignalArg (SignalArg name t) = do
	typeStr <- formatType t
	writeEmptyElement "arg" $
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

attributeString :: X.Name -> X.Element -> Maybe String
attributeString name e = fmap Data.Text.unpack (X.attributeText name e)

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
