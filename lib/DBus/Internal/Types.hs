{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2009-2012 John Millikin <john@john-millikin.com>
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

module DBus.Internal.Types where

import           Control.DeepSeq
import           Control.Monad (liftM, when, (>=>))
import           Control.Monad.Catch
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import           Data.Coerce
import           Data.Int
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import qualified Data.Map
import           Data.Map (Map)
import qualified Data.String
import qualified Data.Text
import           Data.Text (Text)
import qualified Data.Text.Lazy
import           Data.Typeable (Typeable, Proxy(..))
import qualified Data.Vector
import           Data.Vector (Vector)
import           Data.Word
import           GHC.Generics
import qualified Language.Haskell.TH.Lift as THL
import           System.Posix.Types (Fd)
import           Text.ParserCombinators.Parsec ((<|>), oneOf)
import qualified Text.ParserCombinators.Parsec as Parsec

data Type
    = TypeBoolean
    | TypeWord8
    | TypeWord16
    | TypeWord32
    | TypeWord64
    | TypeInt16
    | TypeInt32
    | TypeInt64
    | TypeDouble
    | TypeUnixFd
    | TypeString
    | TypeSignature
    | TypeObjectPath
    | TypeVariant
    | TypeArray Type
    | TypeDictionary Type Type
    | TypeStructure [Type]
    deriving (Eq, Ord, Generic)

instance NFData Type

instance Show Type where
    showsPrec d = showString . showType (d > 10)

showType :: Bool -> Type -> String
showType paren t = case t of
    TypeBoolean -> "Bool"
    TypeWord8 -> "Word8"
    TypeWord16 -> "Word16"
    TypeWord32 -> "Word32"
    TypeWord64 -> "Word64"
    TypeInt16 -> "Int16"
    TypeInt32 -> "Int32"
    TypeInt64 -> "Int64"
    TypeDouble -> "Double"
    TypeUnixFd -> "UnixFd"
    TypeString -> "String"
    TypeSignature -> "Signature"
    TypeObjectPath -> "ObjectPath"
    TypeVariant -> "Variant"
    TypeArray t' -> concat ["[", show t', "]"]
    TypeDictionary kt vt -> showParen paren (
                            showString "Dict " .
                            shows kt .
                            showString " " .
                            showsPrec 11 vt) ""
    TypeStructure ts -> concat
        ["(", intercalate ", " (map show ts), ")"]

-- | A signature is a list of D-Bus types, obeying some basic rules of
-- validity.
--
-- The rules of signature validity are complex: see
-- <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-signatures>
-- for details.
newtype Signature = Signature [Type]
    deriving (Eq, Ord, NFData)

-- | Get the list of types in a signature. The inverse of 'signature'.
signatureTypes :: Signature -> [Type]
signatureTypes (Signature types) = types

instance Show Signature where
    showsPrec d sig = showParen (d > 10) $
        showString "Signature " .
        shows (formatSignature sig)

-- | Convert a signature into a signature string. The inverse of
-- 'parseSignature'.
formatSignature :: Signature -> String
formatSignature = concatMap typeCode . signatureTypes

typeCode :: Type -> String
typeCode TypeBoolean    = "b"
typeCode TypeWord8      = "y"
typeCode TypeWord16     = "q"
typeCode TypeWord32     = "u"
typeCode TypeWord64     = "t"
typeCode TypeInt16      = "n"
typeCode TypeInt32      = "i"
typeCode TypeInt64      = "x"
typeCode TypeDouble     = "d"
typeCode TypeUnixFd     = "h"
typeCode TypeString     = "s"
typeCode TypeSignature  = "g"
typeCode TypeObjectPath = "o"
typeCode TypeVariant    = "v"
typeCode (TypeArray t)  = 'a' : typeCode t
typeCode (TypeDictionary kt vt) = concat
    [ "a{", typeCode kt , typeCode vt, "}"]

typeCode (TypeStructure ts) = concat
    ["(", concatMap typeCode ts, ")"]

instance Data.String.IsString Signature where
    fromString = forceParse "signature" parseSignature

-- | Convert a list of types into a valid signature.
--
-- Throws if the given types are not a valid signature.
signature :: MonadThrow m => [Type] -> m Signature
signature = check where
    check ts = if sumLen ts > 255
        then throwM $ userError "invalid signature"
        else pure (Signature ts)
    sumLen :: [Type] -> Int
    sumLen = sum . map len

    len (TypeArray t) = 1 + len t
    len (TypeDictionary kt vt)
        | typeIsAtomic kt = 3 + len kt + len vt
        | otherwise = 256
    len (TypeStructure []) = 256
    len (TypeStructure ts) = 2 + sumLen ts
    len _ = 1

    typeIsAtomic TypeVariant = False
    typeIsAtomic TypeArray{} = False
    typeIsAtomic TypeDictionary{} = False
    typeIsAtomic TypeStructure{} = False
    typeIsAtomic _ = True

-- | Convert a list of types into a valid signature.
--
-- Throws an exception if the given types are not a valid signature.
signature_ :: [Type] -> Signature
signature_ ts = case signature ts of
    Just sig -> sig
    Nothing -> error ("invalid signature: " ++ show ts)

-- | Parse a signature string into a valid signature.
--
-- Throws if the given string is not a valid signature.
parseSignature :: MonadThrow m => String -> m Signature
parseSignature s = do
    when (length s > 255) $ throwM $ userError "string too long"
    when (any (\c -> ord c > 0x7F) s) $ throwM $ userError "invalid signature"
    parseSignatureBytes (BS8.pack s)

parseSignatureBytes :: MonadThrow m => BS.ByteString -> m Signature
parseSignatureBytes bytes =
    case BS.length bytes of
        0 -> pure (Signature [])
        1 -> parseSigFast bytes
        len | len <= 255 -> parseSigFull bytes
        _ -> throwM $ userError "string too long"

parseSigFast :: MonadThrow m => BS.ByteString -> m Signature
parseSigFast bytes =
    let byte = BS.head bytes
     in parseAtom (fromIntegral byte)
            (\t -> pure (Signature [t]))
            (case byte of
                0x76 -> pure (Signature [TypeVariant])
                _ -> throwM $ userError "invalid signature")

parseAtom :: Int -> (Type -> a) -> a -> a
parseAtom byte yes no = case byte of
    0x62 -> yes TypeBoolean
    0x6E -> yes TypeInt16
    0x69 -> yes TypeInt32
    0x78 -> yes TypeInt64
    0x79 -> yes TypeWord8
    0x71 -> yes TypeWord16
    0x75 -> yes TypeWord32
    0x74 -> yes TypeWord64
    0x64 -> yes TypeDouble
    0x68 -> yes TypeUnixFd
    0x73 -> yes TypeString
    0x67 -> yes TypeSignature
    0x6F -> yes TypeObjectPath
    _ -> no
{-# INLINE parseAtom #-}

data SigParseError = SigParseError
    deriving (Show, Typeable)

instance Exception SigParseError

peekWord8AsInt :: BS.ByteString -> Int -> Int
peekWord8AsInt str i = fromIntegral $ BS.index str i

parseSigFull :: MonadThrow m => BS.ByteString -> m Signature
parseSigFull bytes = Signature <$> mainLoop [] 0
  where
    len = BS.length bytes
    mainLoop acc ii | ii >= len = pure (reverse acc)
    mainLoop acc ii = do
        let c = peekWord8AsInt bytes ii
        let next t = mainLoop (t : acc) (ii + 1)
        parseAtom c next $ case c of
            0x76 -> next TypeVariant
            0x28 -> do -- '('
                (ii', t) <- structure (ii + 1)
                mainLoop (t : acc) ii'
            0x61 -> do -- 'a'
                (ii', t) <- array (ii + 1)
                mainLoop (t : acc) ii'
            _ -> throwM SigParseError

    structure = loop [] where
        loop _ ii | ii >= len = throwM SigParseError
        loop acc ii = do
            let c = peekWord8AsInt bytes ii
            let next t = loop (t : acc) (ii + 1)
            parseAtom c next $ case c of
                0x76 -> next TypeVariant
                0x28 -> do -- '('
                    (ii', t) <- structure (ii + 1)
                    loop (t : acc) ii'
                0x61 -> do -- 'a'
                    (ii', t) <- array (ii + 1)
                    loop (t : acc) ii'
                -- ')'
                0x29 -> case acc of
                    [] -> throwM SigParseError
                    _ -> pure (ii + 1, TypeStructure (reverse acc))
                _ -> throwM SigParseError

    array ii | ii >= len = throwM SigParseError
    array ii = do
        let c = peekWord8AsInt bytes ii
        let next t = pure (ii + 1, TypeArray t)
        parseAtom c next $ case c of
            0x76 -> next TypeVariant
            0x7B -> dict (ii + 1) -- '{'
            0x28 -> do -- '('
                (ii', t) <- structure (ii + 1)
                pure (ii', TypeArray t)
            0x61 -> do -- 'a'
                (ii', t) <- array (ii + 1)
                pure (ii', TypeArray t)
            _ -> throwM SigParseError

    dict ii | ii + 1 >= len = throwM SigParseError
    dict ii = do
        let c1 = peekWord8AsInt bytes ii
        let c2 = peekWord8AsInt bytes (ii + 1)

        let next t = pure (ii + 2, t)
        (ii', t2) <- parseAtom c2 next $ case c2 of
            0x76 -> next TypeVariant
            0x28 -> structure (ii + 2) -- '('
            0x61 -> array (ii + 2) -- 'a'
            _ -> throwM SigParseError

        if ii' >= len
            then throwM SigParseError
            else do
                let c3 = peekWord8AsInt bytes ii'
                if c3 == 0x7D
                    then do
                        t1 <- parseAtom c1 pure (throwM SigParseError)
                        pure (ii' + 1, TypeDictionary t1 t2)
                    else throwM SigParseError

extractFromVariant :: IsValue a => Variant -> Maybe a
extractFromVariant (Variant (ValueVariant v)) = extractFromVariant v
extractFromVariant v = fromVariant v

typeOf :: forall a. IsValue a => a -> Type
typeOf _ = typeOf_ (Proxy :: Proxy a)

class IsVariant a where
    toVariant :: a -> Variant
    fromVariant :: Variant -> Maybe a

-- | Value types can be used as items in containers, such as lists or
-- dictionaries.
--
-- Users may not provide new instances of 'IsValue' because this could allow
-- containers to be created with items of heterogenous types.
class IsVariant a => IsValue a where
    typeOf_ :: Proxy a -> Type
    toValue :: a -> Value
    fromValue :: Value -> Maybe a

-- | Atomic types can be used as keys to dictionaries.
--
-- Users may not provide new instances of 'IsAtom' because this could allow
-- dictionaries to be created with invalid keys.
class IsValue a => IsAtom a where
    toAtom :: a -> Atom
    fromAtom :: Atom -> Maybe a

-- | Variants may contain any other built-in D-Bus value. Besides
-- representing native @VARIANT@ values, they allow type-safe storage and
-- inspection of D-Bus collections.
newtype Variant = Variant Value
    deriving (Eq)

data Value
    = ValueAtom Atom
    | ValueVariant Variant
    | ValueBytes BS.ByteString
    | ValueVector Type (Vector Value)
    | ValueMap Type Type (Map Atom Value)
    | ValueStructure [Value]
    deriving (Show)

data Atom
    = AtomBool Bool
    | AtomWord8 Word8
    | AtomWord16 Word16
    | AtomWord32 Word32
    | AtomWord64 Word64
    | AtomInt16 Int16
    | AtomInt32 Int32
    | AtomInt64 Int64
    | AtomDouble Double
    | AtomUnixFd Fd
    | AtomText Text
    | AtomSignature Signature
    | AtomObjectPath ObjectPath
    deriving (Show, Eq, Ord)

instance Eq Value where
    (==) (ValueBytes x) y = case y of
        ValueBytes y' -> x == y'
        ValueVector TypeWord8 y' -> x == vectorToBytes y'
        _ -> False

    (==) (ValueVector TypeWord8 x) y = case y of
        ValueBytes y' -> vectorToBytes x == y'
        ValueVector TypeWord8 y' -> x == y'
        _ -> False

    (==) (ValueAtom x) (ValueAtom y) = x == y
    (==) (ValueVariant x) (ValueVariant y) = x == y
    (==) (ValueVector tx x) (ValueVector ty y) = tx == ty && x == y
    (==) (ValueMap ktx vtx x) (ValueMap kty vty y) = ktx == kty && vtx == vty && x == y
    (==) (ValueStructure x) (ValueStructure y) = x == y
    (==) _ _ = False

showAtom :: Bool -> Atom -> String
showAtom _ (AtomBool x) = show x
showAtom _ (AtomWord8 x) = show x
showAtom _ (AtomWord16 x) = show x
showAtom _ (AtomWord32 x) = show x
showAtom _ (AtomWord64 x) = show x
showAtom _ (AtomInt16 x) = show x
showAtom _ (AtomInt32 x) = show x
showAtom _ (AtomInt64 x) = show x
showAtom _ (AtomDouble x) = show x
showAtom p (AtomUnixFd x) = showParen p (showString "UnixFd " . shows x) ""
showAtom _ (AtomText x) = show x
showAtom p (AtomSignature x) = showsPrec (if p then 11 else 0) x ""
showAtom p (AtomObjectPath x) = showsPrec (if p then 11 else 0) x ""

showValue :: Bool -> Value -> String
showValue p (ValueAtom x) = showAtom p x
showValue p (ValueVariant x) = showsPrec (if p then 11 else 0) x ""
showValue _ (ValueBytes xs) = 'b' : show xs
showValue _ (ValueVector TypeWord8 xs) = 'b' : show (vectorToBytes xs)
showValue _ (ValueVector _ xs) = showThings "[" (showValue False) "]" (Data.Vector.toList xs)
showValue _ (ValueMap _ _ xs) = showThings "{" showPair "}" (Data.Map.toList xs) where
    showPair (k, v) = showAtom False k ++ ": " ++ showValue False v
showValue _ (ValueStructure xs) = showThings "(" (showValue False) ")" xs

showThings :: String -> (a -> String) -> String -> [a] -> String
showThings a s z xs = a ++ intercalate ", " (map s xs) ++ z

vectorToBytes :: Vector Value -> BS.ByteString
vectorToBytes = BS.pack
              . Data.Vector.toList
              . Data.Vector.map (\(ValueAtom (AtomWord8 x)) -> x)

instance Show Variant where
    showsPrec d (Variant x) = showParen (d > 10) $
        showString "Variant " .  showString (showValue True x)

-- | Every variant is strongly-typed; that is, the type of its contained
-- value is known at all times. This function retrieves that type, so that
-- the correct cast can be used to retrieve the value.
variantType :: Variant -> Type
variantType (Variant val) = valueType val

valueType :: Value -> Type
valueType (ValueAtom x) = atomType x
valueType (ValueVariant _) = TypeVariant
valueType (ValueVector t _) = TypeArray t
valueType (ValueBytes _) = TypeArray TypeWord8
valueType (ValueMap kt vt _) = TypeDictionary kt vt
valueType (ValueStructure vs) = TypeStructure (map valueType vs)

atomType :: Atom -> Type
atomType (AtomBool _) = TypeBoolean
atomType (AtomWord8 _) = TypeWord8
atomType (AtomWord16 _) = TypeWord16
atomType (AtomWord32 _) = TypeWord32
atomType (AtomWord64 _) = TypeWord64
atomType (AtomInt16 _) = TypeInt16
atomType (AtomInt32 _) = TypeInt32
atomType (AtomInt64 _) = TypeInt64
atomType (AtomDouble _) = TypeDouble
atomType (AtomUnixFd _) = TypeUnixFd
atomType (AtomText _) = TypeString
atomType (AtomSignature _) = TypeSignature
atomType (AtomObjectPath _) = TypeObjectPath

#define IS_ATOM(HsType, AtomCons, TypeCons) \
    instance IsAtom HsType where \
    { toAtom = AtomCons \
    ; fromAtom (AtomCons x) = Just x \
    ; fromAtom _ = Nothing \
    }; \
    instance IsValue HsType where \
    { typeOf_ _ = TypeCons \
    ; toValue = ValueAtom . toAtom \
    ; fromValue (ValueAtom x) = fromAtom x \
    ; fromValue _ = Nothing \
    }; \
    instance IsVariant HsType where \
    { toVariant = Variant . toValue \
    ; fromVariant (Variant val) = fromValue val \
    }

IS_ATOM(Bool,       AtomBool,       TypeBoolean)
IS_ATOM(Word8,      AtomWord8,      TypeWord8)
IS_ATOM(Word16,     AtomWord16,     TypeWord16)
IS_ATOM(Word32,     AtomWord32,     TypeWord32)
IS_ATOM(Word64,     AtomWord64,     TypeWord64)
IS_ATOM(Int16,      AtomInt16,      TypeInt16)
IS_ATOM(Int32,      AtomInt32,      TypeInt32)
IS_ATOM(Int64,      AtomInt64,      TypeInt64)
IS_ATOM(Double,     AtomDouble,     TypeDouble)
IS_ATOM(Fd,         AtomUnixFd,     TypeUnixFd)
IS_ATOM(Text,       AtomText,       TypeString)
IS_ATOM(Signature,  AtomSignature,  TypeSignature)
IS_ATOM(ObjectPath, AtomObjectPath, TypeObjectPath)

instance IsValue Variant where
    typeOf_ _ = TypeVariant
    toValue = ValueVariant
    fromValue (ValueVariant x) = Just x
    fromValue _ = Nothing

instance IsVariant Variant where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance IsAtom Data.Text.Lazy.Text where
    toAtom = toAtom . Data.Text.Lazy.toStrict
    fromAtom = fmap Data.Text.Lazy.fromStrict . fromAtom

instance IsValue Data.Text.Lazy.Text where
    typeOf_ _ = TypeString
    toValue = ValueAtom . toAtom
    fromValue (ValueAtom x) = fromAtom x
    fromValue _ = Nothing

instance IsVariant Data.Text.Lazy.Text where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance IsAtom String where
    toAtom = toAtom . Data.Text.pack
    fromAtom = fmap Data.Text.unpack . fromAtom

instance IsValue String where
    typeOf_ _ = TypeString
    toValue = ValueAtom . toAtom
    fromValue (ValueAtom x) = fromAtom x
    fromValue _ = Nothing

instance IsVariant String where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance IsValue a => IsValue (Vector a) where
    typeOf_ _ = TypeArray (typeOf_ (Proxy :: Proxy a))
    toValue v = ValueVector
        (typeOf_ (Proxy :: Proxy a))
        (Data.Vector.map toValue v)
    fromValue (ValueVector _ v) = Data.Vector.mapM fromValue v
    fromValue _ = Nothing

instance IsValue a => IsVariant (Vector a) where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance IsValue a => IsValue [a] where
    typeOf_ _ = TypeArray (typeOf_ (Proxy :: Proxy a))
    toValue = toValue . Data.Vector.fromList
    fromValue = fmap Data.Vector.toList . fromValue

instance IsValue a => IsVariant [a] where
    toVariant = toVariant . Data.Vector.fromList
    fromVariant = fmap Data.Vector.toList . fromVariant

instance IsValue BS.ByteString where
    typeOf_ _ = TypeArray TypeWord8
    toValue = ValueBytes
    fromValue (ValueBytes bs) = Just bs
    fromValue (ValueVector TypeWord8 v) = Just (vectorToBytes v)
    fromValue _ = Nothing

instance IsVariant BS.ByteString where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance IsValue BL.ByteString where
    typeOf_ _ = TypeArray TypeWord8
    toValue = toValue
            . BS.concat
            . BL.toChunks
    fromValue = fmap (\bs -> BL.fromChunks [bs])
              . fromValue

instance IsVariant BL.ByteString where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance (Ord k, IsAtom k, IsValue v) => IsValue (Map k v) where
    typeOf_ _ = TypeDictionary
        (typeOf_ (Proxy :: Proxy k))
        (typeOf_ (Proxy :: Proxy v))

    toValue m = ValueMap kt vt (bimap box m) where
        kt = typeOf_ (Proxy :: Proxy k)
        vt = typeOf_ (Proxy :: Proxy v)
        box k v = (toAtom k, toValue v)

    fromValue (ValueMap _ _ m) = bimapM unbox m where
        unbox k v = do
            k' <- fromAtom k
            v' <- fromValue v
            return (k', v')
    fromValue _ = Nothing

bimap :: Ord k' => (k -> v -> (k', v')) -> Map k v -> Map k' v'
bimap f = Data.Map.fromList . map (\(k, v) -> f k v) . Data.Map.toList

bimapM :: (Monad m, Ord k') => (k -> v -> m (k', v')) -> Map k v -> m (Map k' v')
bimapM f = liftM Data.Map.fromList . mapM (\(k, v) -> f k v) . Data.Map.toList

instance (Ord k, IsAtom k, IsValue v) => IsVariant (Map k v) where
    toVariant = Variant . toValue
    fromVariant (Variant val) = fromValue val

instance IsValue () where
  typeOf_ _ = TypeStructure []
  toValue _ = ValueStructure []
  fromValue (ValueStructure []) = return ()
  fromValue _ = Nothing

instance IsVariant () where
  toVariant () = Variant (ValueStructure [])
  fromVariant (Variant (ValueStructure [])) = Just ()
  fromVariant _ = Nothing

instance (IsValue a1, IsValue a2) => IsValue (a1, a2) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        ]
    toValue (a1, a2) = ValueStructure [toValue a1, toValue a2]
    fromValue (ValueStructure [a1, a2]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        return (a1', a2')
    fromValue _ = Nothing

instance (IsVariant a1, IsVariant a2) => IsVariant (a1, a2) where
    toVariant (a1, a2) = Variant (ValueStructure [varToVal a1, varToVal a2])
    fromVariant (Variant (ValueStructure [a1, a2])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        return (a1', a2')
    fromVariant _ = Nothing

varToVal :: IsVariant a => a -> Value
varToVal a = case toVariant a of
    Variant val -> val

-- | Object paths are special strings, used to identify a particular object
-- exported from a D-Bus application.
--
-- Object paths must begin with a slash, and consist of alphanumeric
-- characters separated by slashes.
--
-- See
-- <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-marshaling-object-path>
-- for details.
newtype ObjectPath = ObjectPath String
    deriving (Eq, Ord, Show, NFData)

pathElements :: ObjectPath -> [String]
pathElements = filter (not . null) . splitOn "/" . coerce

fromElements :: [String] -> ObjectPath
fromElements elems = objectPath_ $ '/':intercalate "/" elems

formatObjectPath :: ObjectPath -> String
formatObjectPath (ObjectPath s) = s

parseObjectPath :: MonadThrow m => String -> m ObjectPath
parseObjectPath s = do
    maybeParseString parserObjectPath s
    return (ObjectPath s)

objectPath_ :: String -> ObjectPath
objectPath_ = forceParse "object path" parseObjectPath

instance Data.String.IsString ObjectPath where
    fromString = objectPath_

parserObjectPath :: Parsec.Parser ()
parserObjectPath = root <|> object where
    root = Parsec.try $ do
        slash
        Parsec.eof

    object = do
        slash
        skipSepBy1 element slash
        Parsec.eof

    element = Parsec.skipMany1 (oneOf chars)

    slash = Parsec.char '/' >> return ()
    chars = concat [ ['a'..'z']
                   , ['A'..'Z']
                   , ['0'..'9']
                   , "_"]

-- | Interfaces are used to group a set of methods and signals within an
-- exported object. Interface names consist of alphanumeric characters
-- separated by periods.
--
-- See
-- <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names-interface>
-- for details.
newtype InterfaceName = InterfaceName String
    deriving (Eq, Ord, Show, NFData)

formatInterfaceName :: InterfaceName -> String
formatInterfaceName (InterfaceName s) = s

parseInterfaceName :: MonadThrow m => String -> m InterfaceName
parseInterfaceName s = do
    when (length s > 255) $ throwM $ userError "name too long"
    maybeParseString parserInterfaceName s
    return (InterfaceName s)

interfaceName_ :: String -> InterfaceName
interfaceName_ = forceParse "interface name" parseInterfaceName

instance Data.String.IsString InterfaceName where
    fromString = interfaceName_

instance IsVariant InterfaceName where
    toVariant = toVariant . formatInterfaceName
    fromVariant = fromVariant >=> parseInterfaceName

parserInterfaceName :: Parsec.Parser ()
parserInterfaceName = name >> Parsec.eof where
    alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    alphanum = alpha ++ ['0'..'9']
    element = do
        _ <- oneOf alpha
        Parsec.skipMany (oneOf alphanum)
    name = do
        element
        _ <- Parsec.char '.'
        skipSepBy1 element (Parsec.char '.')

-- | Member names are used to identify a single method or signal within an
-- interface. Method names consist of alphanumeric characters.
--
-- See
-- <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names-member>
-- for details.
newtype MemberName = MemberName String
    deriving (Eq, Ord, Show, NFData)

formatMemberName :: MemberName -> String
formatMemberName (MemberName s) = s

parseMemberName :: MonadThrow m => String -> m MemberName
parseMemberName s = do
    when (length s > 255) $ throwM $ userError "name too long"
    maybeParseString parserMemberName s
    return (MemberName s)

memberName_ :: String -> MemberName
memberName_ = forceParse "member name" parseMemberName

instance Data.String.IsString MemberName where
    fromString = memberName_

instance IsVariant MemberName where
    toVariant = toVariant . formatMemberName
    fromVariant = fromVariant >=> parseMemberName

parserMemberName :: Parsec.Parser ()
parserMemberName = name >> Parsec.eof where
    alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_"
    alphanum = alpha ++ ['0'..'9']
    name = do
        _ <- oneOf alpha
        Parsec.skipMany (oneOf alphanum)

-- | Error names are used to identify which type of error was returned from
-- a method call. Error names consist of alphanumeric characters
-- separated by periods.
--
-- See
-- <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names-error>
-- for details.
newtype ErrorName = ErrorName String
    deriving (Eq, Ord, Show, NFData)

formatErrorName :: ErrorName -> String
formatErrorName (ErrorName s) = s

parseErrorName :: MonadThrow m => String -> m ErrorName
parseErrorName s = do
    when (length s > 255) $ throwM $ userError "name too long"
    maybeParseString parserInterfaceName s
    return (ErrorName s)

errorName_ :: String -> ErrorName
errorName_ = forceParse "error name" parseErrorName

instance Data.String.IsString ErrorName where
    fromString = errorName_

instance IsVariant ErrorName where
    toVariant = toVariant . formatErrorName
    fromVariant = fromVariant >=> parseErrorName

-- | Bus names are used to identify particular clients on the message bus.
-- A bus name may be either /unique/ or /well-known/, where unique names
-- start with a colon. Bus names consist of alphanumeric characters separated
-- by periods.
--
-- See
-- <http://dbus.freedesktop.org/doc/dbus-specification.html#message-protocol-names-bus>
-- for details.
newtype BusName = BusName String
    deriving (Eq, Ord, Show, NFData)

formatBusName :: BusName -> String
formatBusName (BusName s) = s

parseBusName :: MonadThrow m => String -> m BusName
parseBusName s = do
    when (length s > 255) $ throwM $ userError "name too long"
    maybeParseString parserBusName s
    return (BusName s)

busName_ :: String -> BusName
busName_ = forceParse "bus name" parseBusName

instance Data.String.IsString BusName where
    fromString = busName_

instance IsVariant BusName where
    toVariant = toVariant . formatBusName
    fromVariant = fromVariant >=> parseBusName

parserBusName :: Parsec.Parser ()
parserBusName = name >> Parsec.eof where
    alpha = ['a'..'z'] ++ ['A'..'Z'] ++ "_-"
    alphanum = alpha ++ ['0'..'9']

    name = unique <|> wellKnown
    unique = do
        _ <- Parsec.char ':'
        elements alphanum

    wellKnown = elements alpha

    elements :: [Char] -> Parsec.Parser ()
    elements start = do
        element start
        Parsec.skipMany1 $ do
            _ <- Parsec.char '.'
            element start

    element :: [Char] -> Parsec.Parser ()
    element start = do
        _ <- oneOf start
        Parsec.skipMany (oneOf alphanum)

-- | A D-Bus Structure is a container type similar to Haskell tuples, storing
-- values of any type that is convertable to 'IsVariant'. A Structure may
-- contain up to 255 values.
--
-- Most users can use the 'IsVariant' instance for tuples to extract the
-- values of a structure. This type is for very large structures, which may
-- be awkward to work with as tuples.
newtype Structure = Structure [Value]
    deriving (Eq)

instance Show Structure where
    show (Structure xs) = showValue True (ValueStructure xs)

instance IsVariant Structure where
    toVariant (Structure xs) = Variant (ValueStructure xs)
    fromVariant (Variant (ValueStructure xs)) = Just (Structure xs)
    fromVariant _ = Nothing

structureItems :: Structure -> [Variant]
structureItems (Structure xs) = map Variant xs

-- | A D-Bus Array is a container type similar to Haskell lists, storing
-- zero or more values of a single D-Bus type.
--
-- Most users can use the 'IsVariant' instance for lists or vectors to extract
-- the values of an array. This type is for advanced use cases, where the user
-- wants to convert array values to Haskell types that are not instances of
-- 'IsValue'.
data Array
    = Array Type (Vector Value)
    | ArrayBytes BS.ByteString

instance Show Array where
    show (Array t xs) = showValue True (ValueVector t xs)
    show (ArrayBytes xs) = showValue True (ValueBytes xs)

instance Eq Array where
    x == y = norm x == norm y where
        norm (Array TypeWord8 xs) = Left (vectorToBytes xs)
        norm (Array t xs) = Right (t, xs)
        norm (ArrayBytes xs) = Left xs

instance IsVariant Array where
    toVariant (Array t xs) = Variant (ValueVector t xs)
    toVariant (ArrayBytes bs) = Variant (ValueBytes bs)
    fromVariant (Variant (ValueVector t xs)) = Just (Array t xs)
    fromVariant (Variant (ValueBytes bs)) = Just (ArrayBytes bs)
    fromVariant _ = Nothing

arrayItems :: Array -> [Variant]
arrayItems (Array _ xs) = map Variant (Data.Vector.toList xs)
arrayItems (ArrayBytes bs) = map toVariant (BS.unpack bs)

-- | A D-Bus Dictionary is a container type similar to Haskell maps, storing
-- zero or more associations between keys and values.
--
-- Most users can use the 'IsVariant' instance for maps to extract the values
-- of a dictionary. This type is for advanced use cases, where the user
-- wants to convert dictionary items to Haskell types that are not instances
-- of 'IsValue'.
data Dictionary = Dictionary Type Type (Map Atom Value)
    deriving (Eq)

instance Show Dictionary where
    show (Dictionary kt vt xs) = showValue True (ValueMap kt vt xs)

instance IsVariant Dictionary where
    toVariant (Dictionary kt vt xs) = Variant (ValueMap kt vt xs)
    fromVariant (Variant (ValueMap kt vt xs)) = Just (Dictionary kt vt xs)
    fromVariant _ = Nothing

dictionaryItems :: Dictionary -> [(Variant, Variant)]
dictionaryItems (Dictionary _ _ xs) = do
    (k, v) <- Data.Map.toList xs
    return (Variant (ValueAtom k), Variant v)

instance (IsValue a1, IsValue a2, IsValue a3) => IsValue (a1, a2, a3) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        ]
    toValue (a1, a2, a3) = ValueStructure [toValue a1, toValue a2, toValue a3]
    fromValue (ValueStructure [a1, a2, a3]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        return (a1', a2', a3')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4) => IsValue (a1, a2, a3, a4) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        ]
    toValue (a1, a2, a3, a4) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4]
    fromValue (ValueStructure [a1, a2, a3, a4]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        return (a1', a2', a3', a4')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5) => IsValue (a1, a2, a3, a4, a5) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        ]
    toValue (a1, a2, a3, a4, a5) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5]
    fromValue (ValueStructure [a1, a2, a3, a4, a5]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        return (a1', a2', a3', a4', a5')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6) => IsValue (a1, a2, a3, a4, a5, a6) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        ]
    toValue (a1, a2, a3, a4, a5, a6) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        return (a1', a2', a3', a4', a5', a6')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7) => IsValue (a1, a2, a3, a4, a5, a6, a7) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        return (a1', a2', a3', a4', a5', a6', a7')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        return (a1', a2', a3', a4', a5', a6', a7', a8')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9, IsValue a10) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        , typeOf_ (Proxy :: Proxy a10)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9, toValue a10]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        a10' <- fromValue a10
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9, IsValue a10, IsValue a11) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        , typeOf_ (Proxy :: Proxy a10)
        , typeOf_ (Proxy :: Proxy a11)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9, toValue a10, toValue a11]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        a10' <- fromValue a10
        a11' <- fromValue a11
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9, IsValue a10, IsValue a11, IsValue a12) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        , typeOf_ (Proxy :: Proxy a10)
        , typeOf_ (Proxy :: Proxy a11)
        , typeOf_ (Proxy :: Proxy a12)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9, toValue a10, toValue a11, toValue a12]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        a10' <- fromValue a10
        a11' <- fromValue a11
        a12' <- fromValue a12
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9, IsValue a10, IsValue a11, IsValue a12, IsValue a13) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        , typeOf_ (Proxy :: Proxy a10)
        , typeOf_ (Proxy :: Proxy a11)
        , typeOf_ (Proxy :: Proxy a12)
        , typeOf_ (Proxy :: Proxy a13)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9, toValue a10, toValue a11, toValue a12, toValue a13]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        a10' <- fromValue a10
        a11' <- fromValue a11
        a12' <- fromValue a12
        a13' <- fromValue a13
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9, IsValue a10, IsValue a11, IsValue a12, IsValue a13, IsValue a14) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        , typeOf_ (Proxy :: Proxy a10)
        , typeOf_ (Proxy :: Proxy a11)
        , typeOf_ (Proxy :: Proxy a12)
        , typeOf_ (Proxy :: Proxy a13)
        , typeOf_ (Proxy :: Proxy a14)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9, toValue a10, toValue a11, toValue a12, toValue a13, toValue a14]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        a10' <- fromValue a10
        a11' <- fromValue a11
        a12' <- fromValue a12
        a13' <- fromValue a13
        a14' <- fromValue a14
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13', a14')
    fromValue _ = Nothing

instance (IsValue a1, IsValue a2, IsValue a3, IsValue a4, IsValue a5, IsValue a6, IsValue a7, IsValue a8, IsValue a9, IsValue a10, IsValue a11, IsValue a12, IsValue a13, IsValue a14, IsValue a15) => IsValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    typeOf_ _ = TypeStructure
        [ typeOf_ (Proxy :: Proxy a1)
        , typeOf_ (Proxy :: Proxy a2)
        , typeOf_ (Proxy :: Proxy a3)
        , typeOf_ (Proxy :: Proxy a4)
        , typeOf_ (Proxy :: Proxy a5)
        , typeOf_ (Proxy :: Proxy a6)
        , typeOf_ (Proxy :: Proxy a7)
        , typeOf_ (Proxy :: Proxy a8)
        , typeOf_ (Proxy :: Proxy a9)
        , typeOf_ (Proxy :: Proxy a10)
        , typeOf_ (Proxy :: Proxy a11)
        , typeOf_ (Proxy :: Proxy a12)
        , typeOf_ (Proxy :: Proxy a13)
        , typeOf_ (Proxy :: Proxy a14)
        , typeOf_ (Proxy :: Proxy a15)
        ]
    toValue (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = ValueStructure [toValue a1, toValue a2, toValue a3, toValue a4, toValue a5, toValue a6, toValue a7, toValue a8, toValue a9, toValue a10, toValue a11, toValue a12, toValue a13, toValue a14, toValue a15]
    fromValue (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15]) = do
        a1' <- fromValue a1
        a2' <- fromValue a2
        a3' <- fromValue a3
        a4' <- fromValue a4
        a5' <- fromValue a5
        a6' <- fromValue a6
        a7' <- fromValue a7
        a8' <- fromValue a8
        a9' <- fromValue a9
        a10' <- fromValue a10
        a11' <- fromValue a11
        a12' <- fromValue a12
        a13' <- fromValue a13
        a14' <- fromValue a14
        a15' <- fromValue a15
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13', a14', a15')
    fromValue _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3) => IsVariant (a1, a2, a3) where
    toVariant (a1, a2, a3) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3])
    fromVariant (Variant (ValueStructure [a1, a2, a3])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        return (a1', a2', a3')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4) => IsVariant (a1, a2, a3, a4) where
    toVariant (a1, a2, a3, a4) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        return (a1', a2', a3', a4')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5) => IsVariant (a1, a2, a3, a4, a5) where
    toVariant (a1, a2, a3, a4, a5) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        return (a1', a2', a3', a4', a5')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6) => IsVariant (a1, a2, a3, a4, a5, a6) where
    toVariant (a1, a2, a3, a4, a5, a6) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        return (a1', a2', a3', a4', a5', a6')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7) => IsVariant (a1, a2, a3, a4, a5, a6, a7) where
    toVariant (a1, a2, a3, a4, a5, a6, a7) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        return (a1', a2', a3', a4', a5', a6', a7')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        return (a1', a2', a3', a4', a5', a6', a7', a8')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9, IsVariant a10) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9, varToVal a10])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        a10' <- (fromVariant . Variant) a10
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9, IsVariant a10, IsVariant a11) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9, varToVal a10, varToVal a11])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        a10' <- (fromVariant . Variant) a10
        a11' <- (fromVariant . Variant) a11
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9, IsVariant a10, IsVariant a11, IsVariant a12) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9, varToVal a10, varToVal a11, varToVal a12])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        a10' <- (fromVariant . Variant) a10
        a11' <- (fromVariant . Variant) a11
        a12' <- (fromVariant . Variant) a12
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9, IsVariant a10, IsVariant a11, IsVariant a12, IsVariant a13) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9, varToVal a10, varToVal a11, varToVal a12, varToVal a13])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        a10' <- (fromVariant . Variant) a10
        a11' <- (fromVariant . Variant) a11
        a12' <- (fromVariant . Variant) a12
        a13' <- (fromVariant . Variant) a13
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9, IsVariant a10, IsVariant a11, IsVariant a12, IsVariant a13, IsVariant a14) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9, varToVal a10, varToVal a11, varToVal a12, varToVal a13, varToVal a14])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        a10' <- (fromVariant . Variant) a10
        a11' <- (fromVariant . Variant) a11
        a12' <- (fromVariant . Variant) a12
        a13' <- (fromVariant . Variant) a13
        a14' <- (fromVariant . Variant) a14
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13', a14')
    fromVariant _ = Nothing

instance (IsVariant a1, IsVariant a2, IsVariant a3, IsVariant a4, IsVariant a5, IsVariant a6, IsVariant a7, IsVariant a8, IsVariant a9, IsVariant a10, IsVariant a11, IsVariant a12, IsVariant a13, IsVariant a14, IsVariant a15) => IsVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) where
    toVariant (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = Variant (ValueStructure [varToVal a1, varToVal a2, varToVal a3, varToVal a4, varToVal a5, varToVal a6, varToVal a7, varToVal a8, varToVal a9, varToVal a10, varToVal a11, varToVal a12, varToVal a13, varToVal a14, varToVal a15])
    fromVariant (Variant (ValueStructure [a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15])) = do
        a1' <- (fromVariant . Variant) a1
        a2' <- (fromVariant . Variant) a2
        a3' <- (fromVariant . Variant) a3
        a4' <- (fromVariant . Variant) a4
        a5' <- (fromVariant . Variant) a5
        a6' <- (fromVariant . Variant) a6
        a7' <- (fromVariant . Variant) a7
        a8' <- (fromVariant . Variant) a8
        a9' <- (fromVariant . Variant) a9
        a10' <- (fromVariant . Variant) a10
        a11' <- (fromVariant . Variant) a11
        a12' <- (fromVariant . Variant) a12
        a13' <- (fromVariant . Variant) a13
        a14' <- (fromVariant . Variant) a14
        a15' <- (fromVariant . Variant) a15
        return (a1', a2', a3', a4', a5', a6', a7', a8', a9', a10', a11', a12', a13', a14', a15')
    fromVariant _ = Nothing

-- | A value used to uniquely identify a particular message within a session.
-- Serials are 32-bit unsigned integers, and eventually wrap.
newtype Serial = Serial Word32
    deriving (Eq, Ord, Show)

instance IsVariant Serial where
    toVariant (Serial x) = toVariant x
    fromVariant = fmap Serial . fromVariant

serialValue :: Serial -> Word32
serialValue (Serial x) = x

-- | Get the first serial in the sequence.
firstSerial :: Serial
firstSerial = Serial 1

-- | Get the next serial in the sequence. This may wrap around to
-- 'firstSerial'.
nextSerial :: Serial -> Serial
nextSerial (Serial x) = Serial (if x + 1 == 0
    then 1 -- wrap to firstSerial
    else x + 1)

skipSepBy1 :: Parsec.Parser a -> Parsec.Parser b -> Parsec.Parser ()
skipSepBy1 p sep = do
    _ <- p
    Parsec.skipMany (sep >> p)

forceParse :: String -> (String -> Maybe a) -> String -> a
forceParse label parse str = case parse str of
    Just x -> x
    Nothing -> error ("Invalid " ++ label ++ ": " ++ show str)

maybeParseString :: MonadThrow m => Parsec.Parser a -> String -> m a
maybeParseString parser s = case Parsec.parse parser "" s of
    Left err -> throwM $ userError $ show err
    Right a -> pure a

THL.deriveLiftMany [''BusName, ''ObjectPath, ''InterfaceName, ''MemberName]
