{-# LANGUAGE TemplateHaskell #-}
module DBus.Generate where

import qualified DBus as D
import           DBus.Client as C
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import qualified Data.Char as Char
import           Data.Coerce
import           Data.Int
import           Data.List
import qualified Data.Map as Map
import           Data.String
import           Data.Word
import           Language.Haskell.TH
import           System.Posix.Types (Fd(..))

data GenerationParams = GenerationParams
  { genBusName :: Maybe T.BusName
  , genObjectPath :: Maybe T.ObjectPath
  , genInterfaceName :: String
  , getTHType :: T.Type -> Type
  }

defaultGetTHType :: T.Type -> Type
defaultGetTHType t =
  case t of
    T.TypeBoolean -> ConT ''Bool
    T.TypeWord8 -> ConT ''Word8
    T.TypeWord16 -> ConT ''Word16
    T.TypeWord32 -> ConT ''Word32
    T.TypeWord64 -> ConT ''Word64
    T.TypeInt16 -> ConT ''Int16
    T.TypeInt32 -> ConT ''Int32
    T.TypeInt64 -> ConT ''Int64
    T.TypeDouble -> ConT ''Double
    T.TypeUnixFd -> ConT ''Fd
    T.TypeString -> ConT ''String
    T.TypeSignature -> ConT ''T.Signature
    T.TypeObjectPath -> ConT ''T.ObjectPath
    T.TypeVariant -> ConT ''T.Variant
    T.TypeArray arrayType -> AppT ListT $ defaultGetTHType arrayType
    T.TypeDictionary k v -> (AppT (AppT (ConT ''Map.Map)
                                          (defaultGetTHType k))
                             (defaultGetTHType v))
    T.TypeStructure ts -> foldl AppT (TupleT $ length ts) $ map defaultGetTHType ts

defaultGenerationParams :: GenerationParams
defaultGenerationParams =
  GenerationParams
  { genBusName = Nothing
  , genInterfaceName = fromString ""
  , getTHType = defaultGetTHType
  }

addArg :: Type -> Type -> Type
addArg argT = AppT (AppT ArrowT argT)

mkFunD :: Name -> [Name] -> Exp -> Dec
mkFunD name argNames body =
  FunD name [Clause (map VarP argNames) (NormalB body) []]

generateClient :: GenerationParams -> I.Interface -> Q [Dec]
generateClient params interface@I.Interface{ I.interfaceName = name} = do
  let params' = params { genInterfaceName = coerce name }
  (fmap concat) <$> sequenceA $
                  (map (generateClientMethod params') $
                    I.interfaceMethods interface)
                  ++
                  (map (generateClientProperty params') $
                    I.interfaceProperties interface)

generateClientMethod :: GenerationParams -> I.Method -> Q [Dec]
generateClientMethod GenerationParams
                       { getTHType = getArgType
                       , genInterfaceName = methodInterface
                       }
                     I.Method
                       { I.methodArgs = args
                       , I.methodName = methodNameMN
                       } =
  do
    let (inputArgs, outputArgs) = partition ((== I.In) . I.methodArgDirection) args
        outputLength = length outputArgs
        buildArgNames = mapM (newName . I.methodArgName) inputArgs
        buildOutputNames = mapM (newName . I.methodArgName) outputArgs
        interfaceString :: String
        interfaceString = coerce methodInterface
    clientN <- newName "client"
    busN <- newName "busName"
    objectPathN <- newName "objectPath"
    variantsN <- newName "variants"
    methodCallN <- newName "methodCall"
    callResultN <- newName "callResult"
    replySuccessN <- newName "replySuccess"
    methodArgNames <- buildArgNames
    fromVariantOutputNames <- buildOutputNames
    finalOutputNames <- buildOutputNames
    let makeToVariantApp name = AppE (VarE 'T.toVariant) $ VarE name
        variantListExp = map makeToVariantApp methodArgNames
        methodString :: String
        methodString = coerce methodNameMN
        makeFromVariantApp name = AppE (VarE 'T.fromVariant) $ VarE name
        mapOrHead fn names cons =
          case outputLength of
            1 -> fn $ head fromVariantOutputNames
            _ -> cons $ map fn names
        fromVariantExp = mapOrHead makeFromVariantApp fromVariantOutputNames TupE
        finalResultTuple = mapOrHead VarE finalOutputNames TupE
        makeJustPattern name = ConP 'Just [VarP name]
        maybeExtractionPattern = mapOrHead makeJustPattern finalOutputNames TupP
        getFunctionBody = [|
             do
               let $( varP variantsN ) = $( return $ ListE variantListExp )
                   $( varP methodCallN ) =
                     (D.methodCall $( return $ VarE objectPathN )
                                   (fromString interfaceString)
                                   (fromString methodString)) { M.methodCallDestination = Just $( return $ VarE busN )
                                                              , M.methodCallBody = $( return $ VarE variantsN )
                                                              }
               $( varP callResultN ) <- call $( return $ VarE clientN ) $( varE methodCallN )
               return $ case $( varE callResultN ) of
                 Right $( varP replySuccessN ) ->
                   case (M.methodReturnBody $( varE replySuccessN )) of
                     $( return $ ListP $ map VarP fromVariantOutputNames) ->
                       case $( return $ fromVariantExp ) of
                         $( return maybeExtractionPattern ) -> Right $( return finalResultTuple )
                         _ -> Left C.errorInvalidParameters
                     _ -> Left C.errorInvalidParameters
                 Left _ -> Left C.errorInvalidParameters
               |]
    functionBody <- getFunctionBody
    let methodSignature = foldr addInArg fullOutputSignature inputArgs
        addInArg arg = addArg $ getArgType $ I.methodArgType arg
        fullOutputSignature = AppT (ConT ''IO) $
                              AppT (AppT (ConT ''Either)
                                         (ConT ''T.ErrorName))
                              outputSignature
        outputSignature =
          case outputLength of
            1 -> getArgType $ I.methodArgType $ head outputArgs
            _ -> foldl addOutArg (TupleT outputLength) outputArgs
        addOutArg target arg = AppT target $ getArgType $ I.methodArgType arg
        functionNameFirst:functionNameRest = methodString
        functionN = mkName $ (Char.toLower functionNameFirst):functionNameRest
        fullSignature = addArg (ConT ''C.Client) $
                        addArg (ConT ''T.BusName) $
                        addArg (ConT ''T.ObjectPath) methodSignature
        fullArgNames = clientN:busN:objectPathN:methodArgNames
        definitionDec = SigD functionN fullSignature
        function = FunD functionN [Clause (map VarP fullArgNames) (NormalB functionBody) []]
    return [definitionDec, function]

generateClientProperty :: GenerationParams -> I.Property -> Q [Dec]
generateClientProperty GenerationParams
                         { getTHType = getArgType
                         , genInterfaceName = propertyInterface
                         }
                       I.Property
                         { I.propertyName = name
                         , I.propertyType = propType
                         , I.propertyRead = readable
                         , I.propertyWrite = writable
                         } =
  do
    clientN <- newName "client"
    busN <- newName "busName"
    objectPathN <- newName "objectPath"
    methodCallN <- newName "methodCall"
    argN <- newName "arg"
    let interfaceString :: String
        interfaceString = coerce propertyInterface
        propertyString :: String
        propertyString = coerce name
        makeGetterBody = [|
          do
            let $( varP methodCallN ) =
                  (D.methodCall $( varE objectPathN )
                                (fromString interfaceString)
                                (fromString propertyString))
                     { M.methodCallDestination = Just $( return $ VarE busN ) }
            getPropertyValue $( return $ VarE clientN )
                             $( varE methodCallN )
          |]
        makeSetterBody = [|
          do
            let $( varP methodCallN ) =
                  (D.methodCall $( varE objectPathN )
                                (fromString interfaceString)
                                (fromString propertyString))
                     { M.methodCallDestination = Just $( return $ VarE busN ) }
            setPropertyValue $( varE clientN ) $( varE methodCallN ) $( varE argN )
          |]

    getterBody <- makeGetterBody
    setterBody <- makeSetterBody
    let getterSigType = addArg (ConT ''C.Client) $
                        addArg (ConT ''T.BusName) $
                        addArg (ConT ''T.ObjectPath) $
                               AppT (ConT ''IO) $
                               AppT (AppT (ConT ''Either)
                                          (ConT ''M.MethodError)) $ getArgType propType
        setterSigType = addArg (ConT ''C.Client) $
                        addArg (ConT ''T.BusName) $
                        addArg (ConT ''T.ObjectPath) $
                               AppT (ConT ''IO) $ AppT (ConT ''Maybe) (ConT ''M.MethodError)
        getterArgNames = [clientN, busN, objectPathN]
        setterArgNames = [clientN, busN, objectPathN, argN]
        getterName = mkName $ "get" ++ propertyString
        setterName = mkName $ "set" ++ propertyString
        getterFunction = mkFunD getterName getterArgNames getterBody
        setterFunction = mkFunD setterName setterArgNames setterBody
        getterSignature = SigD getterName getterSigType
        setterSignature = SigD setterName setterSigType
        getterDefs = if readable then [getterSignature, getterFunction] else []
        setterDefs = if writable then [setterSignature, setterFunction] else []
    return $ getterDefs ++ setterDefs