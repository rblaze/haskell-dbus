{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DBus.Generation where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           DBus.Client as C
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection.Parse as I
import qualified DBus.Introspection.Types as I
import qualified Data.ByteString as BS
import qualified Data.Char as Char
import           Data.Coerce
import           Data.Functor ((<$>))
import           Data.Int
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text.IO as Text
import           Data.Traversable
import           Data.Word
import           Language.Haskell.TH
import           Prelude hiding (mapM)
import           System.Posix.Types (Fd(..))

-- | Compatibility helper to create (total) tuple expressions
mkTupE :: [Exp] -> Exp
mkTupE = TupE
#if MIN_VERSION_template_haskell(2,16,0)
         . map Just
#endif

type ClientBusPathR a = ReaderT (Client, T.BusName, T.ObjectPath) IO a

dbusInvoke :: (Client -> T.BusName -> T.ObjectPath -> a) -> ClientBusPathR a
dbusInvoke fn = (\(c, b, p) -> fn c b p) <$> ask

-- Use these operators together with dbusInvoke to invoke functions of the form
-- Client -> T.BusName -> T.ObjectPath
infixl 4 ??
(??) :: Functor f => f (a -> b) -> a -> f b
fab ?? a = fmap ($ a) fab
{-# INLINE (??) #-}

infixl 4 ?/?
(?/?) :: ClientBusPathR (a -> IO b) -> a -> ClientBusPathR b
soFar ?/? arg = do
  returnValue <- fmap ($ arg) soFar
  lift returnValue

data GenerationParams = GenerationParams
  { genBusName :: Maybe T.BusName
  , genObjectPath :: Maybe T.ObjectPath
  , genInterfaceName :: T.InterfaceName
  , genTakeSignalErrorHandler :: Bool
  , getTHType :: T.Type -> Type
  }

defaultGetDictType :: Type -> Type -> Type
defaultGetDictType k =
  AppT (AppT (ConT ''Map.Map) k)

defaultGetTHType :: T.Type -> Type
defaultGetTHType = buildGetTHType (AppT ListT) defaultGetDictType

buildGetTHType ::
  (Type -> Type) -> (Type -> Type -> Type) -> T.Type -> Type
buildGetTHType arrayTypeBuilder dictTypeBuilder = fn
  where fn t =
          case t of
            -- Because of a quirk in how we unmarshal things, we currently HAVE
            -- to decorde arrays of Word8 in this way.
            T.TypeArray T.TypeWord8 -> ConT ''BS.ByteString
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
            T.TypeArray arrayType -> arrayTypeBuilder $ fn arrayType
            T.TypeDictionary k v -> dictTypeBuilder (fn k) (fn v)
            T.TypeStructure ts -> foldl AppT (TupleT $ length ts) $ map fn ts

newNameDef :: String -> Q Name
newNameDef n =
  case n of
    "" -> newName "arg"
    "data" -> newName "arg"
    _ -> newName n

defaultGenerationParams :: GenerationParams
defaultGenerationParams =
  GenerationParams
  { genBusName = Nothing
  , genInterfaceName = fromString ""
  , getTHType = defaultGetTHType
  , genObjectPath = Nothing
  , genTakeSignalErrorHandler = False
  }

addTypeArg :: Type -> Type -> Type
addTypeArg argT = AppT (AppT ArrowT argT)

addTypeArgIf :: Bool -> Type -> Type -> Type
addTypeArgIf condition theType = if condition then addTypeArg theType else id

unitIOType :: Type
unitIOType = AppT (ConT ''IO) (TupleT 0)

addArgIf :: Bool -> a -> [a] -> [a]
addArgIf condition name = if condition then (name:) else id

mkFunD :: Name -> [Name] -> Exp -> Dec
mkFunD name argNames body =
  FunD name [Clause (map VarP argNames) (NormalB body) []]

generateClient :: GenerationParams -> I.Interface -> Q [Dec]
generateClient params
               I.Interface{ I.interfaceName = name
                          , I.interfaceProperties = properties
                          , I.interfaceMethods = methods
                          } =
  let params' = params { genInterfaceName = coerce name } in
  fmap concat <$> sequenceA $
                  map (generateClientMethod params') methods
                  ++
                  map (generateClientProperty params') properties

maybeName :: a -> Bool -> Maybe a
maybeName name condition = if condition then Just name else Nothing

makeToVariantApp :: Name -> Exp
makeToVariantApp name = AppE (VarE 'T.toVariant) $ VarE name

makeFromVariantApp :: Name -> Exp
makeFromVariantApp name = AppE (VarE 'T.fromVariant) $ VarE name

makeJustPattern :: Name -> Pat
makeJustPattern name = ConP 'Just [] [VarP name]

mapOrHead ::
  (Num a, Eq a) => a -> (t -> b) -> [t] -> ([b] -> b) -> b
mapOrHead outputLength fn names cons =
  case outputLength of
    1 -> fn $ head names
    _ -> cons $ map fn names

runGetFirst :: [Maybe a] -> Maybe a
runGetFirst options = getFirst $  mconcat $ map First options

buildGeneratedSignature :: Bool -> Bool -> Type -> Type
buildGeneratedSignature takeBusArg takeObjectPathArg =
  addTypeArg (ConT ''C.Client) . addTypeArgIf takeBusArg (ConT ''T.BusName) .
  addTypeArgIf takeObjectPathArg (ConT ''T.ObjectPath)

getSetMethodCallParams ::
  Name -> Maybe Name -> Maybe Name -> ExpQ -> ExpQ
getSetMethodCallParams methodCallN mBusN mObjectPathN variantsE =
  case (mBusN, mObjectPathN) of
    (Just busN, Just objectPathN) -> [|
                       $( varE methodCallN )
                          { M.methodCallDestination = Just $( varE busN )
                          , M.methodCallPath = $( varE objectPathN )
                          , M.methodCallBody = $( variantsE )
                          }
                     |]
    (Just busN, Nothing) -> [|
                        $( varE methodCallN )
                          { M.methodCallDestination = Just $( varE busN )
                          , M.methodCallBody = $( variantsE )
                          }
                      |]
    (Nothing, Just objectPathN) -> [|
                        $( varE methodCallN )
                          { M.methodCallPath = $( varE objectPathN )
                          , M.methodCallBody = $( variantsE )
                          }
                      |]
    (Nothing, Nothing) -> [|
                         $( varE methodCallN ) { M.methodCallBody = $( variantsE ) }
                      |]

clientArgumentUnpackingMessage :: String
clientArgumentUnpackingMessage =
  "The client method could not unpack the message that was received."

clientArgumentUnpackingError :: [T.Variant] -> M.MethodError
clientArgumentUnpackingError variants =
  M.MethodError
  { M.methodErrorName = C.errorFailed
  , M.methodErrorSerial = T.Serial 0
  , M.methodErrorSender = Nothing
  , M.methodErrorDestination = Nothing
  , M.methodErrorBody = T.toVariant clientArgumentUnpackingMessage : variants
  }

generateClientMethod :: GenerationParams -> I.Method -> Q [Dec]
generateClientMethod GenerationParams
                       { getTHType = getArgType
                       , genInterfaceName = methodInterface
                       , genObjectPath = objectPathM
                       , genBusName = busNameM
                       }
                     I.Method
                       { I.methodArgs = args
                       , I.methodName = methodNameMN
                       } =
  do
    let (inputArgs, outputArgs) = partition ((== I.In) . I.methodArgDirection) args
        outputLength = length outputArgs
        buildArgNames = mapM (newNameDef . I.methodArgName) inputArgs
        buildOutputNames = mapM (newNameDef . I.methodArgName) outputArgs
        takeBusArg = isNothing busNameM
        takeObjectPathArg = isNothing objectPathM
        functionNameFirst:functionNameRest = coerce methodNameMN
        functionName = Char.toLower functionNameFirst:functionNameRest
        functionN = mkName $ Char.toLower functionNameFirst:functionNameRest
        methodCallDefN = mkName $ functionName ++ "MethodCall"
        defObjectPath = fromMaybe (fromString "/") objectPathM
    clientN <- newName "client"
    busN <- newName "busName"
    objectPathN <- newName "objectPath"
    methodCallN <- newName "methodCall"
    callResultN <- newName "callResult"
    replySuccessN <- newName "replySuccess"
    methodArgNames <- buildArgNames
    fromVariantOutputNames <- buildOutputNames
    finalOutputNames <- buildOutputNames
    let variantListExp = map makeToVariantApp methodArgNames
        mapOrHead' = mapOrHead outputLength
        fromVariantExp = mapOrHead' makeFromVariantApp fromVariantOutputNames mkTupE
        finalResultTuple = mapOrHead' VarE finalOutputNames mkTupE
        maybeExtractionPattern = mapOrHead' makeJustPattern finalOutputNames TupP
        getMethodCallDefDec = [d|
               $( varP methodCallDefN ) =
                 M.MethodCall { M.methodCallPath = defObjectPath
                              , M.methodCallInterface = Just methodInterface
                              , M.methodCallMember = methodNameMN
                              , M.methodCallDestination = busNameM
                              , M.methodCallSender = Nothing
                              , M.methodCallReplyExpected = True
                              , M.methodCallAutoStart = True
                              , M.methodCallBody = []
                              }
                 |]
        setMethodCallParamsE = getSetMethodCallParams methodCallDefN
                               (maybeName busN takeBusArg)
                               (maybeName objectPathN takeObjectPathArg)
                               (return $ ListE variantListExp)
        handleReplySuccess =
          if outputLength == 0
          then
            [| Right () |]
          else
            [|
               case M.methodReturnBody $( varE replySuccessN ) of
                     $( return $ ListP $ map VarP fromVariantOutputNames ) ->
                       case $( return fromVariantExp ) of
                         $( return maybeExtractionPattern ) -> Right $( return finalResultTuple )
                         _ -> Left $ clientArgumentUnpackingError $
                              M.methodReturnBody $( varE replySuccessN )
                     _ -> Left $ clientArgumentUnpackingError $
                          M.methodReturnBody $( varE replySuccessN )
             |]
        getFunctionBody = [|
             do
               let $( varP methodCallN ) = $( setMethodCallParamsE )
               $( varP callResultN ) <- call $( return $ VarE clientN ) $( varE methodCallN )
               return $ case $( varE callResultN ) of
                 Right $( return rightPattern  ) -> $( handleReplySuccess )
                 Left e -> Left e
               |]
                    where rightPattern = if outputLength == 0
                                         then WildP
                                         else VarP replySuccessN
    functionBody <- getFunctionBody
    methodCallDef <- getMethodCallDefDec
    let methodSignature = foldr addInArg fullOutputSignature inputArgs
        addInArg arg = addTypeArg $ getArgType $ I.methodArgType arg
        fullOutputSignature = AppT (ConT ''IO) $
                              AppT (AppT (ConT ''Either)
                                         (ConT ''M.MethodError))
                              outputSignature
        outputSignature =
          case outputLength of
            1 -> getArgType $ I.methodArgType $ head outputArgs
            _ -> foldl addOutArg (TupleT outputLength) outputArgs
        addOutArg target arg = AppT target $ getArgType $ I.methodArgType arg
        fullSignature = buildGeneratedSignature takeBusArg takeObjectPathArg methodSignature
        fullArgNames =
          clientN:addArgIf takeBusArg busN
                   (addArgIf takeObjectPathArg objectPathN methodArgNames)
        definitionDec = SigD functionN fullSignature
        function = mkFunD functionN fullArgNames functionBody
        methodCallSignature = SigD methodCallDefN $ ConT ''M.MethodCall
    return $ methodCallSignature:methodCallDef ++ [definitionDec, function]

generateClientProperty :: GenerationParams -> I.Property -> Q [Dec]
generateClientProperty GenerationParams
                         { getTHType = getArgType
                         , genInterfaceName = propertyInterface
                         , genObjectPath = objectPathM
                         , genBusName = busNameM
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
    let takeBusArg = isNothing busNameM
        takeObjectPathArg = isNothing objectPathM
        defObjectPath = fromMaybe (fromString "/") objectPathM
        methodCallDefN = mkName $ "methodCallFor" ++ name
        getMethodCallDefDec = [d|
               $( varP methodCallDefN ) =
                 M.MethodCall { M.methodCallPath = defObjectPath
                              , M.methodCallInterface = Just propertyInterface
                              , M.methodCallMember = fromString name
                              , M.methodCallDestination = busNameM
                              , M.methodCallSender = Nothing
                              , M.methodCallReplyExpected = True
                              , M.methodCallAutoStart = True
                              , M.methodCallBody = []
                              }
                 |]
        setMethodCallParamsE = getSetMethodCallParams methodCallDefN
                                   (maybeName busN takeBusArg)
                                   (maybeName objectPathN takeObjectPathArg)
                                   (return $ ListE [])
        makeGetterBody = [|
          do
            let $( varP methodCallN ) = $( setMethodCallParamsE )
            getPropertyValue $( return $ VarE clientN )
                             $( varE methodCallN )
          |]
        makeSetterBody = [|
          do
            let $( varP methodCallN ) = $( setMethodCallParamsE )
            setPropertyValue $( varE clientN ) $( varE methodCallN ) $( varE argN )
          |]
    methodCallDefs <- getMethodCallDefDec
    getterBody <- makeGetterBody
    setterBody <- makeSetterBody
    let buildSignature = buildGeneratedSignature takeBusArg takeObjectPathArg
        getterSigType =
          buildSignature $ AppT (ConT ''IO) $
                         AppT (AppT (ConT ''Either)
                                      (ConT ''M.MethodError)) $ getArgType propType
        setterSigType = buildSignature $ addTypeArg (getArgType propType) $
                        AppT (ConT ''IO) $ AppT (ConT ''Maybe) (ConT ''M.MethodError)
        buildArgs rest = clientN:addArgIf takeBusArg busN
                         (addArgIf takeObjectPathArg objectPathN rest)
        getterArgNames = buildArgs []
        setterArgNames = buildArgs [argN]
        propertyString = coerce name
        getterName = mkName $ "get" ++ propertyString
        setterName = mkName $ "set" ++ propertyString
        getterFunction = mkFunD getterName getterArgNames getterBody
        setterFunction = mkFunD setterName setterArgNames setterBody
        getterSignature = SigD getterName getterSigType
        setterSignature = SigD setterName setterSigType
        getterDefs = if readable then [getterSignature, getterFunction] else []
        setterDefs = if writable then [setterSignature, setterFunction] else []
        methodCallSignature = SigD methodCallDefN $ ConT ''M.MethodCall
    return $ methodCallSignature:methodCallDefs ++ getterDefs ++ setterDefs

generateSignalsFromInterface :: GenerationParams -> I.Interface -> Q [Dec]
generateSignalsFromInterface params
                             I.Interface{ I.interfaceName = name
                                        , I.interfaceSignals = signals
                                        } = generateSignals params name signals

generateSignals :: GenerationParams -> T.InterfaceName -> [I.Signal] -> Q [Dec]
generateSignals params name signals =
  fmap concat <$> sequenceA $
                map (generateSignal params { genInterfaceName = coerce name })
                    signals

generateSignal :: GenerationParams -> I.Signal -> Q [Dec]
generateSignal GenerationParams
                 { getTHType = getArgType
                 , genInterfaceName = signalInterface
                 , genObjectPath = objectPathM
                 , genBusName = busNameM
                 , genTakeSignalErrorHandler = takeErrorHandler
                 }
               I.Signal
                 { I.signalName = name
                 , I.signalArgs = args
                 } =
  do
    let buildArgNames = mapM (newNameDef . I.signalArgName) args

    argNames <- buildArgNames
    fromVariantOutputNames <- buildArgNames
    toHandlerOutputNames <- buildArgNames
    objectPathN <- newName "objectPath"
    variantsN <- newName "variants"
    signalN <- newName "signal"
    receivedSignalN <- newName "signal"
    clientN <- newName "client"
    handlerArgN <- newName "handlerArg"
    errorHandlerN <- newName "errorHandler"
    matchRuleN <- newName "matchRule"
    matchRuleArgN <- newName "matchRuleArg"

    let variantListExp = map makeToVariantApp argNames
        signalString = coerce name
        signalDefN = mkName $ "signalFor" ++ signalString
        takeObjectPathArg = isNothing objectPathM
        defObjectPath = fromMaybe (fromString "/") objectPathM
        argCount = length argNames
        getSignalDefDec = [d|
          $( varP signalDefN ) =
            M.Signal { M.signalPath = defObjectPath
                     , M.signalInterface = signalInterface
                     , M.signalMember = name
                     , M.signalDestination = Nothing
                     , M.signalSender = Nothing
                     , M.signalBody = []
                     }
                 |]
    let mapOrHead' = mapOrHead argCount
        fromVariantExp = mapOrHead' makeFromVariantApp fromVariantOutputNames mkTupE
        maybeExtractionPattern = mapOrHead' makeJustPattern toHandlerOutputNames TupP
        applyToName toApply n = AppE toApply $ VarE n
        finalApplication = foldl applyToName (VarE handlerArgN)
                           (receivedSignalN:toHandlerOutputNames)
        makeHandlerN = mkName $ "makeHandlerFor" ++ signalString
        makeHandlerCall =
          if takeErrorHandler
          then AppE base (VarE errorHandlerN)
          else base
            where base = AppE (VarE makeHandlerN) (VarE handlerArgN)
        getSetSignal  =
          if takeObjectPathArg
          then [|
                  $( varE signalDefN )
                     { M.signalPath = $( varE objectPathN )
                     , M.signalBody = $( varE variantsN )
                     }
                 |]
          else [| $( varE signalDefN )
                  { M.signalBody = $( varE variantsN ) }
                |]
        getEmitBody = [|
          let $( varP variantsN ) = $( return $ ListE variantListExp )
              $( varP signalN ) = $( getSetSignal )
          in
            emit $( varE clientN ) $( varE signalN )
          |]
        getErrorHandler =
          if takeErrorHandler then
            [| $( varE errorHandlerN  ) $( varE receivedSignalN )|]
          else [| return () |]
        getMakeHandlerBody =
          if argCount == 0
          then
            [| $( return finalApplication ) |]
          else
            [|
               case M.signalBody $( varE receivedSignalN ) of
                 $( return $ ListP $ map VarP fromVariantOutputNames ) ->
                   case $( return fromVariantExp ) of
                     $( return maybeExtractionPattern ) -> $( return finalApplication )
                     _ -> $( getErrorHandler )
                 _ -> $( getErrorHandler )
                   |]
        getRegisterBody = [|
          let $( varP matchRuleN ) = $( varE matchRuleArgN )
                                       { C.matchInterface = Just signalInterface
                                       , C.matchMember = Just name
                                       , C.matchSender =
                                         runGetFirst
                                         [ C.matchSender $( varE matchRuleArgN )
                                         , busNameM
                                         ]
                                       , C.matchPath =
                                         runGetFirst
                                         [ C.matchPath $( varE matchRuleArgN )
                                         , objectPathM
                                         ]
                                       }
          in
            C.addMatch $( varE clientN ) $( varE matchRuleN ) $ $( return makeHandlerCall )
            |]
    registerBody <- getRegisterBody
    makeHandlerBody <- getMakeHandlerBody
    signalDef <- getSignalDefDec
    emitBody <- getEmitBody
    let methodSignature = foldr addInArg unitIOType args
        addInArg arg = addTypeArg $ getArgType $ I.signalArgType arg
        fullArgNames = clientN:addArgIf takeObjectPathArg objectPathN argNames
        -- Never take bus arg because it is set automatically anyway
        fullSignature =
            buildGeneratedSignature False takeObjectPathArg methodSignature
        functionN = mkName $ "emit" ++ signalString
        emitSignature = SigD functionN fullSignature
        emitFunction = mkFunD functionN fullArgNames emitBody
        handlerType = addTypeArg (ConT ''M.Signal) methodSignature
        errorHandlerType = addTypeArg (ConT ''M.Signal) unitIOType
        registerN = mkName $ "registerFor" ++ signalString
        registerArgs = clientN:matchRuleArgN:handlerArgN:
                       addArgIf takeErrorHandler errorHandlerN []
        registerFunction = mkFunD registerN registerArgs registerBody
        registerType =
          addTypeArg (ConT ''C.Client) $
          addTypeArg (ConT ''C.MatchRule) $
          addTypeArg handlerType $
          addTypeArgIf takeErrorHandler (addTypeArg (ConT ''M.Signal) unitIOType) $
          AppT (ConT ''IO) (ConT ''C.SignalHandler)
        registerSignature = SigD registerN registerType
        makeHandlerArgs =
          handlerArgN:addArgIf takeErrorHandler errorHandlerN [receivedSignalN]
        makeHandlerFunction = mkFunD makeHandlerN makeHandlerArgs makeHandlerBody
        makeHandlerType = addTypeArg handlerType $
                          addTypeArgIf takeErrorHandler errorHandlerType $
                          addTypeArg (ConT ''M.Signal) unitIOType
        makeHandlerSignature = SigD makeHandlerN makeHandlerType
        signalSignature = SigD signalDefN (ConT ''M.Signal)
    return $ signalSignature:
           signalDef ++ [ emitSignature, emitFunction
                        , makeHandlerSignature, makeHandlerFunction
                        , registerSignature, registerFunction
                        ]

generateFromFilePath :: GenerationParams -> FilePath -> Q [Dec]
generateFromFilePath generationParams filepath = do
    xml <- runIO $ Text.readFile filepath
    let obj = head $ maybeToList $ I.parseXML "/" xml
        interface = head $ I.objectInterfaces obj
        signals = generateSignalsFromInterface generationParams interface
        client = generateClient generationParams interface
     in fmap (++) signals <*> client
