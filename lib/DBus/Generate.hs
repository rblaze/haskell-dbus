{-# LANGUAGE TemplateHaskell #-}
module DBus.Generate where

import           DBus
import           DBus.Client as C
import qualified DBus.Internal.Message as M
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import           Data.Coerce
import           Data.List
import           Data.String
import           Language.Haskell.TH

data GenerationParams = GenerationParams
  { genBusName :: Maybe BusName
  , genTypeToName :: T.Type -> Name
  }

defaultTypeToName t =
  case t of
    T.TypeString -> ''String
    T.TypeInt32 -> ''Int

defaultGenerationParams =
  GenerationParams
  { genBusName = Nothing
  , genTypeToName = defaultTypeToName
  }

generateMethodCall :: GenerationParams -> String -> I.Method -> Q [Dec]
generateMethodCall GenerationParams
                     { genTypeToName = typeToName
                     } interfaceNameString
                   I.Method
                     { I.methodArgs = args
                     , I.methodName = methodNameMN
                     } =
  do
    let (inputArgs, outputArgs) = partition ((== I.In) . I.methodArgDirection) args
        outputLength = length outputArgs
        buildArgNames = mapM (newName . I.methodArgName) inputArgs
        buildOutputNames = mapM (newName . I.methodArgName) outputArgs
    clientName <- newName "client"
    busName <- newName "busName"
    objectPath <- newName "objectPath"
    variants <- newName "variants"
    methodCallArg <- newName "methodCall"
    callResultName <- newName "callResult"
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
               let $( varP variants ) = $( return $ ListE variantListExp )
                   $( varP methodCallArg ) =
                     (methodCall $( return $ VarE objectPath )
                                   (fromString interfaceNameString)
                                   (fromString methodString))
                     { M.methodCallDestination = $( return $ VarE busName )
                     , M.methodCallBody = $( return $ VarE variants )
                     }
               $( return $ VarP callResultName ) <- call $( return $ VarE clientName )
               return $ case $( return $ VarE callResultName ) of
                          $( return $ ListP $ map VarP fromVariantOutputNames) ->
                            case $( return $ fromVariantExp ) of
                              $( return maybeExtractionPattern ) ->
                                $( return finalResultTuple )
                              _ -> ReplyError errorInvalidParameters []
                          _ -> ReplyError errorInvalidParameters []
           |]
    functionBody <- getFunctionBody
    let methodSignature = foldr addInArg fullOutputSignature inputArgs
        addArg argT = AppT (AppT ArrowT argT)
        addInArg arg = addArg $ getArgType $ I.methodArgType arg
        fullOutputSignature = AppT (ConT ''IO) $
                              AppT (AppT (ConT ''Either)
                                         (ConT ''M.MethodError))
                              outputSignature
        outputSignature =
          case outputLength of
            1 -> getArgType $ I.methodArgType $ head outputArgs
            _ -> foldl addOutArg (TupleT outputLength) outputArgs
        addOutArg target arg = AppT target $ getArgType $ I.methodArgType arg
        getArgType = ConT . typeToName
        functionName = (mkName methodString)
        fullSignature = addArg (ConT ''C.Client) $
                        addArg (ConT ''T.BusName) $
                        addArg (ConT ''T.ObjectPath) methodSignature
        fullArgNames = clientName:busName:objectPath:methodArgNames
        definitionDec = SigD functionName fullSignature
        function = FunD functionName [Clause (map VarP fullArgNames) (NormalB functionBody) []]
    return [definitionDec, function]
