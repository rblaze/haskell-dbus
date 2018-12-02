module DBus.Introspection.Types where

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
