{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- Copyright (C) 2009-2012 John Millikin <john@john-millikin.com>
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

-- | D-Bus clients are an abstraction over the lower-level messaging
-- system. When combined with an external daemon called the \"bus\", clients
-- can perform remote procedure calls to other clients on the bus.
--
-- Clients may also listen for or emit /signals/, which are asynchronous
-- broadcast notifications.
--
-- Example: connect to the session bus, and get a list of active names.
--
-- @
--{-\# LANGUAGE OverloadedStrings \#-}
--
--import Data.List (sort)
--import DBus
--import DBus.Client
--
--main = do
--    client <- 'connectSession'
--    //
--    \-- Request a list of connected clients from the bus
--    reply <- 'call_' client ('methodCall' \"\/org\/freedesktop\/DBus\" \"org.freedesktop.DBus\" \"ListNames\")
--        { 'methodCallDestination' = Just \"org.freedesktop.DBus\"
--        }
--    //
--    \-- org.freedesktop.DBus.ListNames() returns a single value, which is
--    \-- a list of names (here represented as [String])
--    let Just names = 'fromVariant' ('methodReturnBody' reply !! 0)
--    //
--    \-- Print each name on a line, sorted so reserved names are below
--    \-- temporary names.
--    mapM_ putStrLn (sort names)
-- @
--
module DBus.Client
    (
    -- * Clients
      Client

    -- * Connecting to a bus
    , connect
    , connectSystem
    , connectSession
    , connectStarter
    , disconnect

    -- * Sending method calls
    , call
    , call_
    , callNoReply

    -- * Receiving method calls
    , export
    , unexport
    , PathInfo(..)
    , pathLens
    , findPath
    , Interface(..)
    , pathInterfaces
    , pathChildren
    , Method(..)
    , makeMethod
    , Property(..)
    , Reply(..)
    , throwError

    -- ** Automatic method signatures
    , AutoMethod
    , autoMethod
    , autoProperty

    -- * Signals
    , SignalHandler
    , addMatch
    , removeMatch
    , emit
    , listen

    -- ** Match rules
    , MatchRule
    , formatMatchRule
    , matchAny
    , matchSender
    , matchDestination
    , matchPath
    , matchInterface
    , matchMember
    , matchPathNamespace

    -- * Name reservation
    , requestName
    , releaseName

    , RequestNameFlag
    , nameAllowReplacement
    , nameReplaceExisting
    , nameDoNotQueue

    , RequestNameReply(..)
    , ReleaseNameReply(..)

    -- * Client errors
    , ClientError
    , clientError
    , clientErrorMessage
    , clientErrorFatal

    -- * Advanced connection options
    , ClientOptions
    , clientSocketOptions
    , clientThreadRunner
    , defaultClientOptions
    , connectWith
    ) where

import Control.Concurrent

import Control.Arrow
import qualified Control.Exception
import Control.Exception (SomeException, throwIO)
import Control.Lens
import Control.Monad
import Data.Bits ((.|.))
import Data.Coerce
import Data.Function
import Data.IORef
import Data.List (intercalate, isPrefixOf, find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.String
import qualified Data.Traversable as T
import Data.Typeable (Typeable)
import Data.Unique
import Data.Word (Word32)
import Text.Printf

import DBus
import DBus.Internal.Message
import DBus.Internal.Types (pathElements, MemberName(..), Signature(..), ObjectPath(..))
import qualified DBus.Introspection as I
import qualified DBus.Socket
import DBus.Transport (TransportOpen, SocketTransport)

data ClientError = ClientError
    { clientErrorMessage :: String
    , clientErrorFatal :: Bool
    }
    deriving (Eq, Show, Typeable)

instance Control.Exception.Exception ClientError

clientError :: String -> ClientError
clientError msg = ClientError msg True

-- | An active client session to a message bus. Clients may send or receive
-- method calls, and listen for or emit signals.
data Client = Client
    { clientSocket :: DBus.Socket.Socket
    , clientPendingCalls :: IORef (Map Serial (MVar (Either MethodError MethodReturn)))
    , clientSignalHandlers :: IORef (Map Unique SignalHandler)
    , clientObjects :: IORef PathInfo
    , clientThreadID :: ThreadId
    }

data ClientOptions t = ClientOptions
    {
    -- | Options for the underlying socket, for advanced use cases. See
    -- the "DBus.Socket" module.
      clientSocketOptions :: DBus.Socket.SocketOptions t

    -- | A function to run the client thread. The provided IO computation
    -- should be called repeatedly; each time it is called, it will process
    -- one incoming message.
    --
    -- The provided computation will throw a 'ClientError' if it fails to
    -- process an incoming message, or if the connection is lost.
    --
    -- The default implementation is 'forever'.
    , clientThreadRunner :: IO () -> IO ()
    }

type FormattedMatchRule = String
data SignalHandler = SignalHandler Unique FormattedMatchRule (IORef Bool) (Signal -> IO ())

data Method = Method
  { methodName :: MemberName
  , inSignature :: Signature
  , outSignature :: Signature
  , methodHandler :: (MethodCall -> IO Reply)
  }

data Property = Property
  { propertyName :: MemberName
  , propertyType :: Type
  , propertyGetter :: (Maybe (IO Variant))
  , propertySetter :: (Maybe (Variant -> IO ()))
  }

data Reply
    = ReplyReturn [Variant]
    | ReplyError ErrorName [Variant]

data Interface = Interface
  { interfaceName :: InterfaceName
  , interfaceMethods :: [Method]
  , interfaceProperties :: [Property]
  , interfaceSignals :: [I.Signal]
  }

data PathInfo = PathInfo
  { _pathInterfaces :: [Interface]
  , _pathChildren :: Map String PathInfo
  }

-- XXX: This instance is needed to make modifyNothingHandler work, but it
-- shouldn't really be used for much else.
instance Eq PathInfo where
  a == b = null (_pathInterfaces a) &&
           null (_pathInterfaces b) &&
           M.null (_pathChildren a) &&
           M.null (_pathChildren b)

makeLenses ''PathInfo

emptyPathInfo :: PathInfo
emptyPathInfo = PathInfo
  { _pathInterfaces = []
  , _pathChildren = M.empty
  }

traverseElement
  :: Applicative f
  => (a -> Maybe PathInfo -> f (Maybe PathInfo))
  -> [Char]
  -> a
  -> PathInfo
  -> f PathInfo
traverseElement nothingHandler pathElement =
  pathChildren . (at pathElement) . nothingHandler

lookupNothingHandler
  :: (a -> Const (Data.Monoid.First PathInfo) b)
  -> Maybe a
  -> Const (Data.Monoid.First PathInfo) (Maybe b)
lookupNothingHandler = _Just

modifyNothingHandler ::
  (PathInfo -> Identity PathInfo)
    -> Maybe PathInfo
    -> Identity (Maybe PathInfo)
modifyNothingHandler = non emptyPathInfo

pathLens ::
  Applicative f =>
  ObjectPath
  -> ((PathInfo -> f PathInfo) -> Maybe PathInfo -> f (Maybe PathInfo))
  -> (PathInfo -> f PathInfo)
  -> PathInfo
  -> f PathInfo
pathLens path nothingHandler =
  foldl (\f pathElem -> f . (traverseElement nothingHandler pathElem)) id $
  pathElements path

findPath :: ObjectPath -> PathInfo -> Maybe PathInfo
findPath path = preview (pathLens path lookupNothingHandler)

modifyPathInfoLens
  :: ObjectPath
     -> (PathInfo -> Identity PathInfo) -> PathInfo -> Identity PathInfo
modifyPathInfoLens path = pathLens path modifyNothingHandler

modifyPathInterfacesLens
  :: ObjectPath
     -> ([Interface] -> Identity [Interface])
     -> PathInfo
     -> Identity PathInfo
modifyPathInterfacesLens path = (modifyPathInfoLens path . pathInterfaces)

addInterface :: ObjectPath -> Interface -> PathInfo -> PathInfo
addInterface path interface =
  over (modifyPathInterfacesLens path) (interface :)

-- | Connect to the bus specified in the environment variable
-- @DBUS_SYSTEM_BUS_ADDRESS@, or to
-- @unix:path=\/var\/run\/dbus\/system_bus_socket@ if @DBUS_SYSTEM_BUS_ADDRESS@
-- is not set.
--
-- Throws a 'ClientError' if @DBUS_SYSTEM_BUS_ADDRESS@ contains an invalid
-- address, or if connecting to the bus failed.
connectSystem :: IO Client
connectSystem = do
    env <- getSystemAddress
    case env of
        Nothing -> throwIO (clientError "connectSystem: DBUS_SYSTEM_BUS_ADDRESS is invalid.")
        Just addr -> connect addr

-- | Connect to the bus specified in the environment variable
-- @DBUS_SESSION_BUS_ADDRESS@, which must be set.
--
-- Throws a 'ClientError' if @DBUS_SESSION_BUS_ADDRESS@ is unset, contains an
-- invalid address, or if connecting to the bus failed.
connectSession :: IO Client
connectSession = do
    env <- getSessionAddress
    case env of
        Nothing -> throwIO (clientError "connectSession: DBUS_SESSION_BUS_ADDRESS is missing or invalid.")
        Just addr -> connect addr

-- | Connect to the bus specified in the environment variable
-- @DBUS_STARTER_ADDRESS@, which must be set.
--
-- Throws a 'ClientError' if @DBUS_STARTER_ADDRESS@ is unset, contains an
-- invalid address, or if connecting to the bus failed.
connectStarter :: IO Client
connectStarter = do
    env <- getStarterAddress
    case env of
        Nothing -> throwIO (clientError "connectStarter: DBUS_STARTER_ADDRESS is missing or invalid.")
        Just addr -> connect addr

-- | Connect to the bus at the specified address.
--
-- Throws a 'ClientError' on failure.
connect :: Address -> IO Client
connect = connectWith defaultClientOptions

-- | Connect to the bus at the specified address, with the given connection
-- options. Most users should use 'connect' instead.
--
-- Throws a 'ClientError' on failure.
connectWith :: TransportOpen t => ClientOptions t -> Address -> IO Client
connectWith opts addr = do
    sock <- DBus.Socket.openWith (clientSocketOptions opts) addr

    pendingCalls <- newIORef M.empty
    signalHandlers <- newIORef M.empty
    objects <- newIORef $ PathInfo [] M.empty

    let threadRunner = clientThreadRunner opts

    clientMVar <- newEmptyMVar
    threadID <- forkIO $ do
        client <- readMVar clientMVar
        threadRunner (mainLoop client)

    let client = Client
            { clientSocket = sock
            , clientPendingCalls = pendingCalls
            , clientSignalHandlers = signalHandlers
            , clientObjects = objects
            , clientThreadID = threadID
            }
    putMVar clientMVar client

    callNoReply client (methodCall dbusPath dbusInterface "Hello")
        { methodCallDestination = Just dbusName
        }

    return client

-- | Default client options. Uses the built-in Socket-based transport, which
-- supports the @tcp:@ and @unix:@ methods.
defaultClientOptions :: ClientOptions SocketTransport
defaultClientOptions = ClientOptions
    { clientSocketOptions = DBus.Socket.defaultSocketOptions
    , clientThreadRunner = forever
    }

-- | Stop a 'Client''s callback thread and close its underlying socket.
disconnect :: Client -> IO ()
disconnect client = do
    killThread (clientThreadID client)
    disconnect' client

disconnect' :: Client -> IO ()
disconnect' client = do
    pendingCalls <- atomicModifyIORef (clientPendingCalls client) (\p -> (M.empty, p))
    forM_ (M.toList pendingCalls) $ \(k, v) -> do
        putMVar v (Left (methodError k errorDisconnected))

    atomicWriteIORef (clientSignalHandlers client) M.empty
    -- XXX fix
    -- atomicWriteIORef (clientObjects client) M.empty

    DBus.Socket.close (clientSocket client)

mainLoop :: Client -> IO ()
mainLoop client = do
    let sock = clientSocket client

    received <- Control.Exception.try (DBus.Socket.receive sock)
    msg <- case received of
        Left err -> do
            disconnect' client
            throwIO (clientError (DBus.Socket.socketErrorMessage err))
        Right msg -> return msg

    dispatch client msg

dispatch :: Client -> ReceivedMessage -> IO ()
dispatch client = go where
    go (ReceivedMethodReturn _ msg) = dispatchReply (methodReturnSerial msg) (Right msg)
    go (ReceivedMethodError _ msg) = dispatchReply (methodErrorSerial msg) (Left msg)
    go (ReceivedSignal _ msg) = do
        handlers <- readIORef (clientSignalHandlers client)
        forM_ (M.toAscList handlers) (\(_, SignalHandler _ _ _ h) -> forkIO (h msg) >> return ())
    go (ReceivedMethodCall serial msg) = do
        pathInfo <- readIORef (clientObjects client)
        let sender = methodCallSender msg
            sendResult reply =
              case reply of
                ReplyReturn vs -> send_ client (methodReturn serial)
                                  { methodReturnDestination = sender
                                  , methodReturnBody = vs
                                  } (\_ -> return ())
                ReplyError name vs -> send_ client (methodError serial name)
                                      { methodErrorDestination = sender
                                      , methodErrorBody = vs
                                      } (\_ -> return ())
        _ <- forkIO $ case findMethodOrPropForCall pathInfo msg of
            Right action -> action >>= sendResult
            Left errName -> send_ client
                (methodError serial errName) { methodErrorDestination = sender }
                (\_ -> return ())
        return ()
    go _ = return ()

    dispatchReply serial result = do
        pending <- atomicModifyIORef
            (clientPendingCalls client)
            (\p -> case M.lookup serial p of
                Nothing -> (p, Nothing)
                Just mvar -> (M.delete serial p, Just mvar))
        case pending of
            Just mvar -> putMVar mvar result
            Nothing -> return ()

data RequestNameFlag
    = AllowReplacement
    | ReplaceExisting
    | DoNotQueue
    deriving (Eq, Show)

-- | Allow this client's reservation to be replaced, if another client
-- requests it with the 'nameReplaceExisting' flag.
--
-- If this client's reservation is replaced, this client will be added to the
-- wait queue unless the request also included the 'nameDoNotQueue' flag.
nameAllowReplacement :: RequestNameFlag
nameAllowReplacement = AllowReplacement

-- | If the name being requested is already reserved, attempt to replace it.
-- This only works if the current owner provided the 'nameAllowReplacement'
-- flag.
nameReplaceExisting :: RequestNameFlag
nameReplaceExisting = ReplaceExisting

-- | If the name is already in use, do not add this client to the queue, just
-- return an error.
nameDoNotQueue :: RequestNameFlag
nameDoNotQueue = DoNotQueue

data RequestNameReply
    -- | This client is now the primary owner of the requested name.
    = NamePrimaryOwner

    -- | The name was already reserved by another client, and replacement
    -- was either not attempted or not successful.
    | NameInQueue

    -- | The name was already reserved by another client, 'DoNotQueue'
    -- was set, and replacement was either not attempted or not
    -- successful.
    | NameExists

    -- | This client is already the primary owner of the requested name.
    | NameAlreadyOwner

    -- | Not exported; exists to generate a compiler warning if users
    -- case on the reply and forget to include a default case.
    | UnknownRequestNameReply Word32
    deriving (Eq, Show)

data ReleaseNameReply
    -- | This client has released the provided name.
    = NameReleased

    -- | The provided name is not assigned to any client on the bus.
    | NameNonExistent

    -- | The provided name is not assigned to this client.
    | NameNotOwner

    -- | Not exported; exists to generate a compiler warning if users
    -- case on the reply and forget to include a default case.
    | UnknownReleaseNameReply Word32
    deriving (Eq, Show)

encodeFlags :: [RequestNameFlag] -> Word32
encodeFlags = foldr (.|.) 0 . map flagValue where
    flagValue AllowReplacement = 0x1
    flagValue ReplaceExisting  = 0x2
    flagValue DoNotQueue       = 0x4

-- | Asks the message bus to assign the given name to this client. The bus
-- maintains a queue of possible owners, where the head of the queue is the
-- current (\"primary\") owner.
--
-- There are several uses for name reservation:
--
-- * Clients which export methods reserve a name so users and applications
--   can send them messages. For example, the GNOME Keyring reserves the name
--   @\"org.gnome.keyring\"@ on the user's session bus, and NetworkManager
--   reserves @\"org.freedesktop.NetworkManager\"@ on the system bus.
--
-- * When there are multiple implementations of a particular service, the
--   service standard will ususally include a generic bus name for the
--   service. This allows other clients to avoid depending on any particular
--   implementation's name. For example, both the GNOME Keyring and KDE
--   KWallet services request the @\"org.freedesktop.secrets\"@ name on the
--   user's session bus.
--
-- * A process with \"single instance\" behavior can use name assignment to
--   check whether the instance is already running, and invoke some method
--   on it (e.g. opening a new window).
--
-- Throws a 'ClientError' if the call failed.
requestName :: Client -> BusName -> [RequestNameFlag] -> IO RequestNameReply
requestName client name flags = do
    reply <- call_ client (methodCall dbusPath dbusInterface "RequestName")
        { methodCallDestination = Just dbusName
        , methodCallBody = [toVariant name, toVariant (encodeFlags flags)]
        }
    var <- case listToMaybe (methodReturnBody reply) of
        Just x -> return x
        Nothing -> throwIO (clientError "requestName: received empty response")
            { clientErrorFatal = False
            }
    code <- case fromVariant var of
        Just x -> return x
        Nothing -> throwIO (clientError ("requestName: received invalid response code " ++ showsPrec 11 var ""))
            { clientErrorFatal = False
            }
    return $ case code :: Word32 of
        1 -> NamePrimaryOwner
        2 -> NameInQueue
        3 -> NameExists
        4 -> NameAlreadyOwner
        _ -> UnknownRequestNameReply code

-- | Release a name that this client previously requested. See 'requestName'
-- for an explanation of name reservation.
--
-- Throws a 'ClientError' if the call failed.
releaseName :: Client -> BusName -> IO ReleaseNameReply
releaseName client name = do
    reply <- call_ client (methodCall dbusPath dbusInterface "ReleaseName")
        { methodCallDestination = Just dbusName
        , methodCallBody = [toVariant name]
        }
    var <- case listToMaybe (methodReturnBody reply) of
        Just x -> return x
        Nothing -> throwIO (clientError "releaseName: received empty response")
            { clientErrorFatal = False
            }
    code <- case fromVariant var of
        Just x -> return x
        Nothing -> throwIO (clientError ("releaseName: received invalid response code " ++ showsPrec 11 var ""))
            { clientErrorFatal = False
            }
    return $ case code :: Word32 of
        1 -> NameReleased
        2 -> NameNonExistent
        3 -> NameNotOwner
        _ -> UnknownReleaseNameReply code

send_ :: Message msg => Client -> msg -> (Serial -> IO a) -> IO a
send_ client msg io = do
    result <- Control.Exception.try (DBus.Socket.send (clientSocket client) msg io)
    case result of
        Right x -> return x
        Left err -> throwIO (clientError (DBus.Socket.socketErrorMessage err))
            { clientErrorFatal = DBus.Socket.socketErrorFatal err
            }

-- | Send a method call to the bus, and wait for the response.
--
-- Throws a 'ClientError' if the method call couldn't be sent, or if the reply
-- couldn't be parsed.
call :: Client -> MethodCall -> IO (Either MethodError MethodReturn)
call client msg = do
    -- If ReplyExpected is False, this function would block indefinitely
    -- if the remote side honors it.
    let safeMsg = msg
            { methodCallReplyExpected = True
            }
    mvar <- newEmptyMVar
    let ref = clientPendingCalls client
    serial <- send_ client safeMsg (\serial -> atomicModifyIORef ref (\p -> (M.insert serial mvar p, serial)))

    -- At this point, we wait for the reply to arrive. The user may cancel
    -- a pending call by sending this thread an exception via something
    -- like 'timeout'; in that case, we want to clean up the pending call.
    Control.Exception.onException
        (takeMVar mvar)
        (atomicModifyIORef_ ref (M.delete serial))

-- | Send a method call to the bus, and wait for the response.
--
-- Unsets the 'noReplyExpected' message flag before sending.
--
-- Throws a 'ClientError' if the method call couldn't sent, if the reply
-- couldn't be parsed, or if the reply was a 'MethodError'.
call_ :: Client -> MethodCall -> IO MethodReturn
call_ client msg = do
    result <- call client msg
    case result of
        Left err -> throwIO (clientError ("Call failed: " ++ methodErrorMessage err))
            { clientErrorFatal = methodErrorName err == errorDisconnected
            }
        Right ret -> return ret

-- | Send a method call to the bus, and do not wait for a response.
--
-- Sets the 'noReplyExpected' message flag before sending.
--
-- Throws a 'ClientError' if the method call couldn't be sent.
callNoReply :: Client -> MethodCall -> IO ()
callNoReply client msg = do
    -- Ensure that noReplyExpected is always set.
    let safeMsg = msg
            { methodCallReplyExpected = False
            }
    send_ client safeMsg (\_ -> return ())

-- | Request that the bus forward signals matching the given rule to this
-- client, and process them in a callback.
--
-- A received signal might be processed by more than one callback at a time.
-- Callbacks each run in their own thread.
--
-- The returned 'SignalHandler' can be passed to 'removeMatch'
-- to stop handling this signal.
--
-- Throws a 'ClientError' if the match rule couldn't be added to the bus.
addMatch :: Client -> MatchRule -> (Signal -> IO ()) -> IO SignalHandler
addMatch client rule io = do
    let formatted = case formatMatchRule rule of
            "" -> "type='signal'"
            x -> "type='signal'," ++ x

    handlerId <- newUnique
    registered <- newIORef True
    let handler = SignalHandler handlerId formatted registered (\msg -> when (checkMatchRule rule msg) (io msg))

    atomicModifyIORef (clientSignalHandlers client) (\hs -> (M.insert handlerId handler hs, ()))
    _ <- call_ client (methodCall dbusPath dbusInterface "AddMatch")
        { methodCallDestination = Just dbusName
        , methodCallBody = [toVariant formatted]
        }
    return handler

-- | Request that the bus stop forwarding signals for the given handler.
--
-- Throws a 'ClientError' if the match rule couldn't be removed from the bus.
removeMatch :: Client -> SignalHandler -> IO ()
removeMatch client (SignalHandler handlerId formatted registered _) = do
    shouldUnregister <- atomicModifyIORef registered (\wasRegistered -> (False, wasRegistered))
    when shouldUnregister $ do
        atomicModifyIORef (clientSignalHandlers client) (\hs -> (M.delete handlerId hs, ()))
        _ <- call_ client (methodCall dbusPath dbusInterface "RemoveMatch")
            { methodCallDestination = Just dbusName
            , methodCallBody = [toVariant formatted]
            }
        return ()

-- | Equivalent to 'addMatch', but does not return the added 'SignalHandler'.
listen :: Client -> MatchRule -> (Signal -> IO ()) -> IO ()
listen client rule io = addMatch client rule io >> return ()
{-# DEPRECATED listen "Prefer DBus.Client.addMatch in new code." #-}

-- | Emit the signal on the bus.
--
-- Throws a 'ClientError' if the signal message couldn't be sent.
emit :: Client -> Signal -> IO ()
emit client msg = send_ client msg (\_ -> return ())

-- | A match rule describes which signals a particular callback is interested
-- in. Use 'matchAny' to construct match rules.
--
-- Example: a match rule which matches signals sent by the root object.
--
-- @
--matchFromRoot :: MatchRule
--matchFromRoot = 'matchAny' { 'matchPath' = Just \"/\" }
-- @
data MatchRule = MatchRule
    {
    -- | If set, only receives signals sent from the given bus name.
    --
    -- The standard D-Bus implementation from <http://dbus.freedesktop.org/>
    -- almost always sets signal senders to the unique name of the sending
    -- client. If 'matchSender' is a requested name like
    -- @\"com.example.Foo\"@, it will not match any signals.
    --
    -- The exception is for signals sent by the bus itself, which always
    -- have a sender of @\"org.freedesktop.DBus\"@.
      matchSender :: Maybe BusName

    -- | If set, only receives signals sent to the given bus name.
    , matchDestination :: Maybe BusName

    -- | If set, only receives signals sent with the given path.
    , matchPath  :: Maybe ObjectPath

    -- | If set, only receives signals sent with the given interface name.
    , matchInterface :: Maybe InterfaceName

    -- | If set, only receives signals sent with the given member name.
    , matchMember :: Maybe MemberName

    -- | If set, only receives signals sent with the given path or any of
    -- its children.
    , matchPathNamespace :: Maybe ObjectPath
    }

instance Show MatchRule where
    showsPrec d rule = showParen (d > 10) (showString "MatchRule " . shows (formatMatchRule rule))

-- | Convert a match rule into the textual format accepted by the bus.
formatMatchRule :: MatchRule -> String
formatMatchRule rule = intercalate "," predicates where
    predicates = catMaybes
        [ f "sender" matchSender formatBusName
        , f "destination" matchDestination formatBusName
        , f "path" matchPath formatObjectPath
        , f "interface" matchInterface formatInterfaceName
        , f "member" matchMember formatMemberName
        , f "path_namespace" matchPathNamespace formatObjectPath
        ]

    f :: String -> (MatchRule -> Maybe a) -> (a -> String) -> Maybe String
    f key get text = do
        val <- fmap text (get rule)
        return (concat [key, "='", val, "'"])

-- | Match any signal.
matchAny :: MatchRule
matchAny = MatchRule Nothing Nothing Nothing Nothing Nothing Nothing

checkMatchRule :: MatchRule -> Signal -> Bool
checkMatchRule rule msg = and
    [ maybe True (\x -> signalSender msg == Just x) (matchSender rule)
    , maybe True (\x -> signalDestination msg == Just x) (matchDestination rule)
    , maybe True (== signalPath msg) (matchPath rule)
    , maybe True (== signalInterface msg) (matchInterface rule)
    , maybe True (== signalMember msg) (matchMember rule)
    , maybe True (`pathPrefix` signalPath msg) (matchPathNamespace rule)
    ] where
  pathPrefix = isPrefixOf `on` formatObjectPath

data MethodExc = MethodExc ErrorName [Variant]
    deriving (Show, Eq, Typeable)

instance Control.Exception.Exception MethodExc

-- | Normally, any exceptions raised while executing a method will be
-- given the generic @\"org.freedesktop.DBus.Error.Failed\"@ name.
-- 'throwError' allows the programmer to specify an error name, and provide
-- additional information to the remote application. You may use this instead
-- of 'Control.Exception.throwIO' to abort a method call.
throwError :: ErrorName
           -> String -- ^ Error message
           -> [Variant] -- ^ Additional items of the error body
           -> IO a
throwError name message extra = Control.Exception.throwIO (MethodExc name (toVariant message : extra))

-- | Define a method handler, which will accept method calls with the given
-- interface and member name.
--
-- Note that the input and output parameter signatures are used for
-- introspection, but are not checked when executing a method.
--
-- See 'autoMethod' for an easier way to export functions with simple
-- parameter and return types.
makeMethod
  :: MemberName
  -> Signature -- ^ Input parameter signature
  -> Signature -- ^ Output parameter signature
  -> (MethodCall -> IO Reply)
  -> Method
makeMethod name inSig outSig io = Method name inSig outSig
    (\msg -> Control.Exception.catch
        (Control.Exception.catch
            (io msg)
            (\(MethodExc name' vs') -> return (ReplyError name' vs')))
        (\exc -> return (ReplyError errorFailed
            [toVariant (show (exc :: SomeException))])))

-- | Export the given 'Interface' at the given 'ObjectPath'
--
-- Use 'autoMethod' to construct a 'Method' from a function that accepts and
-- returns simple types.
--
-- Use 'method' to construct a 'Method' from a function that handles parameter
-- conversion manually.
--
-- XXX: fix this example:
-- @
--ping :: MethodCall -> IO 'Reply'
--ping _ = ReplyReturn []
--
--sayHello :: String -> IO String
--sayHello name = return (\"Hello \" ++ name ++ \"!\")
--
--export client \"/hello_world\"
--    [ 'method' \"com.example.HelloWorld\" \"Ping\" ping
--    , 'autoMethod' \"com.example.HelloWorld\" \"Hello\" sayHello
--    ]
-- @
export :: Client -> ObjectPath -> Interface -> IO ()
export client path interface =
  atomicModifyIORef_ (clientObjects client) $ addInterface path interface

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

propertiesInterface :: InterfaceName
propertiesInterface = fromString "org.freedesktop.DBus.Properties"

getAllMemberName :: MemberName
getAllMemberName = fromString "GetAll"

getMemberName :: MemberName
getMemberName = fromString "Get"

setMemberName :: MemberName
setMemberName = fromString "Set"

-- | Revokes the export of the given 'ObjectPath'. This will remove all
-- interfaces and methods associated with the path.
unexport :: Client -> ObjectPath -> IO ()
unexport client path = atomicModifyIORef_ (clientObjects client) clear
  where clear = over (modifyPathInterfacesLens path) $ const []

findByGetterAndName
  :: (Show a1, Foldable t)
  => t a2 -> (a2 -> a1) -> String -> Maybe a2
findByGetterAndName options getter name =
  find ((== name) . show . getter) options

findInterface :: InterfaceName -> PathInfo -> Maybe Interface
findInterface name info =
  findByGetterAndName (_pathInterfaces info) interfaceName (show name)

findMethod :: MemberName -> Interface -> Maybe Method
findMethod name interface =
  findByGetterAndName (interfaceMethods interface) methodName (show name)

findProperty :: MemberName -> Interface -> Maybe Property
findProperty name interface =
  findByGetterAndName (interfaceProperties interface) propertyName (show name)

findMethodOrPropForCall
  :: PathInfo
  -> MethodCall
  -> Either ErrorName (IO Reply)
findMethodOrPropForCall
  info msg@MethodCall{ methodCallInterface = interface
                     , methodCallMember = member
                     , methodCallBody = args
                     , methodCallPath = path
                     }
  | interface == Just propertiesInterface &&
    member == getMemberName = callGet
  | interface == Just propertiesInterface &&
    member == getAllMemberName = callGetAll
  | interface == Just propertiesInterface &&
    member == setMemberName = callSet
  | interface == Just introspectableInterface = callIntrospect
  | otherwise = ($ msg) .  methodHandler <$> findMethodForCall info msg
    where
      -- XXX: Add this once better error handling is supported
      -- notAccesible memberName accessType =
      --   ReplyError errorInvalidParameters
      --                [toVariant $ (printf "Member %s is not %s." memberName accessType :: String)]
      runPropGetter getter =
        do
          value <- getter
          return $ makeReply value
      makeReply value = ReplyReturn [toVariant value]
      getProperty propertyInterfaceName memberName =
        (findInterfaceAtPath info path (fromString <$> propertyInterfaceName)) >>=
        (maybeToEither errorInvalidParameters .
                       findProperty (fromString memberName))
      callGet =
        case map fromVariant args of
          [propertyInterfaceName@(Just _), Just memberName] ->
            do
              property <- getProperty propertyInterfaceName memberName
              -- Use notAccesible here?
              getter <- maybeToEither errorInvalidParameters $ propertyGetter property
              return $ runPropGetter getter
          _ -> Left errorInvalidParameters
      callGetAll =
        case map fromVariant args of
          [propertyInterfaceName@(Just _)] ->
            do
              propertyInterface <- findInterfaceAtPath info path
                                   (fromString <$> propertyInterfaceName)
              let properties = interfaceProperties propertyInterface
                  nameGetters :: [IO (String, Variant)]
                  nameGetters = [ ((coerce name,) <$> getter) |
                                 Property { propertyName = name
                                          , propertyGetter = Just getter
                                          } <- properties]
                  buildResult = (makeReply . M.fromList) <$> T.sequenceA nameGetters
              return buildResult
          _ -> Left errorInvalidParameters
      callSet =
        case args of
          [minterface, mname, mvalue] ->
            case (fromVariant minterface, fromVariant mname, fromVariant mvalue) of
              (propertyInterfaceName@(Just _), Just memberName, Just value) ->
                do
                  property <- getProperty propertyInterfaceName memberName
                  -- XXX: Use notAccesible here?
                  setter <- maybeToEither errorInvalidParameters $ propertySetter property
                  -- XXX: Setter really needs to be able to report errors
                  return $ (setter value) >> (return $ ReplyReturn [])
              _ -> Left errorInvalidParameters
          _ -> Left errorInvalidParameters
      callIntrospect = do
        targetInfo <- maybeToEither errorUnknownObject $ findPath path info
        -- XXX: We should probably return a better error here:
        outputXML <- maybeToEither errorUnknownObject $
                     I.formatXML $ buildIntrospectionObject (coerce path) targetInfo
        return $ return $ makeReply outputXML

findInterfaceAtPath
  :: PathInfo
  -> ObjectPath
  -> Maybe InterfaceName
  -> Either ErrorName Interface
findInterfaceAtPath info path name =
  (maybeToEither errorUnknownObject $ findPath path info) >>=
  (maybeToEither errorUnknownInterface .
                 maybe (const Nothing) findInterface name)

findMethodForCall :: PathInfo -> MethodCall -> Either ErrorName Method
findMethodForCall info msg =
  (findInterfaceAtPath info (methodCallPath msg) (methodCallInterface msg)) >>=
  (maybeToEither errorUnknownMethod . findMethod (methodCallMember msg))

returnInvalidParameters :: IO Reply
returnInvalidParameters = return $ ReplyError errorInvalidParameters []

alwaysPresentInterfaces :: [I.Interface]
alwaysPresentInterfaces =
  [ I.Interface { I.interfaceName = propertiesInterface
                , I.interfaceMethods =
                  [ I.Method
                    { I.methodName = "Get"
                    , I.methodArgs =
                      [ I.MethodArg { I.methodArgName = "interfaceName"
                                    , I.methodArgType = TypeString
                                    , I.methodArgDirection = I.In
                                    }
                      , I.MethodArg { I.methodArgName = "memberName"
                                    , I.methodArgType = TypeString
                                    , I.methodArgDirection =  I.In
                                    }
                      , I.MethodArg { I.methodArgName = "value"
                                    , I.methodArgType = TypeVariant
                                    , I.methodArgDirection = I.Out
                                    }
                      ]
                    }
                  , I.Method
                    { I.methodName = "Set"
                    , I.methodArgs =
                      [ I.MethodArg { I.methodArgName = "interfaceName"
                                    , I.methodArgType = TypeString
                                    , I.methodArgDirection = I.In
                                    }
                      , I.MethodArg { I.methodArgName = "memberName"
                                    , I.methodArgType = TypeString
                                    , I.methodArgDirection =  I.In
                                    }
                      , I.MethodArg { I.methodArgName = "value"
                                    , I.methodArgType = TypeVariant
                                    , I.methodArgDirection = I.In
                                    }
                      ]
                    }
                  , I.Method
                    { I.methodName = "GetAll"
                    , I.methodArgs =
                      [ I.MethodArg { I.methodArgName = "interfaceName"
                                    , I.methodArgType = TypeString
                                    , I.methodArgDirection = I.In
                                    }
                      , I.MethodArg { I.methodArgName = "values"
                                    , I.methodArgType = TypeDictionary TypeString TypeVariant
                                    , I.methodArgDirection = I.Out
                                    }
                      ]
                    }
                  ]
                , I.interfaceProperties = []
                , I.interfaceSignals = []
                }
  , I.Interface { I.interfaceName = introspectableInterface
                , I.interfaceMethods =
                  [ I.Method
                    { I.methodName = "Introspect"
                    , I.methodArgs =
                      [ I.MethodArg { I.methodArgName = "output"
                                    , I.methodArgType = TypeString
                                    , I.methodArgDirection = I.Out
                                    }
                      ]
                    }
                  ]
                , I.interfaceProperties = []
                , I.interfaceSignals = []
                }
  ]

buildIntrospectionObject :: String -> PathInfo -> I.Object
buildIntrospectionObject path
                         PathInfo
                         { _pathInterfaces = interfaces
                         , _pathChildren = infoChildren
                         } =
  I.Object
     { I.objectPath = fromString path
     , I.objectInterfaces =
       (if null interfaces then [] else alwaysPresentInterfaces) ++
       (map buildIntrospectionInterface interfaces)
     -- XXX: Eventually we should support not outputting everything if there is
     -- a lot of stuff.
     , I.objectChildren = M.elems $ M.mapWithKey recurseFromString infoChildren
     }
    where recurseFromString stringNode nodeInfo =
            flip buildIntrospectionObject nodeInfo $ printf "%s/%s" path stringNode

buildIntrospectionInterface :: Interface -> I.Interface
buildIntrospectionInterface Interface
  { interfaceName = name
  , interfaceMethods = methods
  , interfaceProperties = properties
  , interfaceSignals = signals
  } =
  I.Interface
   { I.interfaceName = name
   , I.interfaceMethods = map buildIntrospectionMethod methods
   , I.interfaceProperties = map buildIntrospectionProperty properties
   , I.interfaceSignals = signals
   }

buildIntrospectionProperty :: Property -> I.Property
buildIntrospectionProperty (Property memberName ptype getter setter) =
  I.Property { I.propertyName = show memberName
             , I.propertyType = ptype
             , I.propertyRead = isJust getter
             , I.propertyWrite = isJust setter
             }

buildIntrospectionMethod :: Method -> I.Method
buildIntrospectionMethod Method
  { methodName = name
  , inSignature = inSig
  , outSignature = outSig
  } = I.Method
    { I.methodName = name
    , I.methodArgs = zipWith makeMethodArg ['a'..'z'] $ inTuples ++ outTuples
    }
  where inTuples = map (, I.In) $ coerce inSig
        outTuples = map (, I.Out) $ coerce outSig
        makeMethodArg nameChar (t, dir) =
          I.MethodArg { I.methodArgName = [nameChar]
                      , I.methodArgType = t
                      , I.methodArgDirection = dir
                      }

-- | Used to automatically generate method signatures for introspection
-- documents. To support automatic signatures, a method's parameters and
-- return value must all be instances of 'IsValue'.
--
-- This class maps Haskell idioms to D-Bus; it is therefore unable to
-- generate some signatures. In particular, it does not support methods
-- which accept/return a single structure, or single-element structures.
-- It also cannot generate signatures for methods with parameters or return
-- values which are only instances of 'IsVariant'. For these cases, please
-- use 'DBus.Client.method'.
--
-- To match common Haskell use, if the return value is a tuple, it will be
-- converted to a list of return values.
class AutoMethod a where
    funTypes :: a -> ([Type], [Type])
    apply :: a -> [Variant] -> IO Reply

instance AutoMethod (IO ()) where
    funTypes _ = ([], [])

    apply io [] = (io >> (return $ ReplyReturn []))
    apply _ _ = returnInvalidParameters

instance IsValue a => AutoMethod (IO a) where
    funTypes io = cased where
        cased = ([], case ioT io undefined of
            (_, t) -> case t of
                TypeStructure ts -> ts
                _ -> [t])

        ioT :: IsValue a => IO a -> a -> (a, Type)
        ioT _ a = (a, typeOf a)

    apply io [] = ReplyReturn <$> (do
        var <- fmap toVariant io
        case fromVariant var of
            Just struct -> return (structureItems struct)
            Nothing -> return [var])
    apply _ _ = returnInvalidParameters

-- XXX: This is just a hack to allow the use of this type class to apply.
-- Really, applyable out to be broken out.
instance AutoMethod (IO Reply) where
    funTypes _ = undefined

    apply io [] = io
    apply _ _ = returnInvalidParameters

instance (IsValue a, AutoMethod fn) => AutoMethod (a -> fn) where
    funTypes fn = cased where
        cased = case valueT undefined of
            (a, t) -> case funTypes (fn a) of
                (ts, ts') -> (t : ts, ts')

        valueT :: IsValue a => a -> (a, Type)
        valueT a = (a, typeOf a)

    apply _ [] = returnInvalidParameters
    apply fn (v:vs) = case fromVariant v of
        Just v' -> apply (fn v') vs
        Nothing -> returnInvalidParameters

-- | Prepare a Haskell function for export, automatically detecting the
-- function's type signature.
--
-- See 'AutoMethod' for details on the limitations of this function.
--
-- See 'method' for exporting functions with user-defined types.
autoMethod :: (AutoMethod fn) => InterfaceName -> MemberName -> fn -> Method
autoMethod iface name fun = makeMethod name inSig outSig io where
    (typesIn, typesOut) = funTypes fun
    inSig = case signature typesIn of
        Just sig -> sig
        Nothing -> invalid "input"
    outSig = case signature typesOut of
        Just sig -> sig
        Nothing -> invalid "output"
    io msg = apply fun (methodCallBody msg)

    invalid label = error (concat
        [ "Method "
        , formatInterfaceName iface
        , "."
        , formatMemberName name
        , " has an invalid "
        , label
        , " signature."])

autoProperty
  :: forall v. (IsValue v)
  => MemberName -> Maybe (IO v) -> Maybe (v -> IO ()) -> Property
autoProperty name mgetter msetter =
  Property name propType (fmap toVariant <$> mgetter) (variantSetter <$> msetter)
    where propType = typeOf (undefined :: v)
          variantSetter setter =
            let newFun variant = maybe (return ()) setter (fromVariant variant)
            in newFun

errorFailed :: ErrorName
errorFailed = errorName_ "org.freedesktop.DBus.Error.Failed"

errorDisconnected :: ErrorName
errorDisconnected = errorName_ "org.freedesktop.DBus.Error.Disconnected"

errorUnknownObject :: ErrorName
errorUnknownObject = errorName_ "org.freedesktop.DBus.Error.UnknownObject"

errorUnknownInterface :: ErrorName
errorUnknownInterface = errorName_ "org.freedesktop.DBus.Error.UnknownInterface"

errorUnknownMethod :: ErrorName
errorUnknownMethod = errorName_ "org.freedesktop.DBus.Error.UnknownMethod"

errorInvalidParameters :: ErrorName
errorInvalidParameters = errorName_ "org.freedesktop.DBus.Error.InvalidParameters"

dbusName :: BusName
dbusName = busName_ "org.freedesktop.DBus"

dbusPath :: ObjectPath
dbusPath = objectPath_ "/org/freedesktop/DBus"

dbusInterface :: InterfaceName
dbusInterface = interfaceName_ "org.freedesktop.DBus"

introspectableInterface :: InterfaceName
introspectableInterface = interfaceName_ "org.freedesktop.DBus.Introspectable"

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref fn = atomicModifyIORef ref (fn &&& (const ()))

#if !MIN_VERSION_base(4,6,0)
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef ref x = atomicModifyIORef ref (\_ -> (x, ()))
#endif
