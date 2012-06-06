{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

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

module DBus.Client
	(
	-- * Clients
	  Client
	
	-- * Client errors
	, ClientError
	, clientError
	, clientErrorMessage
	, clientErrorFatal
	
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
	, Method
	, Reply
	, replyReturn
	, replyError
	, export
	, throwError
	, method
	
	-- ** Automatic method signatures
	, AutoMethod
	, autoMethod
	
	-- * Signals
	, listen
	, emit
	
	-- ** Match rules
	, MatchRule
	, formatMatchRule
	, matchAny
	, matchSender
	, matchDestination
	, matchPath
	, matchInterface
	, matchMember
	
	-- * Name reservation
	, requestName
	, releaseName
	, RequestNameFlag(..)
	, RequestNameReply(..)
	, ReleaseNameReply(..)
	
	-- * Advanced connection options
	, ClientOptions
	, clientSocketOptions
	, defaultClientOptions
	, connectWith
	
	-- * Object proxies
	, Proxy
	, proxy
	, proxyCall
	, proxyListen
	) where

import           Control.Concurrent
import           Control.Exception (SomeException, throwIO)
import qualified Control.Exception
import           Control.Monad (forever, forM_, when)
import           Data.Bits ((.|.))
import           Data.IORef
import           Data.List (foldl', intercalate)
import qualified Data.Map
import           Data.Map (Map)
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Text (Text)
import qualified Data.Text
import           Data.Typeable (Typeable)
import           Data.Word (Word32)

import           DBus
import qualified DBus.Introspection
import qualified DBus.Socket
import           DBus.Transport (TransportOpen, SocketTransport)

data ClientError = ClientError
	{ clientErrorMessage :: String
	, clientErrorFatal :: Bool
	}
	deriving (Eq, Show, Typeable)

instance Control.Exception.Exception ClientError

clientError :: String -> ClientError
clientError msg = ClientError msg True

-- | Represents an active client session to a message bus. Clients may
-- send or receive method calls, and listen for or emit signals.
--
-- Example: connect to the session bus, and get a list of active names.
--
-- @
--{-\# LANGUAGE OverloadedStrings \#-}
--
--main = do
--    client <- 'connectSession'
--    bus <- 'proxy' client \"org.freedesktop.DBus\" \"/org/freedesktop/DBus\"
--    [names] <- 'proxyCall' bus \"org.freedesktop.DBus\" \"ListNames\" []
--    print names
-- @
--
data Client = Client
	{ clientSocket :: DBus.Socket.Socket
	, clientPendingCalls :: IORef (Map Serial (MVar (Either MethodError MethodReturn)))
	, clientSignalHandlers :: IORef [Signal -> IO ()]
	, clientObjects :: IORef (Map ObjectPath ObjectInfo)
	, clientThreadID :: ThreadId
	}

data ClientOptions t = ClientOptions
	{
	-- | If specified, forces connection attempts to abort after the given
	-- number of milliseconds.
	  clientTimeout :: Maybe Integer
	
	-- | Whether the client should attempt to reconnect, if it loses its
	-- connection to the server. any pending method calls will fail with
	-- an error saying the connection was lost.
	, clientReconnect :: Bool
	
	-- | Options for the underlying socket, for advanced use cases. See
	-- the "DBus.Socket" module.
	, clientSocketOptions :: DBus.Socket.SocketOptions t
	}

type Callback = (ReceivedMessage -> IO ())

data Reply
	= ReplyReturn [Variant]
	| ReplyError ErrorName [Variant]

-- | Reply to a method call with a successful return, containing the given body.
replyReturn :: [Variant] -> Reply
replyReturn = ReplyReturn

-- | Reply to a method call with an error, containing the given error name and
-- body.
--
-- Typically, the first item of the error body is a string with a message
-- describing the error.
replyError :: ErrorName -> [Variant] -> Reply
replyError = ReplyError

data Method = Method InterfaceName MemberName Signature Signature ([Variant] -> IO Reply)

type ObjectInfo = Map InterfaceName InterfaceInfo
type InterfaceInfo = Map MemberName MethodInfo
data MethodInfo = MethodInfo Signature Signature Callback

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
	
	pendingCalls <- newIORef Data.Map.empty
	signalHandlers <- newIORef []
	objects <- newIORef Data.Map.empty
	
	clientMVar <- newEmptyMVar
	threadID <- forkIO $ do
		client <- readMVar clientMVar
		mainLoop client
	
	let client = Client
		{ clientSocket = sock
		, clientPendingCalls = pendingCalls
		, clientSignalHandlers = signalHandlers
		, clientObjects = objects
		, clientThreadID = threadID
		}
	putMVar clientMVar client
	
	export client "/" [introspectRoot client]
	
	callNoReply client MethodCall
		{ methodCallPath = "/org/freedesktop/DBus"
		, methodCallMember = "Hello"
		, methodCallInterface = Just "org.freedesktop.DBus"
		, methodCallSender = Nothing
		, methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallFlags = []
		, methodCallBody = []
		}
	
	return client

-- | Default client options. Uses the built-in Socket-based transport, which
-- supports the @tcp:@ and @unix:@ methods.
defaultClientOptions :: ClientOptions SocketTransport
defaultClientOptions = ClientOptions
	{ clientTimeout = Nothing
	, clientReconnect = True
	, clientSocketOptions = DBus.Socket.defaultSocketOptions
	}

-- | Stop a 'Client''s callback thread and close its underlying socket.
disconnect :: Client -> IO ()
disconnect client = do
	killThread (clientThreadID client)
	disconnect' client

disconnect' :: Client -> IO ()
disconnect' client = do
	pendingCalls <- atomicModifyIORef (clientPendingCalls client) (\p -> (Data.Map.empty, p))
	forM_ (Data.Map.toList pendingCalls) $ \(k, v) -> do
		putMVar v (Left MethodError
			{ methodErrorName = errorDisconnected
			, methodErrorSerial = k
			, methodErrorSender = Nothing
			, methodErrorDestination = Nothing
			, methodErrorBody = []
			})
	
	atomicModifyIORef (clientSignalHandlers client) (\_ -> ([], ()))
	atomicModifyIORef (clientObjects client) (\_ -> (Data.Map.empty, ()))
	
	DBus.Socket.close (clientSocket client)

mainLoop :: Client -> IO ()
mainLoop client = forever $ do
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
		forM_ handlers (\h -> forkIO (h msg) >> return ())
	go received@(ReceivedMethodCall serial msg) = do
		objects <- readIORef (clientObjects client)
		let sender = methodCallSender msg
		_ <- forkIO $ case findMethod objects msg of
			Right io -> io received
			Left errName -> send_ client
				(MethodError errName serial Nothing sender [])
				(\_ -> return ())
		return ()
	go (ReceivedUnknown _ _) = return ()
	
	dispatchReply serial result = do
		pending <- atomicModifyIORef
			(clientPendingCalls client)
			(\p -> case Data.Map.lookup serial p of
				Nothing -> (p, Nothing)
				Just mvar -> (Data.Map.delete serial p, Just mvar))
		case pending of
			Just mvar -> putMVar mvar result
			Nothing -> return ()

data RequestNameFlag
	-- | Allow this client's reservation to be replaced, if another client
	-- requests it with the 'ReplaceExisting' flag.
	--
	-- If this client's reservation is replaced, this client will be added
	-- to the wait queue unless the request also included the 'DoNotQueue'
	-- flag.
	= AllowReplacement
	
	-- | If the name being requested is already reserved, attempt to
	-- replace it. This only works if the current owner provided the
	-- 'AllowReplacement' flag.
	| ReplaceExisting
	
	-- | If the name is already in use, do not add this client to the
	-- queue, just return an error.
	| DoNotQueue
	deriving (Eq, Show)

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
	deriving (Eq, Show)

data ReleaseNameReply
	-- | This client has released the provided name.
	= NameReleased
	
	-- | The provided name is not assigned to any client on the bus.
	| NameNonExistent
	
	-- | The provided name is not assigned to this client.
	| NameNotOwner
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
	reply <- call_ client MethodCall
		{ methodCallPath = "/org/freedesktop/DBus"
		, methodCallInterface = Just "org.freedesktop.DBus"
		, methodCallMember = "RequestName"
		, methodCallSender = Nothing
		, methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallFlags = []
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
	case code :: Word32 of
		1 -> return NamePrimaryOwner
		2 -> return NameInQueue
		3 -> return NameExists
		4 -> return NameAlreadyOwner
		_ -> throwIO (clientError ("requestName: received unknown response code " ++ show code))
			{ clientErrorFatal = False
			}

-- | Release a name that this client previously requested. See 'requestName'
-- for an explanation of name reservation.
--
-- Throws a 'ClientError' if the call failed.
releaseName :: Client -> BusName -> IO ReleaseNameReply
releaseName client name = do
	reply <- call_ client MethodCall
		{ methodCallPath = "/org/freedesktop/DBus"
		, methodCallInterface = Just "org.freedesktop.DBus"
		, methodCallMember = "ReleaseName"
		, methodCallSender = Nothing
		, methodCallDestination = Just "org.freedesktop.DBus"
		, methodCallFlags = []
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
	case code :: Word32 of
		1 -> return NameReleased
		2 -> return NameNonExistent
		3 -> return NameNotOwner
		_ -> throwIO (clientError ("releaseName: received unknown response code " ++ show code))
			{ clientErrorFatal = False
			}

send_ :: Message msg => Client -> msg -> (Serial -> IO a) -> IO a
send_ client msg io = do
	result <- Control.Exception.try (DBus.Socket.send (clientSocket client) msg io)
	case result of
		Right serial -> return serial
		Left err -> throwIO (clientError (DBus.Socket.socketErrorMessage err))
			{ clientErrorFatal = DBus.Socket.socketErrorFatal err
			}

-- | Send a method call to the bus, and wait for the response.
--
-- Throws a 'ClientError' if the method call couldn't be sent, or if the reply
-- couldn't be parsed.
call :: Client -> MethodCall -> IO (Either MethodError MethodReturn)
call client msg = do
	-- Remove some fields that should not be set:
	--
	-- methodCallSender: not used in client/bus mode.
	-- NoReplyExpected: can cause this function to block indefinitely.
	let safeMsg = msg
		{ methodCallSender = Nothing
		, methodCallFlags = filter (/= NoReplyExpected) (methodCallFlags msg)
		}
	mvar <- newEmptyMVar
	send_ client safeMsg (\serial -> do
		let ref = clientPendingCalls client
		
		-- If the mvar is finalized, remove its serial from the pending
		-- call map. This allows calls to be canceled, by using
		-- something like 'timeout' to make 'call' return early, and
		-- having the finalizer clear out the map.
		addMVarFinalizer mvar (atomicModifyIORef ref (\p -> (Data.Map.delete serial p, ())))
		
		atomicModifyIORef ref (\p -> (Data.Map.insert serial mvar p, ())))
	takeMVar mvar

-- | Send a method call to the bus, and wait for the response.
--
-- Unsets the 'NoReplyExpected' message flag before sending.
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
-- Sets the 'NoReplyExpected' message flag before sending.
--
-- Throws a 'ClientError' if the method call couldn't be sent.
callNoReply :: Client -> MethodCall -> IO ()
callNoReply client msg = do
	-- Ensure that NoReplyExpected is always set.
	let safeMsg = msg
		{ methodCallSender = Nothing
		, methodCallFlags = NoReplyExpected : methodCallFlags msg
		}
	send_ client safeMsg (\_ -> return ())

-- | Request that the bus forward signals matching the given rule to this
-- client, and process them in a callback.
--
-- A received signal might be processed by more than one callback at a time.
-- Callbacks each run in their own thread.
--
-- Throws a 'ClientError' if adding the match rule couldn't be added to the
-- bus.
listen :: Client -> MatchRule -> (BusName -> Signal -> IO ()) -> IO ()
listen client rule io = do
	let handler msg = case signalSender msg of
		Just sender -> when (checkMatchRule rule sender msg) (io sender msg)
		Nothing -> return ()
	
	atomicModifyIORef (clientSignalHandlers client) (\hs -> (handler : hs, ()))
	callNoReply client MethodCall
		{ methodCallPath = dbusPath
		, methodCallMember = "AddMatch"
		, methodCallInterface = Just dbusInterface
		, methodCallSender = Nothing
		, methodCallDestination = Just dbusName
		, methodCallFlags = []
		, methodCallBody = [toVariant (formatMatchRule rule)]
		}
	return ()

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
	  matchSender :: Maybe BusName
	
	-- | If set, only receives signals sent to the given bus name.
	, matchDestination :: Maybe BusName
	
	-- | If set, only receives signals sent with the given path.
	, matchPath  :: Maybe ObjectPath
	
	-- | If set, only receives signals sent with the given interface name.
	, matchInterface :: Maybe InterfaceName
	
	-- | If set, only receives signals sent with the given member name.
	, matchMember :: Maybe MemberName
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
		]
	
	f :: String -> (MatchRule -> Maybe a) -> (a -> String) -> Maybe String
	f key get text = do
		val <- fmap text (get rule)
		return (concat [key, "='", val, "'"])

-- | Match any signal.
matchAny :: MatchRule
matchAny = MatchRule Nothing Nothing Nothing Nothing Nothing

checkMatchRule :: MatchRule -> BusName -> Signal -> Bool
checkMatchRule rule sender msg = and
	[ maybe True (== sender) (matchSender rule)
	, maybe True (\x -> signalDestination msg == Just x) (matchDestination rule)
	, maybe True (== signalPath msg) (matchPath rule)
	, maybe True (== signalInterface msg) (matchInterface rule)
	, maybe True (== signalMember msg) (matchMember rule)
	]

data MethodExc = MethodExc ErrorName [Variant]
	deriving (Show, Eq, Typeable)

instance Control.Exception.Exception MethodExc

-- | Normally, any exceptions raised while executing a method will be
-- given the generic @\"org.freedesktop.DBus.Error.Failed\"@ name.
-- 'throwError' allows the programmer to specify an error name, and provide
-- additional information to the remote application. You may use this instead
-- of 'Control.Exception.throwIO' to abort a method call.
throwError :: ErrorName -> Text -> [Variant] -> IO a
throwError name message extra = Control.Exception.throwIO (MethodExc name (toVariant message : extra))

-- | Define a method handler, which will accept method calls with the given
-- interface and member name.
--
-- Note that the input and output parameter signatures are used for
-- introspection, but are not checked when executing a method.
method :: InterfaceName
       -> MemberName
       -> Signature -- ^ Input parameter signature
       -> Signature -- ^ Output parameter signature
       -> ([Variant] -> IO Reply)
       -> Method
method iface name inSig outSig io = Method iface name inSig outSig
	(\vs -> Control.Exception.catch
		(Control.Exception.catch
			(io vs)
			(\(MethodExc name' vs') -> return (ReplyError name' vs')))
		(\exc -> return (ReplyError errorFailed
			[toVariant (Data.Text.pack (show (exc :: SomeException)))])))

-- | Export the given functions under the given 'ObjectPath' and
-- 'InterfaceName'. 
--
-- The functions may accept/return any types that are
-- instances of 'IsValue'; see 'AutoMethod'.
--
-- @
--sayHello :: Text -> IO Text
--sayHello name = return ('Data.Text.concat' [\"Hello \", name, \"!\"])
--
--export client \"/hello_world\"
--    [ 'method'
--    , 'autoMethod' \"com.example.HelloWorld\" \"Hello\" sayHello
--    ]
-- @
export :: Client -> ObjectPath -> [Method] -> IO ()
export client path methods = atomicModifyIORef (clientObjects client) addObject where
	addObject objs = (Data.Map.insert path info objs, ())
	
	info = foldl' addMethod Data.Map.empty (defaultIntrospect : methods)
	addMethod m (Method iface name inSig outSig cb) = Data.Map.insertWith'
		Data.Map.union iface
		(Data.Map.fromList [(name, MethodInfo inSig outSig (wrapCB cb))]) m
	
	wrapCB cb (ReceivedMethodCall serial msg) = do
		reply <- cb (methodCallBody msg)
		let sender = methodCallSender msg
		case reply of
			ReplyReturn vs -> send_ client (MethodReturn serial Nothing sender vs) (\_ -> return ())
			ReplyError name vs -> send_ client (MethodError name serial Nothing sender vs) (\_ -> return ())
	wrapCB _ _ = return ()
	
	defaultIntrospect = methodIntrospect $ do
		objects <- readIORef (clientObjects client)
		let Just obj = Data.Map.lookup path objects
		return (introspect path obj)

findMethod :: Map ObjectPath ObjectInfo -> MethodCall -> Either ErrorName Callback
findMethod objects msg = case Data.Map.lookup (methodCallPath msg) objects of
	Nothing -> Left errorUnknownObject
	Just obj -> case methodCallInterface msg of
		Nothing -> let
			members = do
				iface <- Data.Map.elems obj
				case Data.Map.lookup (methodCallMember msg) iface of
					Just member -> [member]
					Nothing -> []
			in case members of
				[MethodInfo _ _ io] -> Right io
				_ -> Left errorUnknownMethod
		Just ifaceName -> case Data.Map.lookup ifaceName obj of
			Nothing -> Left errorUnknownInterface
			Just iface -> case Data.Map.lookup (methodCallMember msg) iface of
				Just (MethodInfo _ _ io) -> Right io
				_ -> Left errorUnknownMethod

introspectRoot :: Client -> Method
introspectRoot client = methodIntrospect $ do
	objects <- readIORef (clientObjects client)
	let paths = filter (/= "/") (Data.Map.keys objects)
	return (DBus.Introspection.Object "/"
		[DBus.Introspection.Interface interfaceIntrospectable
			[DBus.Introspection.Method "Introspect"
				[]
				[DBus.Introspection.Parameter "" TypeString]]
			[] []]
		[DBus.Introspection.Object p [] [] | p <- paths])

methodIntrospect :: IO DBus.Introspection.Object -> Method
methodIntrospect get = method interfaceIntrospectable "Introspect" "" "s" impl where
	impl [] = do
		obj <- get
		let Just xml = DBus.Introspection.toXML obj
		return (ReplyReturn [toVariant xml])
	impl _ = return (ReplyError errorInvalidParameters [])

introspect :: ObjectPath -> ObjectInfo -> DBus.Introspection.Object
introspect path obj = DBus.Introspection.Object path interfaces [] where
	interfaces = map introspectIface (Data.Map.toList obj)
	
	introspectIface (name, iface) = let
		members = Data.Map.toList iface
		methods = concatMap introspectMethod members
		in DBus.Introspection.Interface name methods [] []
	
	introspectMethod (name, MethodInfo inSig outSig _) =
		[DBus.Introspection.Method name
			(map introspectParam (signatureTypes inSig))
			(map introspectParam (signatureTypes outSig))]
	
	introspectParam = DBus.Introspection.Parameter ""

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
	apply :: a -> [Variant] -> Maybe (IO [Variant])

instance AutoMethod (IO ()) where
	funTypes _ = ([], [])
	
	apply io [] = Just (io >> return [])
	apply _ _ = Nothing

instance IsValue a => AutoMethod (IO a) where
	funTypes io = cased where
		cased = ([], case ioT io undefined of
			(_, t) -> case t of
				TypeStructure ts -> ts
				_ -> [t])
		
		ioT :: IsValue a => IO a -> a -> (a, Type)
		ioT _ a = (a, typeOf a)
	
	apply io [] = Just (do
		var <- fmap toVariant io
		case fromVariant var of
			Just struct -> return (structureItems struct)
			Nothing -> return [var])
	apply _ _ = Nothing

instance (IsValue a, AutoMethod fn) => AutoMethod (a -> fn) where
	funTypes fn = cased where
		cased = case valueT undefined of
			(a, t) -> case funTypes (fn a) of
				(ts, ts') -> (t : ts, ts')
		
		valueT :: IsValue a => a -> (a, Type)
		valueT a = (a, typeOf a)
	
	apply _ [] = Nothing
	apply fn (v:vs) = case fromVariant v of
		Just v' -> apply (fn v') vs
		Nothing -> Nothing

-- | Prepare a Haskell function for export. This automatically detects the
-- function's type signature; see 'AutoMethod'.
--
-- To manage the type signature and marshaling yourself, use
-- 'DBus.Client.method' instead.
autoMethod:: (AutoMethod fn) => InterfaceName -> MemberName -> fn -> Method
autoMethod iface name fun = DBus.Client.method iface name inSig outSig io where
	(typesIn, typesOut) = funTypes fun
	inSig = case signature typesIn of
		Just sig -> sig
		Nothing -> invalid "input"
	outSig = case signature typesOut of
		Just sig -> sig
		Nothing -> invalid "output"
	io vs = case apply fun vs of
		Nothing -> return (ReplyError errorInvalidParameters [])
		Just io' -> fmap ReplyReturn io'
	
	invalid label = error (concat
		[ "Method "
		, formatInterfaceName iface
		, "."
		, formatMemberName name
		, " has an invalid "
		, label
		, " signature."])

data Proxy = Proxy Client BusName ObjectPath

proxy :: Client -> BusName -> ObjectPath -> IO Proxy
proxy client dest path = return (Proxy client dest path)

proxyCall :: Proxy -> InterfaceName -> MemberName -> [Variant] -> IO [Variant]
proxyCall (Proxy client dest path) iface member body = do
	reply <- call_ client MethodCall
		{ methodCallPath = path
		, methodCallMember = member
		, methodCallInterface = Just iface
		, methodCallSender = Nothing
		, methodCallDestination = Just dest
		, methodCallFlags = []
		, methodCallBody = body
		}
	return (methodReturnBody reply)

proxyListen :: Proxy -> InterfaceName -> MemberName -> (BusName -> Signal -> IO ()) -> IO ()
proxyListen (Proxy client dest path) iface member = DBus.Client.listen client matchAny
	{ matchSender = Just dest
	, matchInterface = Just iface
	, matchMember = Just member
	, matchPath = Just path
	, matchDestination = Nothing
	}

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

interfaceIntrospectable :: InterfaceName
interfaceIntrospectable = interfaceName_ "org.freedesktop.DBus.Introspectable"
