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
	, Method
	, method
	, Reply
	, replyReturn
	, replyError
	, throwError
	
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
	
	, RequestNameFlag
	, nameAllowReplacement
	, nameReplaceExisting
	, nameDoNotQueue
	
	, RequestNameReply(NamePrimaryOwner, NameInQueue, NameExists, NameAlreadyOwner)
	, ReleaseNameReply(NameReleased, NameNonExistent, NameNotOwner)
	
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
import           Data.Typeable (Typeable)
import           Data.Word (Word32)

import           DBus
import qualified DBus.Introspection as I
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

-- | An active client session to a message bus. Clients may send or receive
-- method calls, and listen for or emit signals.
data Client = Client
	{ clientSocket :: DBus.Socket.Socket
	, clientPendingCalls :: IORef (Map Serial (MVar (Either MethodError MethodReturn)))
	, clientSignalHandlers :: IORef [Signal -> IO ()]
	, clientObjects :: IORef (Map ObjectPath ObjectInfo)
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

data Method = Method InterfaceName MemberName Signature Signature (MethodCall -> IO Reply)

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
	
	export client "/" [introspectRoot client]
	
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
	pendingCalls <- atomicModifyIORef (clientPendingCalls client) (\p -> (Data.Map.empty, p))
	forM_ (Data.Map.toList pendingCalls) $ \(k, v) -> do
		putMVar v (Left (methodError k errorDisconnected))
	
	atomicModifyIORef (clientSignalHandlers client) (\_ -> ([], ()))
	atomicModifyIORef (clientObjects client) (\_ -> (Data.Map.empty, ()))
	
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
		forM_ handlers (\h -> forkIO (h msg) >> return ())
	go received@(ReceivedMethodCall serial msg) = do
		objects <- readIORef (clientObjects client)
		let sender = methodCallSender msg
		_ <- forkIO $ case findMethod objects msg of
			Right io -> io received
			Left errName -> send_ client
				(methodError serial errName)
					{ methodErrorDestination = sender
					}
				(\_ -> return ())
		return ()
	go _ = return ()
	
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
	-- If ReplyExpected is False, this function would block indefinitely
	-- if the remote side honors it.
	let safeMsg = msg
		{ methodCallReplyExpected = True
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
-- Throws a 'ClientError' if the match rule couldn't be added to the bus.
listen :: Client -> MatchRule -> (Signal -> IO ()) -> IO ()
listen client rule io = do
	let handler msg = when (checkMatchRule rule msg) (io msg)
	
	let formatted = case formatMatchRule rule of
		"" -> "type='signal'"
		x -> "type='signal'," ++ x
	
	atomicModifyIORef (clientSignalHandlers client) (\hs -> (handler : hs, ()))
	_ <- call_ client (methodCall dbusPath dbusInterface "AddMatch")
		{ methodCallDestination = Just dbusName
		, methodCallBody = [toVariant formatted]
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

checkMatchRule :: MatchRule -> Signal -> Bool
checkMatchRule rule msg = and
	[ maybe True (\x -> signalSender msg == Just x) (matchSender rule)
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
method :: InterfaceName
       -> MemberName
       -> Signature -- ^ Input parameter signature
       -> Signature -- ^ Output parameter signature
       -> (MethodCall -> IO Reply)
       -> Method
method iface name inSig outSig io = Method iface name inSig outSig
	(\msg -> Control.Exception.catch
		(Control.Exception.catch
			(io msg)
			(\(MethodExc name' vs') -> return (ReplyError name' vs')))
		(\exc -> return (ReplyError errorFailed
			[toVariant (show (exc :: SomeException))])))

-- | Export the given functions under the given 'ObjectPath' and
-- 'InterfaceName'. 
--
-- Use 'autoMethod' to construct a 'Method' from a function that accepts and
-- returns simple types.
--
-- Use 'method' to construct a 'Method' from a function that handles parameter
-- conversion manually.
--
-- @
--ping :: MethodCall -> IO 'Reply'
--ping _ = replyReturn []
--
--sayHello :: String -> IO String
--sayHello name = return (\"Hello \" ++ name ++ \"!\")
--
--export client \"/hello_world\"
--    [ 'method' \"com.example.HelloWorld\" \"Ping\" ping
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
		reply <- cb msg
		let sender = methodCallSender msg
		case reply of
			ReplyReturn vs -> send_ client (methodReturn serial)
				{ methodReturnDestination = sender
				, methodReturnBody = vs
				} (\_ -> return ())
			ReplyError name vs -> send_ client (methodError serial name)
				{ methodErrorDestination = sender
				, methodErrorBody = vs
				} (\_ -> return ())
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
	return (I.object "/")
		{ I.objectInterfaces =
			[ (I.interface interfaceIntrospectable)
				{ I.interfaceMethods =
					[ (I.method "Introspect")
						{ I.methodArgs =
							[ I.methodArg "" TypeString I.directionOut
							]
						}
					]
				}
			]
		, I.objectChildren = [I.object p | p <- paths]
		}

methodIntrospect :: IO I.Object -> Method
methodIntrospect get = method interfaceIntrospectable "Introspect" "" "s" $
	\msg -> case methodCallBody msg of
		[] -> do
			obj <- get
			let Just xml = I.formatXML obj
			return (replyReturn [toVariant xml])
		_ -> return (replyError errorInvalidParameters [])

introspect :: ObjectPath -> ObjectInfo -> I.Object
introspect path obj = (I.object path) { I.objectInterfaces = interfaces } where
	interfaces = map introspectIface (Data.Map.toList obj)
	
	introspectIface (name, iface) = (I.interface name)
		{ I.interfaceMethods = concatMap introspectMethod (Data.Map.toList iface)
		}
	
	args inSig outSig =
		map (introspectArg I.directionIn) (signatureTypes inSig) ++
		map (introspectArg I.directionOut) (signatureTypes outSig)
	
	introspectMethod (name, MethodInfo inSig outSig _) =
		[ (I.method name)
			{ I.methodArgs = args inSig outSig
			}
		]
	
	introspectArg dir t = I.methodArg "" t dir

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

-- | Prepare a Haskell function for export, automatically detecting the
-- function's type signature.
--
-- See 'AutoMethod' for details on the limitations of this function.
--
-- See 'method' for exporting functions with user-defined types.
autoMethod :: (AutoMethod fn) => InterfaceName -> MemberName -> fn -> Method
autoMethod iface name fun = DBus.Client.method iface name inSig outSig io where
	(typesIn, typesOut) = funTypes fun
	inSig = case signature typesIn of
		Just sig -> sig
		Nothing -> invalid "input"
	outSig = case signature typesOut of
		Just sig -> sig
		Nothing -> invalid "output"
	io msg = case apply fun (methodCallBody msg) of
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
