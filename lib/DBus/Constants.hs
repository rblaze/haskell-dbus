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

module DBus.Constants where

import           DBus

dbusName :: BusName
dbusName = busName_ "org.freedesktop.DBus"

dbusPath :: ObjectPath
dbusPath = objectPath_ "/org/freedesktop/DBus"

dbusInterface :: InterfaceName
dbusInterface = interfaceName_ "org.freedesktop.DBus"

interfaceIntrospectable :: InterfaceName
interfaceIntrospectable = interfaceName_ "org.freedesktop.DBus.Introspectable"

interfacePeer :: InterfaceName
interfacePeer = interfaceName_ "org.freedesktop.DBus.Peer"

interfaceProperties :: InterfaceName
interfaceProperties = interfaceName_ "org.freedesktop.DBus.Properties"

errorAccessDenied :: ErrorName
errorAccessDenied = errorName_ "org.freedesktop.DBus.Error.AccessDenied"

errorAuthFailed :: ErrorName
errorAuthFailed = errorName_ "org.freedesktop.DBus.Error.AuthFailed"

errorDisconnected :: ErrorName
errorDisconnected = errorName_ "org.freedesktop.DBus.Error.Disconnected"

errorFailed :: ErrorName
errorFailed = errorName_ "org.freedesktop.DBus.Error.Failed"

errorNoReply :: ErrorName
errorNoReply = errorName_ "org.freedesktop.DBus.Error.NoReply"

errorNoServer :: ErrorName
errorNoServer = errorName_ "org.freedesktop.DBus.Error.NoServer"

errorTimedOut :: ErrorName
errorTimedOut = errorName_ "org.freedesktop.DBus.Error.TimedOut"

errorTimeout :: ErrorName
errorTimeout = errorName_ "org.freedesktop.DBus.Error.Timeout"

errorServiceUnknown :: ErrorName
errorServiceUnknown = errorName_ "org.freedesktop.DBus.Error.ServiceUnknown"

errorUnknownObject :: ErrorName
errorUnknownObject = errorName_ "org.freedesktop.DBus.Error.UnknownObject"

errorUnknownInterface :: ErrorName
errorUnknownInterface = errorName_ "org.freedesktop.DBus.Error.UnknownInterface"

errorUnknownMethod :: ErrorName
errorUnknownMethod = errorName_ "org.freedesktop.DBus.Error.UnknownMethod"

errorInvalidParameters :: ErrorName
errorInvalidParameters = errorName_ "org.freedesktop.DBus.Error.InvalidParameters"

errorSpawnChildExited :: ErrorName
errorSpawnChildExited = errorName_ "org.freedesktop.DBus.Error.Spawn.ChildExited"

errorSpawnChildSignaled :: ErrorName
errorSpawnChildSignaled = errorName_ "org.freedesktop.DBus.Error.Spawn.ChildSignaled"

errorSpawnConfigInvalid :: ErrorName
errorSpawnConfigInvalid = errorName_ "org.freedesktop.DBus.Error.Spawn.ConfigInvalid"

errorSpawnExecFailed :: ErrorName
errorSpawnExecFailed = errorName_ "org.freedesktop.DBus.Error.Spawn.ExecFailed"

errorSpawnForkFailed :: ErrorName
errorSpawnForkFailed = errorName_ "org.freedesktop.DBus.Error.Spawn.ForkFailed"

errorSpawnFailed :: ErrorName
errorSpawnFailed = errorName_ "org.freedesktop.DBus.Error.Spawn.Failed"

errorSpawnFailedToSetup :: ErrorName
errorSpawnFailedToSetup = errorName_ "org.freedesktop.DBus.Error.Spawn.FailedToSetup"

errorSpawnFileInvalid :: ErrorName
errorSpawnFileInvalid = errorName_ "org.freedesktop.DBus.Error.Spawn.FileInvalid"

errorSpawnNoMemory :: ErrorName
errorSpawnNoMemory = errorName_ "org.freedesktop.DBus.Error.Spawn.NoMemory"

errorSpawnPermissionsInvalid :: ErrorName
errorSpawnPermissionsInvalid = errorName_ "org.freedesktop.DBus.Error.Spawn.PermissionsInvalid"

errorSpawnServiceNotFound :: ErrorName
errorSpawnServiceNotFound = errorName_ "org.freedesktop.DBus.Error.Spawn.ServiceNotFound"

errorSpawnServiceNotValid :: ErrorName
errorSpawnServiceNotValid = errorName_ "org.freedesktop.DBus.Error.Spawn.ServiceNotValid"

errorAddressInUse :: ErrorName
errorAddressInUse = errorName_ "org.freedesktop.DBus.Error.AddressInUse"

errorAdtAuditDataUnknown :: ErrorName
errorAdtAuditDataUnknown = errorName_ "org.freedesktop.DBus.Error.AdtAuditDataUnknown"

errorBadAddress :: ErrorName
errorBadAddress = errorName_ "org.freedesktop.DBus.Error.BadAddress"

errorFileExists :: ErrorName
errorFileExists = errorName_ "org.freedesktop.DBus.Error.FileExists"

errorFileNotFound :: ErrorName
errorFileNotFound = errorName_ "org.freedesktop.DBus.Error.FileNotFound"

errorInconsistentMessage :: ErrorName
errorInconsistentMessage = errorName_ "org.freedesktop.DBus.Error.InconsistentMessage"

errorInvalidFileContent :: ErrorName
errorInvalidFileContent = errorName_ "org.freedesktop.DBus.Error.InvalidFileContent"

errorIOError :: ErrorName
errorIOError = errorName_ "org.freedesktop.DBus.Error.IOError"

errorLimitsExceeded :: ErrorName
errorLimitsExceeded = errorName_ "org.freedesktop.DBus.Error.LimitsExceeded"

errorMatchRuleInvalid :: ErrorName
errorMatchRuleInvalid = errorName_ "org.freedesktop.DBus.Error.MatchRuleInvalid"

errorMatchRuleNotFound :: ErrorName
errorMatchRuleNotFound = errorName_ "org.freedesktop.DBus.Error.MatchRuleNotFound"

errorNameHasNoOwner :: ErrorName
errorNameHasNoOwner = errorName_ "org.freedesktop.DBus.Error.NameHasNoOwner"

errorNoMemory :: ErrorName
errorNoMemory = errorName_ "org.freedesktop.DBus.Error.NoMemory"

errorNoNetwork :: ErrorName
errorNoNetwork = errorName_ "org.freedesktop.DBus.Error.NoNetwork"

errorNotSupported :: ErrorName
errorNotSupported = errorName_ "org.freedesktop.DBus.Error.NotSupported"

errorObjectPathInUse :: ErrorName
errorObjectPathInUse = errorName_ "org.freedesktop.DBus.Error.ObjectPathInUse"

errorSELinuxSecurityContextUnknown :: ErrorName
errorSELinuxSecurityContextUnknown = errorName_ "org.freedesktop.DBus.Error.SELinuxSecurityContextUnknown"

errorUnixProcessIdUnknown :: ErrorName
errorUnixProcessIdUnknown = errorName_ "org.freedesktop.DBus.Error.UnixProcessIdUnknown"
