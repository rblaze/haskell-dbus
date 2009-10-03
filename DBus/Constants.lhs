% Copyright (C) 2009 John Millikin <jmillikin@gmail.com>
% 
% This program is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

\ignore{
\begin{code}
module DBus.Constants where
import qualified DBus.Types as T
\end{code}
}

\section{Constants}

\subsection{Main bus location}

\begin{code}
busName :: T.BusName
busName = T.mkBusName' "org.freedesktop.DBus"
\end{code}

\begin{code}
busPath :: T.ObjectPath
busPath = T.mkObjectPath' "/org/freedesktop/DBus"
\end{code}

\begin{code}
busInterface :: T.InterfaceName
busInterface = T.mkInterfaceName' "org.freedesktop.DBus"
\end{code}

\subsection{Other pre-defined interfaces}

\begin{code}
interfaceIntrospectable :: T.InterfaceName
interfaceIntrospectable = T.mkInterfaceName' "org.freedesktop.DBus.Introspectable"
\end{code}

\begin{code}
interfaceProperties :: T.InterfaceName
interfaceProperties = T.mkInterfaceName' "org.freedesktop.DBus.Properties"
\end{code}

\begin{code}
interfacePeer :: T.InterfaceName
interfacePeer = T.mkInterfaceName' "org.freedesktop.DBus.Peer"
\end{code}

\subsection{Pre-defined error names}

\begin{code}
errorFailed :: T.ErrorName
errorFailed = T.mkErrorName' "org.freedesktop.DBus.Error.Failed"
\end{code}

\begin{code}
errorNoMemory :: T.ErrorName
errorNoMemory = T.mkErrorName' "org.freedesktop.DBus.Error.NoMemory"
\end{code}

\begin{code}
errorServiceUnknown :: T.ErrorName
errorServiceUnknown = T.mkErrorName' "org.freedesktop.DBus.Error.ServiceUnknown"
\end{code}

\begin{code}
errorNameHasNoOwner :: T.ErrorName
errorNameHasNoOwner = T.mkErrorName' "org.freedesktop.DBus.Error.NameHasNoOwner"
\end{code}

\begin{code}
errorNoReply :: T.ErrorName
errorNoReply = T.mkErrorName' "org.freedesktop.DBus.Error.NoReply"
\end{code}

\begin{code}
errorIOError :: T.ErrorName
errorIOError = T.mkErrorName' "org.freedesktop.DBus.Error.IOError"
\end{code}

\begin{code}
errorBadAddress :: T.ErrorName
errorBadAddress = T.mkErrorName' "org.freedesktop.DBus.Error.BadAddress"
\end{code}

\begin{code}
errorNotSupported :: T.ErrorName
errorNotSupported = T.mkErrorName' "org.freedesktop.DBus.Error.NotSupported"
\end{code}

\begin{code}
errorLimitsExceeded :: T.ErrorName
errorLimitsExceeded = T.mkErrorName' "org.freedesktop.DBus.Error.LimitsExceeded"
\end{code}

\begin{code}
errorAccessDenied :: T.ErrorName
errorAccessDenied = T.mkErrorName' "org.freedesktop.DBus.Error.AccessDenied"
\end{code}

\begin{code}
errorAuthFailed :: T.ErrorName
errorAuthFailed = T.mkErrorName' "org.freedesktop.DBus.Error.AuthFailed"
\end{code}

\begin{code}
errorNoServer :: T.ErrorName
errorNoServer = T.mkErrorName' "org.freedesktop.DBus.Error.NoServer"
\end{code}

\begin{code}
errorTimeout :: T.ErrorName
errorTimeout = T.mkErrorName' "org.freedesktop.DBus.Error.Timeout"
\end{code}

\begin{code}
errorNoNetwork :: T.ErrorName
errorNoNetwork = T.mkErrorName' "org.freedesktop.DBus.Error.NoNetwork"
\end{code}

\begin{code}
errorAddressInUse :: T.ErrorName
errorAddressInUse = T.mkErrorName' "org.freedesktop.DBus.Error.AddressInUse"
\end{code}

\begin{code}
errorDisconnected :: T.ErrorName
errorDisconnected = T.mkErrorName' "org.freedesktop.DBus.Error.Disconnected"
\end{code}

\begin{code}
errorInvalidArgs :: T.ErrorName
errorInvalidArgs = T.mkErrorName' "org.freedesktop.DBus.Error.InvalidArgs"
\end{code}

\begin{code}
errorFileNotFound :: T.ErrorName
errorFileNotFound = T.mkErrorName' "org.freedesktop.DBus.Error.FileNotFound"
\end{code}

\begin{code}
errorFileExists :: T.ErrorName
errorFileExists = T.mkErrorName' "org.freedesktop.DBus.Error.FileExists"
\end{code}

\begin{code}
errorUnknownMethod :: T.ErrorName
errorUnknownMethod = T.mkErrorName' "org.freedesktop.DBus.Error.UnknownMethod"
\end{code}

\begin{code}
errorTimedOut :: T.ErrorName
errorTimedOut = T.mkErrorName' "org.freedesktop.DBus.Error.TimedOut"
\end{code}

\begin{code}
errorMatchRuleNotFound :: T.ErrorName
errorMatchRuleNotFound = T.mkErrorName' "org.freedesktop.DBus.Error.MatchRuleNotFound"
\end{code}

\begin{code}
errorMatchRuleInvalid :: T.ErrorName
errorMatchRuleInvalid = T.mkErrorName' "org.freedesktop.DBus.Error.MatchRuleInvalid"
\end{code}

\begin{code}
errorSpawnExecFailed :: T.ErrorName
errorSpawnExecFailed = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ExecFailed"
\end{code}

\begin{code}
errorSpawnForkFailed :: T.ErrorName
errorSpawnForkFailed = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ForkFailed"
\end{code}

\begin{code}
errorSpawnChildExited :: T.ErrorName
errorSpawnChildExited = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ChildExited"
\end{code}

\begin{code}
errorSpawnChildSignaled :: T.ErrorName
errorSpawnChildSignaled = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ChildSignaled"
\end{code}

\begin{code}
errorSpawnFailed :: T.ErrorName
errorSpawnFailed = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.Failed"
\end{code}

\begin{code}
errorSpawnFailedToSetup :: T.ErrorName
errorSpawnFailedToSetup = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.FailedToSetup"
\end{code}

\begin{code}
errorSpawnConfigInvalid :: T.ErrorName
errorSpawnConfigInvalid = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ConfigInvalid"
\end{code}

\begin{code}
errorSpawnServiceNotValid :: T.ErrorName
errorSpawnServiceNotValid = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ServiceNotValid"
\end{code}

\begin{code}
errorSpawnServiceNotFound :: T.ErrorName
errorSpawnServiceNotFound = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.ServiceNotFound"
\end{code}

\begin{code}
errorSpawnPermissionsInvalid :: T.ErrorName
errorSpawnPermissionsInvalid = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.PermissionsInvalid"
\end{code}

\begin{code}
errorSpawnFileInvalid :: T.ErrorName
errorSpawnFileInvalid = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.FileInvalid"
\end{code}

\begin{code}
errorSpawnNoMemory :: T.ErrorName
errorSpawnNoMemory = T.mkErrorName' "org.freedesktop.DBus.Error.Spawn.NoMemory"
\end{code}

\begin{code}
errorUnixProcessIdUnknown :: T.ErrorName
errorUnixProcessIdUnknown = T.mkErrorName' "org.freedesktop.DBus.Error.UnixProcessIdUnknown"
\end{code}

\begin{code}
errorInvalidFileContent :: T.ErrorName
errorInvalidFileContent = T.mkErrorName' "org.freedesktop.DBus.Error.InvalidFileContent"
\end{code}

\begin{code}
errorSELinuxSecurityContextUnknown :: T.ErrorName
errorSELinuxSecurityContextUnknown = T.mkErrorName' "org.freedesktop.DBus.Error.SELinuxSecurityContextUnknown"
\end{code}

\begin{code}
errorAdtAuditDataUnknown :: T.ErrorName
errorAdtAuditDataUnknown = T.mkErrorName' "org.freedesktop.DBus.Error.AdtAuditDataUnknown"
\end{code}

\begin{code}
errorObjectPathInUse :: T.ErrorName
errorObjectPathInUse = T.mkErrorName' "org.freedesktop.DBus.Error.ObjectPathInUse"
\end{code}

\begin{code}
errorInconsistentMessage :: T.ErrorName
errorInconsistentMessage = T.mkErrorName' "org.freedesktop.DBus.Error.InconsistentMessage"
\end{code}
