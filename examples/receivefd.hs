{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import DBus
import DBus.Client
import DBus.Socket (authenticatorWithUnixFds, defaultSocketOptions, socketAuthenticator)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import System.Posix.IO (fdToHandle, handleToFd)
import System.Posix.Types (Fd)

exampleObjectPath :: ObjectPath
exampleObjectPath = "/"
exampleInterfaceName :: InterfaceName
exampleInterfaceName = "org.example.receivefd"
exampleBusName :: BusName
exampleBusName = "org.example.receivefd"

receivefd :: Fd -> IO String
receivefd fd = fdToHandle fd >>= hGetContents

interface :: Interface
interface =
    defaultInterface
        { interfaceName = exampleInterfaceName
        , interfaceMethods = [autoMethod "receivefd" receivefd]
        }

main :: IO ()
main = do
    let sockopt = defaultSocketOptions{socketAuthenticator = authenticatorWithUnixFds}
    let clientopt = defaultClientOptions{clientSocketOptions = sockopt}
    Just sessionBus <- getSessionAddress
    client <- connectWith clientopt sessionBus
    bracket
        (requestName client exampleBusName [nameDoNotQueue])
        (\_ -> releaseName client exampleBusName)
        ( \reservation ->
            case reservation of
                NamePrimaryOwner -> do
                    putStrLn $
                        unwords
                            [ "sendfd"
                            , formatBusName exampleBusName
                            , formatObjectPath exampleObjectPath
                            , formatInterfaceName exampleInterfaceName
                            , "receivefd"
                            , "dbus.cabal"
                            ]
                    export client exampleObjectPath interface
                    forever $ threadDelay 5000
                _ -> print reservation
        )
