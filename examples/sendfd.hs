{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import DBus
import DBus.Client (call_, clientSocketOptions, connectWith, defaultClientOptions)
import DBus.Socket (authenticatorWithUnixFds, defaultSocketOptions, socketAuthenticator)
import Data.List (sort)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), withFile)
import System.Posix.IO (handleToFd)

main :: IO ()
main = do
    let sockopt = defaultSocketOptions{socketAuthenticator = authenticatorWithUnixFds}
    let clientopt = defaultClientOptions{clientSocketOptions = sockopt}
    (Just sessionBus) <- getSessionAddress
    client <- connectWith clientopt sessionBus

    getArgs >>= \case
        [busName, objectPath, interfaceName, memberName, file] -> do
            withFile file ReadMode $ \handle -> do
                fd <- handleToFd handle
                mcall <- methodCall <$> parseObjectPath objectPath <*> parseInterfaceName interfaceName <*> parseMemberName memberName
                bus <- parseBusName busName
                reply <-
                    call_
                        client
                        mcall
                            { methodCallDestination = Just bus
                            , methodCallBody = [toVariant fd]
                            }
                print $ methodReturnBody reply
        _ -> putStrLn "syntax: sendfd [busName] [objectPath] [interfaceName] [memberName] [file]"
