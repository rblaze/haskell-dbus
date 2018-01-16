-- Copyright (C) 2010-2012 John Millikin <john@john-millikin.com>
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

module DBusTests.Util
    ( assertVariant
    , assertValue
    , assertAtom
    , assertException
    , assertThrows

    , getTempPath
    , listenRandomUnixPath
    , listenRandomUnixAbstract
    , listenRandomIPv4
    , listenRandomIPv6
    , noIPv6
    , forkVar

    , withEnv
    , countFileDescriptors

    , dropWhileEnd

    , halfSized
    , clampedSize
    , smallListOf
    , smallListOf1

    , DBusTests.Util.requireLeft
    , DBusTests.Util.requireRight
    ) where

import Control.Concurrent
import Control.Exception (Exception, IOException, try, bracket, bracket_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource
import Data.Bits ((.&.))
import Data.Char (chr)
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.QuickCheck hiding ((.&.))
import Test.Tasty.HUnit
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Network as N
import qualified Network.Socket as NS
import qualified System.Posix as Posix

import DBus
import DBus.Internal.Types

assertVariant :: (Eq a, Show a, IsVariant a) => Type -> a -> Test.Tasty.HUnit.Assertion
assertVariant t a = do
    t @=? variantType (toVariant a)
    Just a @=? fromVariant (toVariant a)
    toVariant a @=? toVariant a

assertValue :: (Eq a, Show a, IsValue a) => Type -> a -> Test.Tasty.HUnit.Assertion
assertValue t a = do
    t @=? DBus.typeOf a
    t @=? DBus.Internal.Types.typeOf a
    t @=? valueType (toValue a)
    fromValue (toValue a) @?= Just a
    toValue a @=? toValue a
    assertVariant t a

assertAtom :: (Eq a, Show a, IsAtom a) => Type -> a -> Test.Tasty.HUnit.Assertion
assertAtom t a = do
    t @=? (atomType (toAtom a))
    fromAtom (toAtom a) @?= (Just a)
    toAtom a @=? toAtom a
    assertValue t a

getTempPath :: IO String
getTempPath = do
    tmp <- getTemporaryDirectory
    uuid <- randomUUID
    return (tmp </> formatUUID uuid)

listenRandomUnixPath :: MonadResource m => m Address
listenRandomUnixPath = do
    path <- liftIO getTempPath

    let sockAddr = NS.SockAddrUnix path
    (_, sock) <- allocate
        (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol)
        N.sClose
    liftIO (NS.bind sock sockAddr)
    liftIO (NS.listen sock 1)
    _ <- register (removeFile path)

    let Just addr = address "unix" (Map.fromList
            [ ("path", path)
            ])
    return addr

listenRandomUnixAbstract :: MonadResource m => m (Address, ReleaseKey)
listenRandomUnixAbstract = do
    uuid <- liftIO randomUUID
    let sockAddr = NS.SockAddrUnix ('\x00' : formatUUID uuid)

    (key, sock) <- allocate
        (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol)
        N.sClose

    liftIO $ NS.bind sock sockAddr
    liftIO $ NS.listen sock 1

    let Just addr = address "unix" (Map.fromList
            [ ("abstract", formatUUID uuid)
            ])
    return (addr, key)

listenRandomIPv4 :: MonadResource m => m (Address, N.Socket, ReleaseKey)
listenRandomIPv4 = do
    hostAddr <- liftIO $ NS.inet_addr "127.0.0.1"
    let sockAddr = NS.SockAddrInet 0 hostAddr

    (key, sock) <- allocate
        (NS.socket NS.AF_INET NS.Stream NS.defaultProtocol)
        N.sClose
    liftIO $ NS.bind sock sockAddr
    liftIO $ NS.listen sock 1

    sockPort <- liftIO $ NS.socketPort sock
    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv4")
            , ("host", "localhost")
            , ("port", show (toInteger sockPort))
            ])
    return (addr, sock, key)

listenRandomIPv6 :: MonadResource m => m Address
listenRandomIPv6 = do
    addrs <- liftIO $ NS.getAddrInfo Nothing (Just "::1") Nothing
    let sockAddr = case addrs of
            [] -> error "listenRandomIPv6: no address for localhost?"
            a:_ -> NS.addrAddress a

    (_, sock) <- allocate
        (NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol)
        N.sClose
    liftIO $ NS.bind sock sockAddr
    liftIO $ NS.listen sock 1

    sockPort <- liftIO $ NS.socketPort sock
    let Just addr = address "tcp" (Map.fromList
            [ ("family", "ipv6")
            , ("host", "::1")
            , ("port", show (toInteger sockPort))
            ])
    return addr

noIPv6 :: IO Bool
noIPv6 = do
    tried <- try (NS.getAddrInfo Nothing (Just "::1") Nothing)
    case (tried :: Either IOException [NS.AddrInfo]) of
        Left _ -> return True
        Right addrs -> return (null addrs)

forkVar :: MonadIO m => IO a -> m (MVar a)
forkVar io = liftIO $ do
    var <- newEmptyMVar
    _ <- forkIO (io >>= putMVar var)
    return var

withEnv :: MonadIO m => String -> Maybe String -> IO a -> m a
withEnv name value io = liftIO $ do
    let set val = case val of
            Just x -> Posix.setEnv name x True
            Nothing -> Posix.unsetEnv name
    old <- Posix.getEnv name
    bracket_ (set value) (set old) io

countFileDescriptors :: MonadIO m => m Int
countFileDescriptors = liftIO io where
    io = do
        pid <- Posix.getProcessID
        let fdDir = "/proc/" ++ show pid ++ "/fd"
        bracket (Posix.openDirStream fdDir) Posix.closeDirStream countDirEntries
    countDirEntries dir = loop 0 where
        loop n = do
            name <- Posix.readDirStream dir
            if null name
                then return n
                else loop (n + 1)

halfSized :: Gen a -> Gen a
halfSized gen = sized (\n -> if n > 0
    then resize (div n 2) gen
    else gen)

smallListOf :: Gen a -> Gen [a]
smallListOf gen = clampedSize 10 (listOf gen)

smallListOf1 :: Gen a -> Gen [a]
smallListOf1 gen = clampedSize 10 (listOf1 gen)

clampedSize :: Int -> Gen a -> Gen a
clampedSize maxN gen = sized (\n -> resize (min n maxN) gen)

instance Arbitrary T.Text where
    arbitrary = fmap T.pack genUnicode

genUnicode :: Gen [Char]
genUnicode = string where
    string = sized $ \n -> do
        k <- choose (0,n)
        sequence [ char | _ <- [1..k] ]

    excluding :: [a -> Bool] -> Gen a -> Gen a
    excluding bad gen = loop where
        loop = do
            x <- gen
            if or (map ($ x) bad)
                then loop
                else return x

    reserved = [lowSurrogate, highSurrogate, noncharacter]
    lowSurrogate c = c >= 0xDC00 && c <= 0xDFFF
    highSurrogate c = c >= 0xD800 && c <= 0xDBFF
    noncharacter c = masked == 0xFFFE || masked == 0xFFFF where
        masked = c .&. 0xFFFF

    ascii = choose (0x20, 0x7F)
    plane0 = choose (0xF0, 0xFFFF)
    plane1 = oneof [ choose (0x10000, 0x10FFF)
                   , choose (0x11000, 0x11FFF)
                   , choose (0x12000, 0x12FFF)
                   , choose (0x13000, 0x13FFF)
                   , choose (0x1D000, 0x1DFFF)
                   , choose (0x1F000, 0x1FFFF)
                   ]
    plane2 = oneof [ choose (0x20000, 0x20FFF)
                   , choose (0x21000, 0x21FFF)
                   , choose (0x22000, 0x22FFF)
                   , choose (0x23000, 0x23FFF)
                   , choose (0x24000, 0x24FFF)
                   , choose (0x25000, 0x25FFF)
                   , choose (0x26000, 0x26FFF)
                   , choose (0x27000, 0x27FFF)
                   , choose (0x28000, 0x28FFF)
                   , choose (0x29000, 0x29FFF)
                   , choose (0x2A000, 0x2AFFF)
                   , choose (0x2B000, 0x2BFFF)
                   , choose (0x2F000, 0x2FFFF)
                   ]
    plane14 = choose (0xE0000, 0xE0FFF)
    planes = [ascii, plane0, plane1, plane2, plane14]

    char = chr `fmap` excluding reserved (oneof planes)

instance Arbitrary Data.ByteString.ByteString where
    arbitrary = fmap Data.ByteString.pack arbitrary

instance Arbitrary Data.ByteString.Lazy.ByteString where
    arbitrary = fmap Data.ByteString.Lazy.fromChunks arbitrary

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = T.unpack . T.dropWhileEnd p . T.pack

requireLeft :: Show b => Either a b -> IO a
requireLeft (Left a) = return a
requireLeft (Right b) = assertFailure ("Right " ++ show b ++ " is not Left") >> undefined

requireRight :: Show a => Either a b -> IO b
requireRight (Right b) = return b
requireRight (Left a) = assertFailure ("Left " ++ show a ++ " is not Right") >> undefined

assertException :: (Eq e, Exception e) => e -> IO a -> Test.Tasty.HUnit.Assertion
assertException e f = do
    result <- try f
    case result of
        Left ex -> ex @?= e
        Right _ -> assertFailure "expected exception not thrown"

assertThrows :: Exception e => (e -> Bool) -> IO a -> Test.Tasty.HUnit.Assertion
assertThrows check f = do
    result <- try f
    case result of
        Left ex -> assertBool ("unexpected exception " ++ show ex) (check ex)
        Right _ -> assertFailure "expected exception not thrown"
