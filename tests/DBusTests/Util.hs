{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2012 John Millikin <jmillikin@gmail.com>
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
	) where

import           Control.Concurrent
import           Control.Exception (IOException, try, bracket, bracket_)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Bits ((.&.))
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import           Data.Char (chr)
import qualified Data.Map as Map
import qualified Data.Set
import qualified Data.Text as T
import qualified Network as N
import qualified Network.Socket as NS
import           System.Directory (getTemporaryDirectory, removeFile)
import qualified System.Posix as Posix
import           System.FilePath ((</>))

import           Test.Chell
import           Test.QuickCheck hiding ((.&.))

import           DBus
import           DBus.Types

assertVariant :: (Eq a, Show a, IsVariant a) => Type -> a -> Assertions ()
assertVariant t a = do
	$expect $ equal t (variantType (toVariant a))
	$expect $ equal (fromVariant (toVariant a)) (Just a)
	$expect $ equal (toVariant a) (toVariant a)

$([d||])

assertValue :: (Eq a, Show a, IsValue a) => Type -> a -> Assertions ()
assertValue t a = do
	$expect $ equal t (DBus.typeOf a)
	$expect $ equal t (DBus.Types.typeOf a)
	$expect $ equal t (valueType (toValue a))
	$expect $ equal (fromValue (toValue a)) (Just a)
	$expect $ equal (toValue a) (toValue a)
	assertVariant t a

$([d||])

assertAtom :: (Eq a, Show a, IsAtom a) => Type -> a -> Assertions ()
assertAtom t a = do
	$expect $ equal t (atomType (toAtom a))
	$expect $ equal (fromAtom (toAtom a)) (Just a)
	$expect $ equal (toAtom a) (toAtom a)
	assertValue t a

getTempPath :: IO String
getTempPath = do
	tmp <- getTemporaryDirectory
	uuid <- randomUUID
	return (tmp </> formatUUID uuid)

listenRandomUnixPath :: Assertions (Address, N.Socket)
listenRandomUnixPath = do
	path <- liftIO getTempPath
	
	let sockAddr = NS.SockAddrUnix path
	sock <- liftIO (NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol)
	liftIO (NS.bindSocket sock sockAddr)
	liftIO (NS.listen sock 1)
	afterTest (removeFile path)
	
	let Just addr = address "unix" (Map.fromList
		[ ("path", path)
		])
	return (addr, sock)

listenRandomUnixAbstract :: MonadIO m => m (Address, N.Socket)
listenRandomUnixAbstract = liftIO $ do
	uuid <- liftIO randomUUID
	let sockAddr = NS.SockAddrUnix ('\x00' : formatUUID uuid)
	
	sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
	NS.bindSocket sock sockAddr
	NS.listen sock 1
	
	let Just addr = address "unix" (Map.fromList
		[ ("abstract", formatUUID uuid)
		])
	return (addr, sock)

listenRandomIPv4 :: MonadIO m => m (Address, N.Socket)
listenRandomIPv4 = liftIO $ do
	hostAddr <- NS.inet_addr "127.0.0.1"
	let sockAddr = NS.SockAddrInet 0 hostAddr
	
	sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
	NS.bindSocket sock sockAddr
	NS.listen sock 1
	
	sockPort <- NS.socketPort sock
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv4")
		, ("host", "localhost")
		, ("port", show (toInteger sockPort))
		])
	return (addr, sock)

listenRandomIPv6 :: MonadIO m => m (Address, N.Socket)
listenRandomIPv6 = liftIO $ do
	addrs <- NS.getAddrInfo Nothing (Just "::1") Nothing
	let sockAddr = case addrs of
		[] -> error "listenRandomIPv6: no address for localhost?"
		a:_ -> NS.addrAddress a
	
	sock <- NS.socket NS.AF_INET6 NS.Stream NS.defaultProtocol
	NS.bindSocket sock sockAddr
	NS.listen sock 1
	
	sockPort <- NS.socketPort sock
	let Just addr = address "tcp" (Map.fromList
		[ ("family", "ipv6")
		, ("host", "localhost")
		, ("port", show (toInteger sockPort))
		])
	return (addr, sock)

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

instance (Arbitrary a, Ord a) => Arbitrary (Data.Set.Set a) where
	arbitrary = fmap Data.Set.fromList arbitrary

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
