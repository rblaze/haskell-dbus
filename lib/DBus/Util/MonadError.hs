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

module DBus.Util.MonadError
	( ErrorM (..)
	, ErrorT (..)
	, throwErrorM
	, throwErrorT
	) where

newtype ErrorM e a = ErrorM { runErrorM :: Either e a }

instance Functor (ErrorM e) where
	fmap f m = ErrorM $ case runErrorM m of
		Left err -> Left err
		Right x -> Right $ f x

instance Monad (ErrorM e) where
	return = ErrorM . Right
	(>>=) m k = case runErrorM m of
		Left err -> ErrorM $ Left err
		Right x -> k x

throwErrorM :: e -> ErrorM e a
throwErrorM = ErrorM . Left

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

instance Monad m => Monad (ErrorT e m) where
	return = ErrorT . return . Right
	(>>=) m k = ErrorT $ do
		x <- runErrorT m
		case x of
			Left l -> return $ Left l
			Right r -> runErrorT $ k r

throwErrorT :: Monad m => e -> ErrorT e m a
throwErrorT = ErrorT . return . Left
