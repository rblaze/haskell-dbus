{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2010-2011 John Millikin <jmillikin@gmail.com>
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

module DBus.Tests.Util where

import           Test.Chell

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

