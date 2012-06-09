{-# LANGUAGE TemplateHaskell #-}

-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
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

module DBusTests.Message (test_Message) where

import           Test.Chell

import           DBus

test_Message :: Suite
test_Message = suite "Message"
	test_MethodErrorMessage

test_MethodErrorMessage :: Test
test_MethodErrorMessage = assertions "methodErrorMessage" $ do
	let emptyError = methodError firstSerial (errorName_ "com.example.Error")
	
	$expect (equal
		"(no error message)"
		(methodErrorMessage emptyError
			{ methodErrorBody = []
			}))
	$expect (equal
		"(no error message)"
		(methodErrorMessage emptyError
			{ methodErrorBody = [toVariant True]
			}))
	$expect (equal
		"(no error message)"
		(methodErrorMessage emptyError
			{ methodErrorBody = [toVariant ""]
			}))
	$expect (equal
		"error"
		(methodErrorMessage emptyError
			{ methodErrorBody = [toVariant "error"]
			}))
