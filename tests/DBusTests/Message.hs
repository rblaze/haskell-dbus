-- Copyright (C) 2012 John Millikin <john@john-millikin.com>
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module DBusTests.Message (test_Message) where

import Test.Tasty
import Test.Tasty.HUnit

import           DBus

test_Message :: TestTree
test_Message = testGroup "Message"
    [ test_MethodErrorMessage
    ]

test_MethodErrorMessage :: TestTree
test_MethodErrorMessage = testCase "methodErrorMessage" $ do
    let emptyError = methodError firstSerial (errorName_ "com.example.Error")

    "(no error message)" @=?
        methodErrorMessage emptyError
            { methodErrorBody = []
            }
    "(no error message)" @=?
        methodErrorMessage emptyError
            { methodErrorBody = [toVariant True]
            }
    "(no error message)" @=?
        methodErrorMessage emptyError
            { methodErrorBody = [toVariant ""]
            }
    "error" @=?
        methodErrorMessage emptyError
            { methodErrorBody = [toVariant "error"]
            }
