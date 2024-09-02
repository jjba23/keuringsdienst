{-
  Copyright © 2023 Josep Bigorra

  This file is part of Keuringsdienst.
  Keuringsdienst is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation,
    either version 3 of the License, or (at your option) any later version.

  Keuringsdienst is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
    without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  See the GNU General Public License for more details.
  You should have received a copy of the GNU General Public License along with Keuringsdienst.
  If not, see <https://www.gnu.org/licenses/>.
-}

module Keuringsdienst.HelpersSpec where

import Data.Text as T
import Keuringsdienst as K
import Keuringsdienst.Helpers as KH
import Test.HUnit

keuringsdienstHelpersSpec :: Test
keuringsdienstHelpersSpec =
  TestList
    [ context "is equal to rule will succeed with text" isEqualToRuleWillSucceedWithText,
      context "is equal to rule will fail with text" isEqualToRuleWillFailWithText,
      context "is not equal to rule will succeed with text" isNotEqualToRuleWillSucceedWithText,
      context "is not equal to rule will fail with text" isNotEqualToRuleWillFailWithText
    ]

context :: String -> Test -> Test
context = TestLabel

someText :: Text
someText = "Keuringsdienst van Waarde"

isEqualToRuleWillSucceedWithText :: Test
isEqualToRuleWillSucceedWithText =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = someText |?| isEqualTo someText

isEqualToRuleWillFailWithText :: Test
isEqualToRuleWillFailWithText =
  case systemUnderTest of
    K.Failure _ -> TestCase (assertBool "" True)
    K.Success -> error "Expected rule validation to return a failure"
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = T.take 1 someText |?| isEqualTo someText

isNotEqualToRuleWillSucceedWithText :: Test
isNotEqualToRuleWillSucceedWithText =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = T.take 1 someText |?| isNotEqualTo someText

isNotEqualToRuleWillFailWithText :: Test
isNotEqualToRuleWillFailWithText =
  case systemUnderTest of
    K.Failure _ -> TestCase (assertBool "" True)
    K.Success -> error "Expected rule validation to return a failure"
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = someText |?| isNotEqualTo someText
