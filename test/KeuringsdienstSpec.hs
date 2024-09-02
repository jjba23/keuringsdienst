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

module KeuringsdienstSpec where

import Data.Text
import Keuringsdienst as K
import Test.HUnit

keuringsdienstSpec :: Test
keuringsdienstSpec =
  TestList
    [ context "validation results can be composed if both success" validationResultsCanBeComposedIfBothSuccess,
      context "validation results can be composed if success failure" validationResultsCanBeComposedIfSuccessFailure,
      context "validation results can be composed if failure success" validationResultsCanBeComposedIfFailureSuccess,
      context "keuren operator applies validation" keurenOperatorAppliesValidation,
      context "non infix keuren operator applies validation" nonInfixKeurenOperatorAppliesValidation,
      context "non infix validate operator applies validation" nonInfixValidateOperatorAppliesValidation,
      context "misschien keuren operator applies validation" misschienKeurenOperatorAppliesValidation,
      context "non infix misschien keuren operator applies validation" nonInfixMisschienKeurenOperatorAppliesValidation,
      context "non infix maybe validate operator applies validation" nonInfixMaybeValidateOperatorAppliesValidation,
      context "misschien keuren operator applies validation on nothing" misschienKeurenOperatorAppliesValidationOnNothing,
      context "non infix misschien keuren operator applies validation on nothing" nonInfixMisschienKeurenOperatorAppliesValidationOnNothing,
      context "non infix maybe validate operator applies validation on nothing" nonInfixMaybeValidateOperatorAppliesValidationOnNothing
    ]

context :: String -> Test -> Test
context = TestLabel

someErrorMessage :: Text
someErrorMessage = "You're simply a test! Better than all the rest!"

alwaysSuccessfulValidationRule :: ValidationRule a
alwaysSuccessfulValidationRule = ValidationRule {performValidation = const K.Success}

alwaysFailureValidationRule :: ValidationRule a
alwaysFailureValidationRule = ValidationRule {performValidation = const (K.Failure [someErrorMessage])}

validationResultsCanBeComposedIfBothSuccess :: Test
validationResultsCanBeComposedIfBothSuccess =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = K.Success <> K.Success

validationResultsCanBeComposedIfSuccessFailure :: Test
validationResultsCanBeComposedIfSuccessFailure =
  TestCase (assertEqual "" expected systemUnderTest)
  where
    expected :: ValidationResult
    expected = K.Failure [someErrorMessage]
    systemUnderTest :: ValidationResult
    systemUnderTest = K.Success <> K.Failure [someErrorMessage]

validationResultsCanBeComposedIfFailureSuccess :: Test
validationResultsCanBeComposedIfFailureSuccess =
  TestCase (assertEqual "" expected systemUnderTest)
  where
    expected :: ValidationResult
    expected = K.Failure [someErrorMessage]
    systemUnderTest :: ValidationResult
    systemUnderTest = K.Failure [someErrorMessage] <> K.Success

keurenOperatorAppliesValidation :: Test
keurenOperatorAppliesValidation =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = ("" :: Text) |?| alwaysSuccessfulValidationRule

nonInfixKeurenOperatorAppliesValidation :: Test
nonInfixKeurenOperatorAppliesValidation =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = keuren ("" :: Text) alwaysSuccessfulValidationRule

nonInfixValidateOperatorAppliesValidation :: Test
nonInfixValidateOperatorAppliesValidation =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = validate ("" :: Text) alwaysSuccessfulValidationRule

misschienKeurenOperatorAppliesValidation :: Test
misschienKeurenOperatorAppliesValidation =
  TestCase (assertEqual "" expected systemUnderTest)
  where
    expected :: ValidationResult
    expected = K.Failure [someErrorMessage]
    systemUnderTest :: ValidationResult
    systemUnderTest = (Just "" :: Maybe Text) |??| alwaysFailureValidationRule

nonInfixMisschienKeurenOperatorAppliesValidation :: Test
nonInfixMisschienKeurenOperatorAppliesValidation =
  TestCase (assertEqual "" expected systemUnderTest)
  where
    expected :: ValidationResult
    expected = K.Failure [someErrorMessage]
    systemUnderTest :: ValidationResult
    systemUnderTest = misschienKeuren (Just "" :: Maybe Text) alwaysFailureValidationRule

nonInfixMaybeValidateOperatorAppliesValidation :: Test
nonInfixMaybeValidateOperatorAppliesValidation =
  TestCase (assertEqual "" expected systemUnderTest)
  where
    expected :: ValidationResult
    expected = K.Failure [someErrorMessage]
    systemUnderTest :: ValidationResult
    systemUnderTest = maybeValidate (Just "" :: Maybe Text) alwaysFailureValidationRule

misschienKeurenOperatorAppliesValidationOnNothing :: Test
misschienKeurenOperatorAppliesValidationOnNothing =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = (Nothing :: Maybe Text) |??| alwaysSuccessfulValidationRule

nonInfixMisschienKeurenOperatorAppliesValidationOnNothing :: Test
nonInfixMisschienKeurenOperatorAppliesValidationOnNothing =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = misschienKeuren (Nothing :: Maybe Text) alwaysSuccessfulValidationRule

nonInfixMaybeValidateOperatorAppliesValidationOnNothing :: Test
nonInfixMaybeValidateOperatorAppliesValidationOnNothing =
  TestCase (assertEqual "" K.Success systemUnderTest)
  where
    systemUnderTest :: ValidationResult
    systemUnderTest = maybeValidate (Nothing :: Maybe Text) alwaysSuccessfulValidationRule
