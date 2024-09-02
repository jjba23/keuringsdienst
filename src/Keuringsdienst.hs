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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Keuringsdienst
  ( -- * Core types
    ValidationResult,
    ValidationRule (..),
    Validation (..),

    -- * Operators
    keuren,
    misschienKeuren,
    validate,
    maybeValidate,
    ofDitOfDat,
    orThisOrThat,
    (*||*),
    (|??|),
    (|?|),
  )
where

import Data.Aeson hiding (Success)
import Data.Text as T
import GHC.Generics

-- | Data type that represents the error messages. Currently is set to a simple Text,
--  futurely this could be expanded to contain more metadata.
type ErrMsg = Text

-- | Core data type representing the outcome of a validation, which can be a success,
--  or a failure with more information about the failure.
data Validation err
  = Success
  | Failure err
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | ValidationResult is the data type of choice for now, since we have chosen to implement
-- the library in a way that it will return a [Text] as context for validation failures.
type ValidationResult = Validation [ErrMsg]

instance Semigroup ValidationResult where
  (<>) a b = case a of
    Success -> case b of
      Success -> Success
      Failure errorsB -> Failure errorsB
    Failure errorsA -> case b of
      Success -> Failure errorsA
      Failure errorsB -> Failure (errorsA <> errorsB)

instance Monoid ValidationResult where
  mempty = Success

-- | Data type representing a composable validation rule to be applied on a certain data.
newtype ValidationRule a = ValidationRule
  { performValidation :: a -> ValidationResult
  }

instance Semigroup (ValidationRule x) where
  (<>) a b = do
    ValidationRule
      { performValidation = \value -> performValidation a value <> performValidation b value
      }

instance Monoid (ValidationRule a) where
  mempty = ValidationRule {performValidation = const Success}

-- | This function applies a validation rule to a value. Stands for validate/judge in Dutch.
keuren :: a -> ValidationRule a -> ValidationResult
keuren x rule = performValidation rule x

infixl 8 |?|

-- | keuren operator
(|?|) :: a -> ValidationRule a -> ValidationResult
(|?|) = keuren

-- | Alias to the keuren operator
validate :: a -> ValidationRule a -> ValidationResult
validate = keuren

-- | This function applies a validation rule to a value when it is a Just and
-- defaults to a Success in case of a Nothing. Stands for maybe validate/judge in Dutch.
misschienKeuren :: Maybe a -> ValidationRule a -> ValidationResult
misschienKeuren x rule = maybe Success (performValidation rule) x

infixl 8 |??|

-- | misschienKeuren operator
(|??|) :: Maybe a -> ValidationRule a -> ValidationResult
(|??|) = misschienKeuren

-- | Alias to the misschienKeuren operator
maybeValidate :: Maybe a -> ValidationRule a -> ValidationResult
maybeValidate = misschienKeuren

-- | If one of the validations has a successful result, then the validation is a success.
-- Stands for or this or that in Dutch.
ofDitOfDat :: ValidationRule a -> ValidationRule a -> ValidationRule a
ofDitOfDat rule1 rule2 = ValidationRule $ \actual ->
  case (performValidation rule1 actual, performValidation rule2 actual) of
    (Failure e1, Failure e2) -> Failure (e1 <> e2)
    (Success, _) -> Success
    (_, Success) -> Success

infixl 6 *||*

-- | ofDitOfDat operator
(*||*) :: ValidationRule a -> ValidationRule a -> ValidationRule a
(*||*) = ofDitOfDat

-- | Alias to the ofDitOfDat operator
orThisOrThat :: ValidationRule a -> ValidationRule a -> ValidationRule a
orThisOrThat = ofDitOfDat
