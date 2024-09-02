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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Keuringsdienst.Helpers
  ( filterFailedValidations,
    isEqualTo,
    isLesserThan,
    isNegative,
    isNegativeOrZero,
    isNonEmptyText,
    isPositive,
    isPositiveOrZero,
    isTextOfLength,
    isTextSmallerThan,
    isTextSmallerThanOrEqual,
    isNotEqualTo,
  )
where

import Data.Map as Map
import Data.Text as T
import Keuringsdienst as K

-- | Validate that a value is equal to another.
isEqualTo :: (Show a, Eq a) => a -> ValidationRule a
isEqualTo value = ValidationRule $ \actual ->
  if actual == value
    then Success
    else
      Failure
        [pack $ "Expected " <> show actual <> " to equal " <> show value]

-- | Validate that a value is different to another.
isNotEqualTo :: (Show a, Eq a) => a -> ValidationRule a
isNotEqualTo value = ValidationRule $ \actual ->
  if actual /= value
    then Success
    else
      Failure
        [pack $ "Expected " <> show actual <> " to not equal " <> show value]

-- | Validate that a value is greater than another.
isGreaterThan :: (Show a, Ord a) => a -> ValidationRule a
isGreaterThan ruleValue = ValidationRule $ \actual ->
  if actual > ruleValue
    then Success
    else
      Failure
        [ pack (show actual <> " was expected to be greater than " <> show ruleValue)
        ]

-- | Validate that a value is lesser than another.
isLesserThan :: (Show a, Ord a) => a -> ValidationRule a
isLesserThan ruleValue = ValidationRule $ \actual ->
  if actual < ruleValue
    then Success
    else
      Failure
        [ pack (show actual <> " was expected to be lesser than " <> show ruleValue)
        ]

-- | Validate that a value is a non empty @Text@.
isNonEmptyText :: ValidationRule Text
isNonEmptyText = ValidationRule $ \actual ->
  if T.null actual
    then Failure [T.pack "Text was expected to be non-empty"]
    else Success

-- | Validate that a value is a @Text@ of certain length.
isTextOfLength :: Int -> ValidationRule Text
isTextOfLength ruleValue = ValidationRule $ \actual -> do
  let actualTextLength = T.length actual
  if actualTextLength /= ruleValue
    then
      Failure
        [ T.pack
            ( "Text was expected to be of size "
                <> show ruleValue
                <> " but was "
                <> show actualTextLength
            )
        ]
    else Success

-- | Validate that a value is a @Text@ of length smaller than n.
isTextSmallerThan :: Int -> ValidationRule Text
isTextSmallerThan ruleValue = ValidationRule $ \actual -> do
  let actualTextLength = T.length actual
  if actualTextLength >= ruleValue
    then
      Failure
        [ T.pack
            ( "Text was expected to be smaller than "
                <> show ruleValue
                <> " but was "
                <> show actualTextLength
            )
        ]
    else Success

-- | Validate that a value is a @Text@ of length smaller or equal to n.
isTextSmallerThanOrEqual :: Int -> ValidationRule Text
isTextSmallerThanOrEqual ruleValue = ValidationRule $ \actual -> do
  let actualTextLength = T.length actual
  if actualTextLength > ruleValue
    then
      Failure
        [ T.pack
            ( "Text was expected to be smaller than "
                <> show ruleValue
                <> " but was "
                <> show actualTextLength
            )
        ]
    else Success

-- | Validate that a value is lesser than 0.
isNegative :: ValidationRule Int
isNegative = isLesserThan 0

-- | Validate that a value is greater than 0.
isPositive :: ValidationRule Int
isPositive = isGreaterThan 0

-- | Validate that a value is positive or zero.
isPositiveOrZero :: ValidationRule Int
isPositiveOrZero = isPositive *||* isEqualTo 0

-- | Validate that a value is negative or zero.
isNegativeOrZero :: ValidationRule Int
isNegativeOrZero = isNegative *||* isEqualTo 0

-- | Filter a Map of @Validation err@ and keep only the failures.
filterFailedValidations :: Map Text (Validation err) -> Map Text (Validation err)
filterFailedValidations =
  Map.filter
    ( \x -> do
        case x of
          Failure _ -> True
          Success -> False
    )
