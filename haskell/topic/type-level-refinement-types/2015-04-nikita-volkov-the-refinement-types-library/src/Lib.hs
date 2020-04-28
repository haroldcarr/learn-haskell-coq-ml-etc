{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Lib where

import           Control.Monad
import           Data.Char
import           Data.Function             ((&))
import qualified Data.Text.Prettyprint.Doc as PP
import           Data.Typeable
import           Refined
{-
https://nikita-volkov.github.io//refined/
https://hackage.haskell.org/package/refined

Nikita Volkov
Announcing the refinement types library
April 27, 2015

examples
- numeric type with a zero to hundred range to describe percentage
- zero to one to describe a proper fraction of something
- positive integer (without the zero) to enumerate something
- vector of a specific length
-}
-- slice a list into chunks of a given length
-- given a negative or zero : generates infinite list of empty lists
-- given a postive : slices then generates infinite list of empty lists
slice0 :: Int -> [a] -> [[a]]
slice0 n l =
  splitAt n l & \(a, b) -> a : slice0 n b

-- 'error' antipattern
slice1 :: Int -> [a] -> [[a]]
slice1 n =
  if n >= 1
    then
      let slice' l = splitAt n l & \(a, b) -> a : slice' b
       in slice'
    else error $ "Invalid `n`: " <> show n

-- check the 'n' parameter and replace invalid values with a default one
-- but masks potential upstream error
slice2 :: Int -> [a] -> [[a]]
slice2 n =
  slice'
  where
    n' = if n >= 1 then n else 1
    slice' l = splitAt n' l & \(a, b) -> a : slice' b

-- use 'Maybe'
slice3 :: Int -> [a] -> Maybe [[a]]
slice3 n l0 =
  if n >= 1
    then
      let slice' l = splitAt n l & \(a, b) -> a : slice' b
       in Just $ slice' l0
    else Nothing

-- Nikita's preferred
slice4 :: Int -> Maybe ([a] -> [[a]])
slice4 n =
  if n >= 1
    then
      let slice' l = splitAt n l & \(a, b) -> a : slice' b
       in Just slice'
    else Nothing
{-
no separation of concerns: all of above conflat slicing and input validation

Smart constructors
-}
newtype PositiveInt = PositiveInt { positiveIntValue :: Int }

positiveInt :: Int -> Maybe PositiveInt
positiveInt n = if n > 0 then Just $ PositiveInt n else Nothing

slice5 :: PositiveInt -> [a] -> [[a]]
slice5 n l =
  splitAt (positiveIntValue n) l & \(a, b) -> a : slice5 n b
{-
API should not export PositiveInt       constructor.
API should     export positiveInt smart constructor and positiveIntValue extractor

Problems of smart constructors
- redundant runtime validation when using hard-coded input values
- smart Constructor is a pattern : needs abstraction

solution: refined
-}
slice6 :: Refined Positive Int -> [a] -> [[a]]
slice6 n l =
  splitAt (unrefine n) l & \(a, b) -> a : slice6 n b
{-
Refined : type that "refines" another type with a type-level predicate
Positive : predicate

Predicates are composable with the predefined logical predicates:
-}
type ProperFraction = Refined (And (Not (LessThan 0)) (Not (GreaterThan 1))) Double
{-
constructing values : both use Predicate type-class to validate value
- refine    : runtime (equivalent to a smart constructor)
- refineTH  : compiletime (using Template Haskell)
-}
sliceToTriplets :: [a] -> [[a]]
sliceToTriplets = slice6 $$(refineTH 3)
{-# ANN sliceToTriplets ("HLint: ignore Redundant bracket" :: String) #-}
{-
A predicate is a type, which requires no value-level implementation.
Requires instances of the Predicate type-class.

E.g., ensures String is lower-case
-}
data LowerCase

instance Predicate LowerCase String where
  validate p value = unless (all isLower value)
    (throwRefineOtherException (typeOf p)
     ("Not all chars are lower-case: '" <> PP.pretty value <> "'"))

type LowerCaseString = Refined LowerCase String

aaa :: Either RefineException LowerCaseString
aaa  = refine "aaa"

bbb :: Either RefineException LowerCaseString
bbb  = refine "bBv"
