{-# LANGUAGE ViewPatterns #-}

module ViewPatterns.ViewPatterns_OCharles where

import qualified Data.Map.Strict as M
import           Data.Sequence

{-
24 Days of GHC Extensions: View Patterns

binding extensions : top-level defs, where/let bindings

ViewPatterns : enable pattern matching on result of function application

example: num downloads for lens library
-}
{-# ANN downloadsFor0 "HLint: ignore Use fromMaybe" #-}
downloadsFor0 :: String -> M.Map String Int -> Int
downloadsFor0 pkg packages =
  case M.lookup pkg packages of
    Just n  -> n
    Nothing -> 0

downloadsFor :: String -> M.Map String Int -> Int
downloadsFor pkg (M.lookup pkg -> Just downloads) = downloads
downloadsFor _   _                                = 0
{-
view pattern defined by two parts
- the view : partially applied function
- the pattern match to perform function application result

------------------------------------------------------------------------------
View Patterns as a Tool for Abstraction

key benefit : view a data type as a def that is easy to pattern match on,
              while using a different type for underlying rep

example : finger tree

-- has poor perfomance
data List a = Nil | Cons a (List a)

Seq uses a finger tree to get better perfomance

but the def is abstract - instead functions are provided to access - but can't pattern match

so use ViewPatterns

example analysing a time series : list of data points
view last data point - if exists

last :: Seq a -> Maybe a
last ?? = Nothing
last ?? = Just _

cannot pattern match directly on a Seq
can view as list from right

data ViewR a = EmptyR | (Seq a) :> a

viewr :: Seq a -> ViewR a

ViewR similar to linked list
-}
last :: Seq a -> Maybe a
last (viewr -> _ :> x) = Just x
last (viewr -> EmptyR) = Nothing
last (viewr -> _)      = Nothing -- non exhautive pattern match without this
{-
balance overhead of new syntax against productivity gains
-}
