{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Lib where

import           Data.Coerce
import qualified Data.List       as L
import qualified Data.List.Utils as U

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
Ghosts of Departed Proofs (Functional Pearl) - GDP
Matt Noonan

------------------------------------------------------------------------------
a type system provides a mechanism for enforcing program invariants at compile time

e.g.,

ST monad
- enables pure computations to use local mutable state
- phantom type parameter and ank-2 types gives compile-time guarantee
  that that state is invisible from the outside
------------------------------------------------------------------------------

GDP
- Properties and proofs are represented in code.
  - Proofs are entities in host language : can be analyzed
  - propositions are represented by types
  - proof of proposition is a value of that type
- Proofs carried by phantom type parameters
  - phantom type param is mechanism for giving proof to the library API
  - phantom type variables attached to newtype wrappers
  - newtype wrapper erased during compilation
    - no run-time cost
    - proofs do not exists in executable
- Library-controlled APIs to create proofs
  - only library able to create domain proofs/axioms
  - e.g., exporting functions that
    - create values with known properties
    - classify a value into mutually disjoint refinements
    - introduce existentially quantified properties
      - e.g., name in TODO, runSt in TODO, or withMap in TODO
- Library exports combinators for manipulating proofs
  - user can use with evidence at hand to produce a proof of a safety property
  - resulting proof communicated to library

------------------------------------------------------------------------------
Safe Coercions

some examples rely on safe coercions
- type T
    and
  newtype N = N T
    have same runtime rep
- coerce :: Coercible a b => a -> b
    is a zero-cost cast from a to b
-   If N is a newtype of T, then constraints
  Coercible N T
  Coercible T N
    hold in any module where constructor of N is visible

------------------------------------------------------------------------------
Case Study : Sorted Lists

User must guarantee.  Not expressed in types:

sortBy  :: (a -> a -> Ordering) -> [a] -> [a]
-- Usage constraint: in `mergeBy comp xs ys`
-- input lists `xs` and `ys` should also be sorted by the same comparator
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]

What if User has a proof that input lists are sorted properly?

2.1 Conjuring a Name

how to express the idea of two comparators being "the same"?
- newtype wrapper with phantom type parameter NAME
- written :  a ~~ n
  - "values of type 'a' with name 'n'
- impl: newtype around a, with phantom type param n
-}

-- module for attaching names to values:

-- module Named (Named, type (~~), name) where
-- - hides constructor of Named
--   ensures that name is the only way to introduce a name for a value

-- import Data.Coerce

-- key feature : exported `name` funcation expresses "any value can be given a name"
newtype Named name a = Named a
type a ~~ name = Named name a

{-
to emulate existentially-quantified type
- instead of directly returning a value with a name attached
- name says to user "tell me what you want to do with that named value, and I’ll do it for you"
- user passes a computation that is agnostic about the name that will be chosen
- See TODO
-}
-- means: ype of `name`    is     a -> (exists name. (a ~~ name))
-- rank-2 type of name, uses a polymorphic continuation (emulates an existential type)
name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

-- module The where

-- to remove names and other phantom data from a value
class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a -- ensures no runtime cost
  the = coerce

-- most instances declared with an empty body
instance The (a ~~ name) a

--------------------------------------------------
{-
2.2 Safe API for sorting/merging

-- module Sorted (Named, SortedBy, sortBy, mergeBy) where
--
-- import           The
-- import           Named

-- import           Data.Coerce
-- import qualified Data.List       as L
-- import qualified Data.List.Utils as U
-}

-- refinement represents the predicate "x has been sorted by the comparator named comp"
-- wrapper’s meaning is the type of sortBy
-- - takes a named comparator and a list
-- - produces a list that has been SortedBy comp
-- do NOT export SortedBy’s constructor
-- - ensures the only way to obtain a value of type `SortedBy comp [a]` is via
-- `sortBy` or `mergeBy`
newtype SortedBy comp a = SortedBy a
instance The (SortedBy comp a) a

-- `the` coerces away the name of the comparator
-- apply sortBy from Data.List
-- introduce `SortedBy comp` predicate by coercing result
-- coercions erased so this is just a call to Data.List.sortBy
sortBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> [a]
  -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

-- after erasure, this will be just a call U.mergeBy
-- user must provide
-- - a named comparator
-- - two lists sorted by same comparator
mergeBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> SortedBy comp [a]
  -> SortedBy comp [a]
  -> SortedBy comp [a]
mergeBy comp xs ys =
  coerce (U.mergeBy (the comp) (the xs) (the ys))

