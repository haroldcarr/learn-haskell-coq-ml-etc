{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeOperators          #-}

-- needed (despite what hlint says):
{-# LANGUAGE TemplateHaskell        #-}

module Lib where

import           Control.Applicative
import           Control.Arrow       (left)
import           Control.Natural
import           Data.Validation
import           Refined
{-
https://gvolpe.github.io/blog/parallel-typeclass-for-haskell/
https://github.com/gvolpe/types-matter

Parallel typeclass for Haskell
Apr 20, 2020 by Gabriel Volpe

https://hackage.haskell.org/package/refined
https://hackage.haskell.org/package/liquidhaskell

Refinement types give the ability to
- define validation rules (aka "predicates") at the type level
-}
type Age  = Refined (GreaterThan 17) Int
type Name = Refined NonEmpty String

data Person = Person
  { personAge  :: Age
  , personName :: Name
  } deriving Show

-- validate reation of Person at compile-time using Template Haskell:

me :: Person
me  = Person $$(refineTH 32)
             $$(refineTH "Gabriel")
{-# ANN me ("HLint: ignore Redundant bracket" :: String) #-}
{-
If age < 18, or name empty, then compile error.

To do runtime validation Refined provides functions (i.e., replacing smart constructors).

refine :: Predicate p x => x -> Either RefineException (Refined p x)
-}
mkPerson0 :: Int -> String -> Either RefineException Person
mkPerson0 a n = Person <$> refine a <*> refine n
{-
Above stops on first error.

To validate all inputs in parallel and accumulates errors convert Either values into Validation.
-}

{-
mkPerson :: Int -> String -> Either RefineException Person
mkPerson a n = toEither $ Person <$> fromEither (refine a) <*> fromEither (refine n)
    • No instance for (Semigroup RefineException)
-}

{-
Above is clunky and repetitive.

Use the "Parallel" typeclass to improve.
Defines relationship between a Monad that can also be an Applicative with "parallely" behavior.
(Note: that means an Applicative instance that will not pass the monadic laws.)
-}
class (Monad m, Applicative f)
    => Parallel f m | m -> f
                    , f -> m where
  parallel   :: m :~> f
  sequential :: f :~> m

-- Either and Validation are isomorphic, except Validation's Applicative instance accumulate errors.
instance Semigroup e => Parallel (Validation e) (Either e) where
  parallel   = NT fromEither
  sequential = NT toEither

-- Relationship between [] and ZipList:
instance Parallel ZipList [] where
  parallel   = NT ZipList
  sequential = NT getZipList

-- parMapN (parMap2 here, but should be abstracted over its arity).
parMapN
  :: (Applicative f, Parallel f m)
  => m a0
  -> m a1
  -> (a0 -> a1 -> a)
  -> m a
parMapN ma0 ma1 f = unwrapNT sequential
  (f <$> unwrapNT parallel ma0 <*> unwrapNT parallel ma1)

-- function ref, converts RefineExceptions into a [Text]
-- because error type needs to be a Semigroup.
ref :: Predicate p x => x -> Either [String] (Refined p x)
ref x = left (\e -> [show e]) (refine x)

mkPerson :: Int -> String -> Either [String] Person
mkPerson a n = parMapN (ref a) (ref n) Person
{-
Now validates all inputs in parallel via implicit round-trip Either/Validation given by
Parallel instance.

Can use parMapN on lists without manually wrapping / unwrapping ZipLists.
-}

n1,n2 :: [Int]
n1 = [1..5]
n2 = [6..10]

n3 :: [Int]
n3  = (+) <$> n1 <*> n2

n4 :: [Int]
n4  = parMapN n1 n2 (+)

-- Without Parallel’s simplicity, it would look as follows:

n4' :: [Int]
n4' = getZipList $ (+) <$> ZipList n1 <*> ZipList n2

parTupled :: Parallel f m => m a0 -> m a1 -> m (a0, a1)
parTupled ma0 ma1 = parMapN ma0 ma1 (,)
