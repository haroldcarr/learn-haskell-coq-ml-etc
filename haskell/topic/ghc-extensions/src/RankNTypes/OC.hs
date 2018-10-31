{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeFamilies   #-}

module RankNTypes.OC where

import Control.Monad.State
import System.Random
import Data.Char

{-
24 Days of GHC Extensions : Rank N Types : higher-rank polymorphism

Ertugrul “ertes” Söylemez

concrete monomorphic values (i.e., function values)
- different types but same definitions:
-}
intId :: Int -> Int
intId x = x

doubleId :: Double -> Double
doubleId x = x
{-
single parametric polymorphic def (Java calls it "generics")
-}
id0 :: a -> a
id0 x = x
{-
    Rank-1 polymorphism

above is *an* identity function
-}

id :: forall a. a -> a
id x = x

{-
'id' is a family of functions indexed by 'a' ("universally quantified" over 'a')

id signature PROMISES it works forall a

applying it DEMANDS a specific type

------------------------------------------------------------------------------
    Rank-2 and higher polymorphism
-}

someInt    :: (forall a. a -> a) -> Integer
someInt id' = id' 3
{-
- expects 1st arg is fully polymorphic identity function
  - can instantiate its type variable anyway it chooses
- when applying a polymorphic function like 'id' we get to choose types
- someInt does not give us such a choice
- requires polymorphic fun so IT can choose
- when we apply someInt, we need to give someInt choice

- identity promises to work forall a
- when we apply identity we demand a specific 'a'
- someInt does NOT promise
- someInt wants us  to pass a function that promisies
- someInt gets to demand (not us)

This is called rank-2 polymorphism.

------------------------------------------------------------------------------
arbitrary-rank polymorphism : nesting the quantifier in more levels
of necessary parentheses
-}

someOtherInt :: ((forall a. a -> a) -> Integer) -> Integer
someOtherInt someInt' = someInt' Prelude.id + someInt' Prelude.id
{-
above: rank-3-polymorphic, because the quantifier is in the
third level of necessary parentheses

------------------------------------------------------------------------------
Example: init structure with random values
-}

data Player = Player
  { playerName :: String
  , playerPos  :: (Double, Double)
  } deriving (Eq, Ord, Show)

-- pass generator
randomPlayer1
  :: RandomGen g
  => g
  -> (Player, g)
randomPlayer1 = undefined

-- more capabilities and more abstract


randomPlayer2
  :: (MonadIO m, MonadState g m, RandomGen g)
  => m Player
randomPlayer2 = undefined
{-
but awkward to use in context of another monad stack

instead
- do not require functional representation of random-number generator
- use a monad 'm' that provides /some/ means of random number generation
-}

-- 'm' action produces a random element
type GenAction  m = forall a. (Random a) =>           m a
-- ranged variant
type GenActionR m = forall a. (Random a) => (a, a) -> m a

-- generates a random number in a state monad, when the state is a generator
genRandom  :: (RandomGen g) => GenAction  (State g)
genRandom        = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

-- expand GenAction alias and simplify
genRandom' :: (Random a, RandomGen g) => State g a
genRandom' = undefined

randomPlayer3
  :: MonadIO m
  => GenActionR m
  -> m Player
randomPlayer3 genR = do
  liftIO (putStrLn "Generating random player...")
  len  <- genR (8, 12)
  name <- replicateM len (genR ('a', 'z'))
  x    <- genR (-100, 100)
  y    <- genR (-100, 100)
  liftIO (putStrLn "Done.")
  return (Player name (x, y))

-- rp3 uses a polymorphic 'genR', instantiated with different type variables at different points
runRP3 :: IO ()
runRP3 = randomPlayer3 randomRIO >>= print

------------------------------------------------------------------------------
-- Scott encoding

data List a
  = Cons a (List a)
  | Nil
  deriving Show

uncons :: (a -> List a -> r) -- ^ continuation to call on Cons
       -> r                  -- ^ value to return on Nil
       -> List a             -- ^ value to deconstruct
       -> r
uncons  c _n (Cons x xs) = c x xs
uncons _c  n  Nil        = n

isNull :: List a -> Bool
isNull = uncons
           (\_ _ -> False)
           True

-- pattern-matching in a functional style
listMap :: (a -> b) -> List a -> List b
listMap f = uncons
              (\x xs -> Cons (f x) (listMap f xs))
              Nil

-- list is fully determined by what happens during uncons
-- means : can represent a list in terms of its uncons operator : Scott encoding
-- requires a rank-2 type

newtype ListS a = ListS
  { unconsS
      :: forall r
       . (a -> ListS a -> r)
      -> r
      -> r
  }

-- list arg seems to be missing, it is constructed with ListS and accessed with unconsS
-- :t unconsS
--            :: ListS a -> (forall r. (a -> ListS a -> r) -> r -> r)

-- only difference with uncons is list arg first

-- Construct ListS : consider what happens on pattern-match

-- empty list
nilS :: ListS a
nilS = ListS (\_c n -> n)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\c _n -> c x xs)

-- convenient to have list arg be last arg
unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' c n (ListS f) = f c n

instance Functor ListS where
  fmap f = unconsS'
             (\x xs -> consS (f x) (fmap f xs))
             nilS
{-
similar to 'listMap'

why rank-2? : the 'ListS' constructor
  ListS :: (forall r. (a -> ListS a -> r) -> r -> r) -> ListS a

------------------------------------------------------------------------------
Church encoding

Scott  : lists defined in terms of what happens during uncons

Church : lists defined in terms of what happens during fold

  foldr :: (a -> r -> r) -> r -> [a] -> r
-}
-- no type recursion (like in ListS)
newtype ListC a = ListC
  { foldC
      :: forall r
       . (a -> r -> r)
      -> r
      -> r
  }

foldC' :: (a -> r -> r) -> r -> ListC a -> r
foldC' c n (ListC f) = f c n

instance Functor ListC where
  fmap f = foldC'
             (\x xs -> consC (f x) xs)
             nilC
consC :: a
consC  = undefined
nilC  :: a
nilC   = undefined

-- again: recursion is gone here at value level
-- because recursion encoded in the fold.

-- Exercise: DIFFICULT : write ListC uncons
{-
------------------------------------------------------------------------------
How 'runST' works

'IO' : allows arbitrary effects, including observable side effects
- takes one type argument: the result type
'ST' : family of monads for embedding a stateful imperative program into pure
- takes /two/ args, e.g.,

  writeSTRef :: STRef s a -> a -> ST s ()
  runST :: (forall s. ST s a) -> a

'runST' enforces
- 's' is polymorphic (needed by GHC)
- higher rank quantified type not allowed to leak out of its scope
  - only result 'a' comes out

------------------------------------------------------------------------------
GADTs and continuation passing style

type equality constraints (needs 'TypeFamilies') : x ~ y

:set -XTypeFamilies
15 :: Int -- Okay.
15 :: (Char ~ Char) => Int -- Okay.
15 :: (Int ~ Int) => Int -- Okay.
15 :: (Char ~ Int) => Int -- Type error!

-- marianna +34 646 69 00 04

TypeFamilies plus RankNTypes are sufficient to encode GADTs
in continuation passing style
-}
data Some :: * -> * where
  SomeInt  :: Int  -> Some Int
  SomeChar :: Char -> Some Char
  Anything :: a    -> Some a

unSome :: Some a -> a
unSome (SomeInt  x) = x + 3     -- type refinement
unSome (SomeChar c) = toLower c -- type refinement
unSome (Anything x) = x

-- when Scott or Church encoding useful, how to make practical?

newtype SomeC a = SomeC
  { runSomeC
      :: forall r
       . ((a ~ Int)  => Int  -> r) -- given "proof" a is Int and an Int : type refinement
      -> ((a ~ Char) => Char -> r)
      -> (a -> r)
      -> r
  }
{-
started with 'Some' sum type with three constructors so three continuations

pattern matching on GADT gives info a about types of args to constructor

above: GADTs without -XGADTs

------------------------------------------------------------------------------
Dependent types

Higher-rank polymorphism related to the dependent function arrow

https://twitter.com/ertesx/status/500034598042996736
- extensions to make Haskell is as expressive as a full dependently typed language.

what does higher-rank polymorphism provide?

Agda

  id : ∀ {A : Set} -> A -> A
  id x = x

takes two args
- 1st is type (called Set; corresponds to '*' kind)
  - not used at value level
  - used on type level in the rest of signature
  - passed implicitly (the curly braces) -- inferred from other args
- 2nd arg is value of that type

Haskell with KindSignatures
-}

idKS :: forall (a :: *) . a -> a
idKS x = x

{-
'forall' is the dependent function arrow
- can only communicate types (passed implicitly)

Haskell identity is a fun of 2 args
- one passed implicitly by type system
  - no runtime rep
- family of funs indexed by type

Agda : omit type when it can be inferred

  id : ∀ {A} -> A -> A

Haskell : without kind sig

  id :: forall a . a -> a

------------------------------------------------------------------------------

'runST' : Agda    : runST : ∀ {A} -> (∀     {S} -> ST S A) -> A
        : Haskell : runST ::         (forall s.    ST s a) -> a

- 2nd (1st explicit) arg is a fun that receives type 'S' from 'runST'
- no way to return 'S'
  - 'S' cannot unify with 'A'
  - because scope of 'A' is broader than scope of 'S'
  - type of 'A' is determined before the type 'S', so 'A' cannot depend on 'S'

------------------------------------------------------------------------------
a quantifier law

manipulating types with quantifiers

identity function is also a proof

 id :: forall a . a -> a

type of 'id' is a proposition
- a proof for 'a' implies a proof for 'a', for all propositions 'a'
- the total fun of the given type is a proof of the proposition

since there is a one-to-one correspondence between types and propositions
- can transfer for

  true for all 'X' and 'Y'

  X -> forall a. Y a = forall a. X -> Y a

means
- if quantifier is on right side of function arrow
- can pull it out and wrap whole function type with the quantifier

e.g.,

type GenAction m = forall a. (Random a) => m a
genRandom :: (RandomGen g) => GenAction (State g)
-- expanded:
genRandom :: (RandomGen g) => (forall a. (Random a) => State g a)

context arrow '(=>)' : way to pass implicit arguments via type classes
- read it like regular function arrow'(->)'

so right side of outer arrow is a quantified type, so apply rule/transformation

genRandom :: forall a. ((RandomGen g) => ((Random a) => State g a))
genRandom :: forall a. ( RandomGen g) =>  (Random a) => State g a
genRandom :: forall a. ( RandomGen g,      Random a) => State g a
-- above is rank-1 : can omit quantifier
genRandom ::           ( RandomGen g,      Random a) => State g a
-}
