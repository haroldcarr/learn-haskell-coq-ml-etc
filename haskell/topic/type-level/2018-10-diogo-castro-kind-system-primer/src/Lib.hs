{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators          #-}

module Lib where

import           Data.Kind    (Constraint)
import qualified Data.List    as List
import           Data.Ratio   ((%))
import qualified Data.Set     as Set
import           GHC.Exts     (RuntimeRep, TYPE)
import           GHC.Prim
import           GHC.TypeLits (Nat, Symbol)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/

Haskell's kind system - a primer
17 Oct 2018

------------------------------------------------------------------------------
Types and Kinds

:t/:type to check the type of a term
:k/:kind to check the kind of a type

inhabited types (types that have at least 1 value) are of kind *

------------------------------------------------------------------------------
Data constructors and type constructors

data constructors for creating data   : Just 1
type constructors for creating types  : Maybe Int

data constructors are curried and can be partially applied
type constructors likewise

:k Either
 :: * -> * -> *
:k Either String
 :: * -> *
:k Either String Int
 :: *

types can be specified explicitly
kinds can be specified explicitly (via KindSignatures)

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

data List (a :: *) = Cons a (List a) | Nil

class Functor (f :: * -> *) where
  fmap :: forall (a :: *) (b :: *). (a -> b) -> (f a -> f b)

------------------------------------------------------------------------------
higher-order functions (HOFs) : functions that take other functions as arguments
higher-kinded types    (HKTs) : types constructors that take other type constructors as arguments

:t map
 :: (a -> b) -> [a] -> [b]
-}
data NonEmpty f a = MkNonEmpty { head :: a, tail :: f a }
{-
:k NonEmpty
 :: (* -> *) -> * -> *

:t MkNonEmpty True [False, True]
 :: NonEmpty [] Bool

can apply this type constructor to any two types, so long as their kinds match the expected kinds
 NonEmpty []     Int
 NonEmpty Tree   String
 NonEmpty Vector Char

------------------------------------------------------------------------------
unboxed/unlifted types

* is the kind of all inhabited boxed (or lifted) types

but GHC also has unboxed (or unlifted) types (defined in GHC.Prim)
unlifted types end with a # (magic hash via MagicHash extension) e.g., Char#, Int#, (# a, b #), (# a | b #)

unlifted types have a kind that describes their runtime representation
(e.g, heap pointer, signed/unsigned word-sized value)
compiler uses type’s kind to decide machine code to produce ("kind-directed compilation")

------------------------------------------------------------------------------
Constraint kind (stuff to left of =>)

:k Show
 :: * -> Constraint

:k Show Int
 :: Constraint

:k Functor
 :: (* -> *) -> Constraint

:k Functor IO
 :: Constraint

ConstraintKinds enables first-class constraints

Set does not have a Functor instance because fmap :: (a -> b) -> f a -> f b must work for all types a and b,
but Set.map needs an Ord constraint

:t Data.Set.map
 :: Ord b => (a -> b) -> Set a -> Set b

write a more generic Functor typeclass that abstracts over constraints (that may or may not exist)
-}
-- {-# LANGUAGE ConstraintKinds        #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE KindSignatures         #-}

-- import           Data.Kind (Constraint)
-- import qualified Data.List as List
-- import qualified Data.Set  as Set

class GFunctor (c :: * -> Constraint) (f :: * -> *) | f -> c where
  gfmap :: c b => (a -> b) -> (f a -> f b)

instance GFunctor Ord Set.Set where
  gfmap = Set.map

instance GFunctor EmptyConstraint [] where
  gfmap = List.map

-- where EmptyConstraint is a typeclass constraint that’s trivially satisfied by all types.

-- {-# LANGUAGE FlexibleInstances #-}

class EmptyConstraint a
instance EmptyConstraint a

{-
------------------------------------------------------------------------------
Datatype promotion

when DataKinds enabled, data keyword creates : a custom kind and set of types/type constructors

{-# LANGUAGE DataKinds #-}
-}
data ConnectionStatus = Open | Closed
{-
new kind : ConnectionStatus
with two uninhabited types : 'Open and 'Closed

type ConnectionStatus has been promoted to a kind, and Open and Closed to types

{-# LANGUAGE KindSignatures #-}
-}

data Connection (s :: ConnectionStatus) = MkConnection

newConnection     :: Address           -> Connection Closed
newConnection      = undefined
openConnection    :: Connection Closed -> Connection Open
openConnection     = undefined
closeConnection   :: Connection Open   -> Connection Closed
closeConnection    = undefined
connectionAddress :: Connection s      -> Address
connectionAddress  = undefined
data Address = Address

{-
:k Connection Int
    • Expected kind ‘ConnectionStatus’, but ‘Int’ has kind ‘*’

builtins get promoted too

data F (b :: Bool) = MkF deriving Show

MkF :: F 'True
MkF :: F 'False

 MkF :: F 'Open
    • Expected kind ‘Bool’, but ‘ 'Open’ has kind ‘ConnectionStatus’

------------------------------------------------------------------------------
GHC.TypeLits

convenient kinds in GHC.TypeLits

Symbol : type-level strings

{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol)
import Data.Ratio ((%))
-}
newtype Money (currency :: Symbol) = Money Rational

fivePence :: Money "GBP"
fivePence = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros = Money 2
{-
enables statically checking different currencies don’t get mixed up
-}
add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)
{-
λ> add fivePence fivePence
Money (1 % 10)

λ> add fivePence twoEuros
    • Couldn't match type ‘"EUR"’ with ‘"GBP"’

Note: using Rational is overkill. Use 'Discrete' a wrapper around Integer

representing monetary values as integers requires keeping track of scale (i.e., dollar 1:1, cents : 100:1)

use Nat for scale

{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol, Nat)
-}
newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat)) -- tuple type
  = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1) -- type type constructor
oneDollarThirtyCents = Discrete 130
{-
------------------------------------------------------------------------------
Kind polymorphism

Parametric polymorphism : abstract over types
PolyKinds extension     : abstract over kinds

-}
data Proxy0 a = MkProxy0
{-
useful for passing types when no value is available

intRepresentative    = MkProxy0 :: Proxy0 Int
stringRepresentative = MkProxy0 :: Proxy0 String

GHC assumes a is kind *, so Proxy only works for lifted types

:k Proxy0
 :: * -> *

λ> maybeRepresentative = MkProxy0 :: Proxy Maybe0
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’

without extension, have to create multiple Proxy types

data Proxy0 a = MkProxy0
-}
data Proxy1   (a ::             * -> *) = MkProxy1
data Proxy2   (a ::        * -> * -> *) = MkProxy2
data HKProxy1 (a :: (* -> *) -> * -> *) = MkHKProxy1

stringRepresentative0   = MkProxy0   :: Proxy0 String
maybeRepresentative0    = MkProxy1   :: Proxy1 Maybe
eitherRepresentative0   = MkProxy2   :: Proxy2 Either
nonEmptyRepresentative0 = MkHKProxy1 :: HKProxy1 NonEmpty
{-
via PolyKinds to create a Proxy that works for all types a and for all kinds k.

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
-}
data Proxy (a :: k) = MkProxy

-- Now `a` can be anything at all
maybeRepresentative    = MkProxy :: Proxy Maybe
nonEmptyRepresentative = MkProxy :: Proxy NonEmpty
functionRepresentative = MkProxy :: Proxy (->)
helloRepresentative    = MkProxy :: Proxy "hello"
showRepresentative     = MkProxy :: Proxy Show
functorRepresentative  = MkProxy :: Proxy Functor
openRepresentative     = MkProxy :: Proxy 'Open
{-
omit kind signature, GHC infers a :: k.

λ> :set -XPolyKinds
λ> data Proxy a = MkProxy

λ> :k Proxy
 :: k -> *

example: Servant;s :> (to combine API components)

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
-}
data (:>) (a :: k) (b :: *)
infixr 4 :>
{-
type BooksAPI = "books" :> ReqBody '[JSON] Book :> Post '[JSON] ()

------------------------------------------------------------------------------
Levity polymorphism

to abstract over lifted and unlifted types

{-# LANGUAGE ExplicitForAll #-}

error :: forall a. String -> a
error is polymorphic in its return type, so it can be used (almost) anywhere.
-}
increment :: Int -> Int
increment x = error "oops"
{-
a assumed to be type of kind *, so cannot use it where an unlifted type is expected:
-}
incrementUnlifted :: Int# -> Int#
incrementUnlifted x = error "oops"
{-
<interactive>:22-40: error
   • Couldn't match a lifted type with an unlifted type

first attempt might be to turn on PolyKinds and introduce a kind variable k, but that won’t work either.

error :: forall k (a :: k). String -> a
/Playground.hs:15:40: error:
    • Expected a type, but ‘a’ has kind ‘k’
    • In the type signature: error :: forall k (a :: k). String -> a

won't compile because it doesn’t make sense for a :: k to be anything other than an inhabited type.
Say k was Symbol and a was "hello".
If the "hello" type is uninhabited, what value could such a function possibly return?

use levity polymorphism to make error work for all lifted and unlifted inhabited types

trick is in the kind TYPE r : kind parameterised over r :: RuntimeRep
describes a type’s runtime representation as one of

data RuntimeRep
  = VecRep VecCount VecElem -- ^ SIMD vector type
  | TupleRep [RuntimeRep]   -- ^ unboxed tuple of the given reps
  | SumRep [RuntimeRep]     -- ^ unboxed sum of the given reps
  | LiftedRep               -- ^ lifted; represented by a pointer
  | UnliftedRep             -- ^ unlifted; represented by a pointer
  | IntRep                  -- ^ signed, word-sized value
  | WordRep                 -- ^ unsigned, word-sized value
  | Int64Rep                -- ^ signed, 64-bit value (on 32-bit only)
  | Word64Rep               -- ^ unsigned, 64-bit value (on 32-bit only)
  | AddrRep                 -- ^ pointer, but /not/ to a Haskell value
  | FloatRep                -- ^ 32-bit floating point number
  | DoubleRep               -- ^ 64-bit floating point number

TYPE 'IntRep   is the kind of unlifted integers
TYPE 'FloatRep is the kind of unlifted floats

TYPE 'LiftedRep : the kind for all lifted types (* is a synonym for TYPE 'LiftedRep)

use TYPE r to abstract over unlifted and lifted types

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}

import GHC.Exts (TYPE, RuntimeRep)
-}
hcerror :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
hcerror  = undefined
{-
hcerror can be used in both increment and incrementUnlifted.

Levity polymorphism restrictions - see Richard Eisenberg talk

------------------------------------------------------------------------------
Summary

kind system classifies types
in Java, C#, all type variables <T> are of kind *
cannot have a type variable of kind * -> *, like in Functor<List>

cannot define abstractions like functors, monads, etc., without classification

GHC 7 and earlier, kinds were classified into sorts.
All kinds had the unique sort BOX, internal to GHC, invisible to developers.

GHC 8 went a different way.
TypeInType : types and kinds (and sorts) became one and the same.
Types can now be classified by other types.
3 is of type Int, Int is of type *, and * is of type *.
Paves way for full dependent types in Haskell.

kind * referred to as Type (do not confuse with TYPE r).
synonyms, but phasing out * in favour of Type.
-}
