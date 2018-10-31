{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeOperators          #-}

module K where

import           Data.Kind    (Constraint)
import           Data.Ratio   ((%))
import qualified Data.Set     as Set
import qualified Data.List    as List
import           GHC.Exts     (TYPE, RuntimeRep)
import           GHC.Prim
import           GHC.TypeLits (Nat, Symbol)
{-
Haskell's kind system
17 Oct 2018

    Types and Kinds
    Data constructors and type constructors
    Type signatures and kind signatures
    HOFs and HKTs
    Other kinds
        Unboxed/unlifted types
        Constraints
        Datatype promotion
        GHC.TypeLits
    Kind polymorphism
    Levity polymorphism

------------------------------------------------------------------------------
Types and Kinds

values/terms : classified by types : e.g., :t "hello" :: String   :t True :: Bool
types        : classified by kinds : e.g., :k String :: *         :k Bool :: *

kind         +------> * <---+
             |              |
type      String           Bool
          ^    ^           ^  ^
term "hello"  "world"   True  False

inhabited types (types that have at least 1 value) are of kind *
- e.g., Int, Int -> String, [Int], Maybe Int, Either Int Int

------------------------------------------------------------------------------
Data constructors and type constructors

data constructors for creating data
type constructors for creating types
-}
data Person = MkPerson
  { name :: String
  , age  :: Int
  }
{-
MkPerson is a data constructor
:t MkPerson
            :: String -> Int -> Person

-}
data EitherX a b = LX a | RX b
{-
EitherX is a type constructor (given two types of kind *,
creates another type of kind *

data constructors are curried and can be partially applied

:t MkPerson
            :: String -> Int -> Person
:t MkPerson "Diogo"
            :: Int -> Person
:t MkPerson "Diogo" 29
            :: Person

type constructors are curried and can be partially applied

:k Either
          :: * -> * -> *
:k Either String
          :: * -> *
:k Either String Int
          :: *

------------------------------------------------------------------------------
Type signatures and kind signatures

GHC can infer types

-- The inferred type of `x` is `Bool`
x = True

-- The inferred type of `y` is `String -> IO ()`
y = putStrLn

GHC can infer kinds

-- The inferred kind of `a` is `*`
data List a = Cons a (List a) | Nil

-- The inferred kind of `f` is `* -> *`
-- The inferred kind of `a` and `b` is `*`
class Functor f where
  fmap :: (a -> b) -> (f a -> f b)

manually specify types

x :: Bool
x = True

y :: String -> IO ()
y = putStrLn

manually specify kinds (via KindSignatures)
-}
data List (a :: *) = Cons a (List a) | Nil

-- also ExplicitForAll
class Functor (f :: * -> *) where
  fmap :: forall (a :: *)
                 (b :: *)
        . (  a ->   b)
       -> (f a -> f b)
{-
------------------------------------------------------------------------------
HOFs and HKTs

higher-order functions (HOFs): functions that take other functions as args
higher-kinded types (HKTs), types constructors that take other type constructors as args

:t map
       :: (a -> b) -> [a] -> [b]
-}
data NonEmpty f a = MkNonEmpty
  { head :: a
  , tail :: f a
  }
{-
:k NonEmpty
NonEmpty :: (* -> *) -> * -> *

NonEmpty is a type constructor that takes
- another type constructor of kind (* -> *)
- and a type of kind *

e.g., NonEmpty [] Bool

-- - list of boolean values guaranteed to have at least one value
:t MkNonEmpty True [False, True]
                                 :: NonEmpty [] Bool

can apply NonEmpty to any types with appropriate kinds, e.g.
- NonEmpty []     Int
- NonEmpty Tree   String
- NonEmpty Vector Char

------------------------------------------------------------------------------
Unboxed/unlifted types

* : kind of all inhabited boxed (aka lifted) types

inhabited types that are not kind *: unboxed (unlifted) types
- defined in ghc-prim package
- convention : unlifted types end with # ("magic hash") MagicHash extension
- e.g.,
  - Char#
  - Int#
  - (# a, b #)
  - (# a | b #)

unlifted types have kinds that describe their runtime representation
- pointer to something in heap; (un)signed word-sized value

kind  TYPE IntRep    TYPE WordRep    Type (TupleRep '[IntRep, WordRep])
       ^              ^      ^             ^
type  Int#           Word#  Char#    (# Int#, Char# #)
       ^              ^      ^             ^
term  3#             4##    'a'#     (# 3#, 'a'# #)

------------------------------------------------------------------------------
Constraint kind

:k Show
        :: * -> Constraint

:k Show Int
        :: Constraint

:k Functor
        :: (* -> *) -> Constraint

:k Functor IO
        :: Constraint

ConstraintKinds : first-class constraints

Set does not have a Functor instance
- because fmap :: (a -> b) -> f a -> f b must work for all a and b
  Set.map has an Ord constraint

:t Data.Set.map
                :: Ord b => (a -> b) -> Set a -> Set b

write generic Functor that abstracts over constraint that may or may not exist
- using : ConstraintKinds, KindSignatures, FunctionalDependencies
-}
class GFunctor (c :: * -> Constraint) (f :: * -> *) | f -> c where
  gfmap :: c b => (a -> b) -> (f a -> f b)

instance GFunctor Ord Set.Set where
  gfmap = Set.map

instance GFunctor EmptyConstraint [] where
  gfmap = List.map

-- constraint that is satisfied by all types
class EmptyConstraint a
instance EmptyConstraint a
{-
------------------------------------------------------------------------------
Datatype promotion

data keyword : define custom type/type constructor and data constructors
-}
data ConnectionStatus = Open | Closed
{-
kind         *
             ^
type  ConnectionStatus
       ^         ^
term  Open      Closed

DataKinds : data keyword also creates
- a custom kind
- set of types/type constructors

new kind ConnectionStatus with two uninhabited types, 'Open and 'Closed

kind  ConnectionStatus
       ^       ^
type  'Open   'Closed
term  (uninhabited)

type ConnectionStatus has been promoted to a kind
terms Open and Closed have been promoted to types
-}

newtype Connection (s :: ConnectionStatus) = MkConnection { address :: Address }

newConnection     :: Address            -> Connection 'Closed
openConnection    :: Connection 'Closed -> Connection 'Open
closeConnection   :: Connection 'Open   -> Connection 'Closed
connectionAddress :: Connection s       -> Address
newConnection      = MkConnection
openConnection   c = MkConnection (address c)
closeConnection  c = MkConnection (address c)
connectionAddress  = address
type Address = String

{-
:k Connection Int
<interactive>:1:12: error:
    • Expected kind ‘ConnectionStatus’, but ‘Int’ has kind ‘*’

"phantom" tagging connection with its status to statically enforce rules
- closeConnection cannot be called on an already closed connection

DataKinds promotes custom data and existing types (e.g., Bool)
-}
data F (b :: Bool) = MkF deriving Show
{-
MkF :: F 'True
MkF :: F 'False
MkF :: F 'Open
<interactive>:30:10: error:
    • Expected kind ‘Bool’, but ‘ 'Open’ has kind ‘ConnectionStatus’

------------------------------------------------------------------------------
GHC.TypeLits : convenient kinds

Symbol : kind for type-level strings : "hello" as a type

kind         +------> * <---+                        Symbol
             |              |                        ^    ^
type      String           Bool                 "hello"  "world"
          ^    ^           ^  ^
term "hello"  "world"   True  False             (uninhabited)


tag a type with a string literal
-}

newtype Money (currency :: Symbol) = Money Rational

fivePence :: Money "GBP"
fivePence  = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros  = Money 2
{-
statically can’t add EUR and GBP
-}
add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)

tenPence :: Money "GBP"
tenPence  = add fivePence fivePence
-- => Money (1 % 10)
{-
add fivePence twoEuros
<interactive>:8:15: error:
    • Couldn't match type ‘"EUR"’ with ‘"GBP"’

--------------------------------------------------
Nat : kind for type-level natural numbers
-}
newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1)
oneDollarThirtyCents = Discrete 130
{-
scale :: (Nat, Nat)    promoted tuple (,)
'(100, 1)    '(,) is tuple data constructor promoted to type constructor

------------------------------------------------------------------------------
Kind polymorphism

parametric polymorphism : abstract over types
PolyKinds               : abstract over kinds
-}
data Proxy a = MkProxy
{-
data constructor with no args, tagged with type variable
useful for passing types (even if no values available)
-}
intRepresentative    :: Proxy Int
intRepresentative     = MkProxy
stringRepresentative :: Proxy String
stringRepresentative  = MkProxy
{-
GHC assumes 'a' is of kind '*', which means Proxy only works for lifted types

:k Proxy
         :: * -> *

maybeRepresentative = MkProxy :: Proxy Maybe
<interactive>:9:38: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’

Need boilerplate
-}
data Proxy1   (a ::             * -> *) = MkProxy1
data Proxy2   (a ::       *  -> * -> *) = MkProxy2
data HKProxy1 (a :: (* -> *) -> * -> *) = MkHKProxy1

stringRepresentative2  :: Proxy String
stringRepresentative2   = MkProxy
maybeRepresentative    :: Proxy1 Maybe
maybeRepresentative     = MkProxy1
eitherRepresentative   :: Proxy2 Either
eitherRepresentative    = MkProxy2
nonEmptyRepresentative :: HKProxy1 NonEmpty
nonEmptyRepresentative  = MkHKProxy1
{-
use PolyKinds to create a Proxy that works for all types a and for all kinds k
-}
data ProxyHC (a :: k) = MkProxyHC

-- Now `a` can be anything at all
maybeRepresentativeHC   :: Proxy Maybe
maybeRepresentativeHC    = MkProxy
nonEmptyRepresentativeHC:: Proxy NonEmpty
nonEmptyRepresentativeHC = MkProxy
functionRepresentative  :: Proxy (->)
functionRepresentative   = MkProxy
helloRepresentative     :: Proxy "hello"
helloRepresentative      = MkProxy
showRepresentative      :: Proxy Show
showRepresentative       = MkProxy
functorRepresentative   :: Proxy Prelude.Functor
functorRepresentative    = MkProxy
openRepresentative      :: Proxy 'Open
openRepresentative       = MkProxy
{-
omit kind signature : GHC will infer a :: k

λ> :set -XPolyKinds
λ> data Proxy a = MkProxy

λ> :k Proxy
Proxy :: k -> *

--------------------------------------------------
Servant
has :> type used to combine components of an API
components can vary in type and kind
-}
data (:>) (a :: k) (b :: *)
infixr 4 :>

-- use :> to combine types "books" :: Symbol, ReqBody '[JSON] Book :: *, Post '[JSON] () :: *
-- type BooksAPI = "books" :> ReqBody '[JSON] Book :> Post '[JSON] ()
{-
------------------------------------------------------------------------------
Levity polymorphism

to abstract over lifted and unlifted types, e.g.,
-}

-- polymorphic in its return type : can be used (almost) anywhere
errorX :: forall a. String -> a
errorX = error

increment :: Int -> Int
increment = error "oops"

-- a (by default) assumed to be a type of kind *
-- so cannot use with unlifted type

incrementUnlifted :: Int# -> Int#
incrementUnlifted = error "oops"
{-
<interactive>:22-40: error
   • Couldn't match a lifted type with an unlifted type

PolyKinds and kind variable won’t work:

error :: forall k (a :: k). String -> a

    • Expected a type, but ‘a’ has kind ‘k’
    • In the type signature: error :: forall k (a :: k). String -> a

doesn’t make sense for a :: k to be anything other than an inhabited type.
If k was Symbol and a was "hello" but "hello" uninhabited then nothing can be return

To make error for all (un)lifted inhabited types : levity polymorphism

kind : TYPE r
- kind parameterised over r :: RuntimeRep

data RuntimeRep = VecRep VecCount VecElem -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]   -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]     -- ^ An unboxed sum of the given reps
                | LiftedRep       -- ^ lifted; represented by a pointer
                | UnliftedRep     -- ^ unlifted; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | WordRep         -- ^ unsigned, word-sized value
                | Int64Rep        -- ^ signed, 64-bit value (on 32-bit only)
                | Word64Rep       -- ^ unsigned, 64-bit value (on 32-bit only)
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number

TYPE 'LiftedRep : kind for all lifted types : * is a synonym

TYPE r to abstract over all (un)lifted types
-}

errorY :: forall (r :: RuntimeRep) (a :: TYPE r). String -> a
errorY = error
{-
some places levity polymorphism cannot be used
- where/why : see Richard Eisenberg’s talk *TODO*

------------------------------------------------------------------------------
conclusion

kinds enables abstractions like functors, monads, NonEmpty
levity polymorphism : abstractions that work with both (un)boxed types
kind system enables type-level programming

terms classified into types
types classified into kinds
are kinds classified into something else?
- <= GHC 7 : kinds were classified into sorts
- >= GHC 8 : similarities between types and kinds: both can be
  - higher-order, polymorphic, inferred, curried, etc
  - TypeInType : types and kinds (and sorts) became one and the same
    - types can now be classified by other types
      3 is of type Int
      Int is of type *
      * is of type *

kind * now referred to as 'Type' (do not confuse with TYPE r)
- gradually phase out * in favour of Type.

more about Data.Proxy and use cases : see Kwang Seo’s blog post *TODO*
-}