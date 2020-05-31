{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module E where

import           Data.Proxy
import           Data.Text
import           Data.Type.Equality
import           Data.String
import           GHC.TypeLits

{-
Mark Karpov
Existential quantification
November 11, 2018
https://markkarpov.com/post/existential-quantification.html

how existentials can be useful

Type variables and the forall keyword
-------------------------------------

type variables introduced via 'forall'

id :: forall a. a -> a

a will unify with ("fixed" to) any type that CONSUMER of id chooses

Fixing/unifying does not necessarily mean a concrete type is put in its place.
It might unify with another variable:

idInt :: Int -> Int
idInt = id -- 'a' unifies with 'Int'

id' :: forall b. b -> b
id' = id -- 'a' unifies with 'b'

What matters is that CHOICE OF TYPE IS MADE.

Rank-N types
------------

Only variables introduced with foralls at the beginning of type signature will be fixed
when calling the corresponding function.

Other foralls deal with independent type variables:
-}
myPrettyPrinter
  :: forall a
   . Show a          -- 'a' will be fixed by caller of 'myPrettyPrinter'
  => (forall b
     .  Show b
     => b -> String) -- but not 'b'
  -> Int
  -> Bool
  -> a
  -> String
myPrettyPrinter f _i _b a = f a
{-# ANN myPrettyPrinter ("HLint: ignore Eta reduce" :: Prelude.String) #-}
{-
two levels of foralls called rank-2 type

Universal and existential quantification
----------------------------------------

key

- universally   quantified when consumer chooses
- existentially quantified when consumer must deal with choice made already

universally and existentially quantified variables are both introduced with forall

examples

myPrettyPrinter
- a is universally   quantified for consumers (consumers choose)
- b is existentially quantified (consumer must handle any b given to the callback)

- in the body of myPrettyPrinter a is existentially quantified
  because caller of myPrettyPrinter has chosen the type
- when the function with b in its type is applied,
  the caller of that function chooses the type
- in the body of myPrettyPrinter b is universally quantified

Existential wrappers
--------------------

existentials by putting values in wrappers that "hide" type variables from signatures
-}
data Something0 where
  Something0 :: forall a. a -> Something0
{-
constructor accepts any a
after construction the type information is lost
later pattern matching only reveals some a, but nothing about what it is

compare to using 'id'
pass anything, but inside id, lack any info about the arg

wrapping enables returning existentially quantified data from a function
wrapper avoids unification of existentials with outer context and "escaping" of type variables

Why existentials?
-----------------

Existentials are always about throwing type information away.

Why?

To work with types not known at compile time.
The types typically depend on state of external world
- user’s input
- parsing a file

How to make use of existentials
-------------------------------

work with values of types not known at compile time
but at run time there are no types at all : they have been erased

Must preserve some info about existentially quantified type to make use of it.

degrees of how much to preserve:
1. have a in the type [a] existentially quantified
  - still can compute length of the list, etc.
  - possible types for a is open i.e. it can grow
2. assume existentially quantified type has certain properties (instances):
-}
data Showable where
  Showable :: forall a. Show a => Showable
{-
Pattern-matching on Showable gives the corresponding dictionary back.
Enables what the constraint permits.
Possible types for a is open (i.e., new instances of Show can be defined).

3. use GADTs to restore exact types of existentially quantified variables
-}
data EType a where
  ETypeInt    :: EType Int
  ETypeFloat  :: EType Float
  ETypeDouble :: EType Double
  ETypeString :: EType String

data Something1 where
  Something1 :: EType a -> a -> Something1
{-
matching on an EType constructor reveals a then can do anything 'a' allows
possible types for a is closed. Expand by changing EType def

closer look at #3:

Constraints vs GADTs
--------------------

constraints approach "method" is ok
- except when you need to know more about existential types
- so the list of constraints will grow and grow
- but different parts of code may require incompatible instances,
  so a given any type a can not satisfies all of them

solution : add more constructors to wrapper:
-}
data Something where
  SomethingIntegral :: forall a. (Show a, Integral a) => a -> Something
  SomethingFloating :: forall a. (Show a, Floating a) => a -> Something
  SomethingStringy  :: forall a. (Show a, IsString a) => a -> Something
{-
but this classifies the existential by a single pre-chosen criteria:
- one of: Integral, Floating,IsString
- but different parts of code may require different criteria
  - instances of Num
    - add Num a to constraints of both data constructors SomethingIntegral and SomethingFloating
    - then pattern match on both and do the same thing in each branch
  - only interested in an instance that only Int, or only Int and String, or ...

GADT-driven method (#3) enables recovering dictionaries of interest
if the value is an instance of right type classes
- fine-grained as it can be:
-}
data Foo a = Foo
data Dict a where
  Dict :: forall a. Dict a

reifyIntegralDict :: EType a -> Maybe (Dict a)
reifyIntegralDict  = \case
  ETypeInt   -> Just Dict
  _          -> Nothing

reifyFloatingDict :: EType a -> Maybe (Dict a)
reifyFloatingDict  = \case
  ETypeFloat  -> Just Dict
  ETypeDouble -> Just Dict
  _           -> Nothing

reifyFooDict :: EType a -> Maybe (Dict (Foo a))
reifyFooDict  = \case
  ETypeInt    -> Just Dict
  ETypeString -> Just Dict
  _           -> Nothing
{-
Example: vector indexed by existential length

Above approach amounts to having a sum data type like this:
-}
data EType0
  = EType0Int    Int
  | EType0Float  Float
  | EType0Double Double
  | EType0String String
{-
but there also can be

- Several existentially quantified type vars in a wrapper : combinatorial explosion
- recursively defined types : no way to enumerate all variants
  - e.g., vector indexed by existential length or something indexed by a list of type-level

example : vector indexed by existential length
-}
data Vector (n :: Nat) a where
  Nil  ::                    Vector 0       a
  Cons :: a -> Vector n a -> Vector (n + 1) a

data SomeVector where
  SomeVector :: KnownNat n => EType a -> Vector n a -> SomeVector
{-
Above is a of combination of all three approaches:

do some things with Vector without knowing type of its elements or its length
- can recover exact type of the elements if we wish

Type assertions: back to concrete types
---------------------------------------

not always necessary to use existentials and reify dictionaries
or proving things with Decision and (:~:)

example
-}
assertVector5Int
  :: SomeVector                 -- we could have parsed this from a file
  -> Either Text (Vector 5 Int) -- only continue if the type is this
assertVector5Int (SomeVector etype (v :: Vector n a)) =
  case etype of
    ETypeInt -> -- reifies that a ~ Int
      case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 5) of
        Nothing   -> Left "expected a vector of length 5"
        Just Refl -> -- reifies that n ~ 5
          Right v    -- can return
    _ -> Left "expected a vector of Ints"
{-
enables e.g., parsers that return existentials
but still able to go concrete types
-}
