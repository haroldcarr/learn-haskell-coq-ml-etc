> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE RankNTypes #-}
>
> module D where
>
> import Data.Singletons.Prelude

http://www.vittoriozaccaria.net/#/blog/2018/01/27/what-i-whish-i-knew-haskell-and-dependent-pairs.html

Vittorio Zaccaria
Haskell and dependent pairs
January 27, 2018

Dependent types

"term b has a dependent type B"
- term b's type depends on the value of another term a : b:Ba

Dependent pair

two terms (a,b) : a:A and b:Ba

aka "dependent sum" : type is composed of an algebraic type sum

    (a,b) : {a1} x Ba1 + {a2} x Ba2 ...

    aka

    (a,b) : ∑x:A Bx

ai
- an inhabitant of A
- subtype of A that includes only the value ai (i.e., a singleton)


Practical use

- proof construction
- make programs more correct (e.g., avoiding access to zero sized vector)
- more precisely describe the intended behavior of programs
- reduce combinatorial space of cases to consider when building a function

    f :: BoolPair -> ...

- BoolPair not a dependent pair
- so must consider all combinations of cartesian product Bool×(String+Int)

How to build one

use singletons

enumerate all ai at type level : produce singleton type for each

    BoolPair : ∑x:Bool (if~x~then~Int~else~String)

> type family   B (x :: Bool)
> type instance B 'True  = Int
> type instance B 'False = String
>
> data BoolPair where
>   (:*:) :: forall x. Sing x -> B x -> BoolPair

GADTs
- enable defining generic sum types
- access each "addend" via pattern matching
- constructor of each pair is :*: operator
- x is type variable of kind Bool
- use to enumerate A, as a label that mimicks each inhabitant ai∈A
  for which a singleton must be defined
- Sing x transforms each label x into a singleton type {x}
  for which value constructors must be provided
  - most important are provided in singletons library
- B x transforms the same label through the type function function B(x)


> f :: BoolPair -> String
> f (STrue  :*: n) = show n
> f (SFalse :*: s) = s

but:

  f' :: BoolPair -> String
  f' (STrue :*: s) = s

type-error: singleton value STrue requires integer for second value of pair

generic version of BoolPair : works with generic t!=B and generic s!='Bool

singletons library : sigma types:

data Sigma (s :: Type) :: (s ~> Type) -> Type where
  (:&:) :: Sing (fst :: s) -> t @@ fst -> Sigma s t

generic BoolPair : uses :&: instead of :*:

Existentials

proposition : collection (type) of all possible witnesses of its truth (propositions as types)

witnesses constructed by combining their types
- follows the way in which truth is combined through logic operators
  provided that one can create a way to build such witnesses through an expression
  of a matching type

If  A is   a collection of witnesses of proposition A
and B is the collection of witnesses of             B,
the collection of witnesses of B∧A can be represented by type A×B
which is inhabited, since at least one pair of them can be formed

assume

- ai  : the type of the witness that ai exists
- Bai : the type of the witnesses for a proposition that depends on ai

then the expression of the dependent pair:

    (a,b) : {a1} x Ba1 + {a2} × Ba2 ...

can be interpreted in logic as

    (∃a1 ∧ Ba1) ∨ (∃a2 ∧ Ba2) ... ∼∃x.B(x)

explains why dependent pairs are used to express existentially quantified predicates






