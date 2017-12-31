> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
>
> module S1_example_of_singletons where
>
> import qualified GHC.Types as T

https://github.com/goldfirere/singletons
https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf

1.1  example of singletons

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat
>
> data Vec :: T.Type -> Nat -> T.Type where
>   VNil  :: Vec a 'Zero
>   VCons :: a -> Vec a n -> Vec a ('Succ n)
>
> type family (m :: Nat) :< (n :: Nat) :: Bool
> type instance       m  :<  'Zero      = 'False
> type instance 'Zero    :<  'Succ n    = 'True
> type instance 'Succ m  :<  'Succ n    = m :< n

Haskell enforces phase distinction between runtime values and compiletime types.

Use singletons to express dependency between runtime value and compiletime type.

Singleton : type with only ONE non ⊥ value.

e.g.,

index into Vect.
- ensure index 'i' < length of vector n
- constraint (i :< n) ∼ True
- 'i' is a runtime value
- 'n' is a compiletime type
- need runtime "witness" for 'i'

SNat : singleton values for natural numbers
- GADT indexed by a type of kind Nat.

> data SNat :: Nat -> T.Type where
>   SZero :: SNat 'Zero
>   SSucc :: forall (n :: Nat). SNat n -> SNat ('Succ n)

kinds        +----------->  promoted kind  -------------+
             |                  Nat                     |
-------------|------------------------------------------|----------
             |                                          v
         original ADT                            singleton type
types       Nat             'Succ  'Zero         SNat ('Succ 'Zero)
-------------------------------------------------------------------
terms    Succ,  Zero      ------------------>    SSucc,   SZero


Because constructors of SNat mirror constructors of the kind Nat
- only one non-⊥ term exists in each fully-applied type in the SNat family.
- the type variable indexing the type and the one non-⊥ term of that type are always isomorphic.

Singletons used to keep term-level computation and type-level computation in lock-step.

This singleton is the first runtime argument of the nth function
and determines the element of the vector that should be returned.

> nth :: (i :< n) ~ 'True => SNat i -> Vec a n -> a
> nth  SZero     (VCons a  _) = a
> nth (SSucc i') (VCons _ as) = nth i' as

second case type-checks because pattern matching refines the type variables m and n to be headed by Succ.
Therefore, constraint m :< n reduces to the simpler constraint required by the recursive call to nth.

GHC observes that indexing must succeed.
So:

  nth _ VNil = undefined

    * Couldn't match type ‘'True’ with ‘'False’
      Inaccessible code in
        a pattern with constructor: VNil :: forall a. Vec a 'Zero,
        in an equation for ‘nth’

1.2 The singletons library

data type overhead : now three sorts of Nats.
- Datatype promotion automatically provides the kind level version
- but not the singleton SNat

function type overhead
- :< type family only applies to types
- even though identical to term level

overhead is boilerplate

necessary boilerplate produced by Template Haskell primitives in singletons library.
- produce singleton types from datatypes,
- type families and singleton functions from function definitions.

Does not hide the encoding from the programmer.
- need to remain aware of the definitions made by library
  and how they interact with GHC’s type inference engine in order for code to compile

