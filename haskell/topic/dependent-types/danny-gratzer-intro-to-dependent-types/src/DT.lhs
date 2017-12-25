> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module DT where

value-level - but also type level via DataKinds

> data Nat = Z | S Nat deriving (Show)

utilities

> toNat :: Int -> Nat
> toNat 0 = Z
> toNat n = S (toNat $ n - 1)

> toInt :: Nat -> Int
> toInt  Z    = 0
> toInt (S n) = 1 + toInt n

mirror (i.e., lift, reflect) value into isomorphic type

> data RNat :: Nat -> * where
>   RZ ::           RNat  Z
>   RS :: RNat n -> RNat (S n)

type-level addition

> type family Plus n m :: Nat where
>   Plus  Z    m =           m
>   Plus (S n) m = S (Plus n m)

value-level addition

> plus :: Nat -> Nat -> Nat
> plus  Z    m =           m
> plus (S n) m = S (plus n m)

value-level addition using dependent types

> plus' :: RNat n -> RNat m -> RNat (Plus n m)
> plus'  RZ    m =             m
> plus' (RS n) m = RS (plus' n m)

get type error if definition incorrect

> {-
> plusB :: RNat n -> RNat m -> RNat (Plus n m)
> plusB  RZ    m =         m
> plusB (RS n) m = plus' n m -- Unification error! n ~ S n
> -}

    • Could not deduce: Plus n1 m ~ 'S (Plus n1 m)
      from the context: n ~ 'S n1
        bound by a pattern with constructor:
                   RS :: forall (n :: Nat). RNat n -> RNat ('S n),
                 in an equation for ‘plusB’
      Expected type: RNat (Plus n m)
        Actual type: RNat (Plus n1 m)

use Nat

> readNat :: String -> Nat
> readNat = toNat <$> read

> e1 = toInt (plus (readNat "4") (readNat "5"))

turn runtime values into types

key idea
- do not mirror types with DataKinds
- enable values to exist in types on their own

1. A special reflective function type

return type use supplied value: called pi types
- (x :: A) -> B x
- Where A :: * and B :: A -> * are some sort of type
-  A in B’s kind is NOT the data kind promoted version - it is a normal value

2. Lifting expressions into types

To enable B to use it’s supplied value
- enable normal types to be indexed by values (like how GADTs can be indexed on types)
- called GGADTs

> -- repeated from above
> -- data RNat :: Nat -> * where
> --   RZ ::           RNat  Z
> --   RS :: RNat n -> RNat (S n)

semantics
- Z’s and S’s represent actual values (not members of some kind)
  - no promoting types to singleton kinds

Because depending on values, do not need peano numbers

function that combines ideas 1 and 2

> {-
> toRNat :: (n :: Nat) -> RNat n
> toRNat Z = RZ
> toRNat (S n) = RZ (toRNat n)
> -}

> {-
> -- types call functions (e.g., +)
> -- can be undecidable
> data RInt :: Int -> * where
>   RIZ ::           RInt  0
>   RIS :: RInt n -> RInt (1 + n)
>
> toRInt :: (n :: Int) -> RInt n
>   toRInt 0 = RZ
>   toRInt n = RS (toRInt $ n - 1)
> -}

used pi types to change return type dependent on the input value

main = print . toInt $ plus' <$> fmap toRInt readLn <*> fmap toRInt readLn

when value reflected to type level : can’t do anything with it

when using pi types often have to pattern match on arguments to help typechecker

First the type checker goes through toRNat.

    toRNat :: (n :: Nat) -> RNat n
    toRNat  Z    = RZ           -- We know that n is `Z` in this branch
    toRNat (S n) = RS (toRNat n {- This has the type RNat n' -})

In the first branch we have n equals Z, so RZ typechecks.

Next is case S n.
- toRNat n has the type RNat n' by induction
- S n' = n.
- Therefore RS builds term of type RNat n

typechecker through p

    p :: (n :: Nat) -> (m :: Int) -> RNat (plus n m)
    p  Z    m =     toRNat   m
    p (S n) m = RS (toRNat n m)

p Z m case
- n is Z
- plus n m is plus Z m is by definition equal to m
- produce RNat m with toRNat :: (n :: Nat) -> RNat n
- apply to m and : resulting term has type RNat m

RS case tring produce  term of type RNat (plus (S n) m)
- via pattern matching, we can reduce plus (S n) m to S (plus n m)
- looking to build term of type plus n m via recursion
- apply RS to give S (plus n m)
- previously noted S (plus n m) is equal to plus (S n) m

typechecker never needed to do arbitrary reductions
- only reduce when given outer constructor (WHNF) of one of args

two of central concepts in dependent types
- Indexed type families (GGADTs)
- Dependent function types (Pi types)
