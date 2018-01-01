> {-# LANGUAGE GADTs          #-}
> {-# LANGUAGE KindSignatures #-}
>
> module Part3_1_vec_and_shortcomings where
>
> import Part1_1_MPTC (Z, S)
> import Part2_type_families (Plus)
> import Prelude hiding (append)

 https://byorgey.wordpress.com/2010/07/19/typed-type-level-programming-in-haskell-part-iii-i-can-haz-typs-plz/

Example use of type families: polymorphic length-indexed vectors (aka "Length-Observed Lists")

> -- | LOL is type constructor of kind * -> * -> *
> -- - takes two type arguments of kind * (1st arg is length, 2nd args is element type)
> -- - produces a type of kind *
> data LOL :: * -> * -> * where
>   -- constructs a vector of length zero
>   KThxBye ::                 LOL  Z    a
>   -- Given element of type a and vector of as of length n
>   -- construct vector of length S n
>   Moar    :: a -> LOL n a -> LOL (S n) a

type-level Plus function used to implement append

> append :: LOL m a -> LOL n a -> LOL (Plus m n) a
> append  KThxBye    v =                   v
> append (Moar x xs) v = Moar x (append xs v)

SHORTCOMINGS

1. Even though Nat is declared with Z and S constructors, have to declare empty types Z and S to represent type-level nats

2. Even though value plus defined for Nat values, have to code plus at type level (with type family Plus)
- even though definitions identical

3. LOL is untyped
- LOL :: * -> * -> *
- nothing constrains 1st arg to be type-level number, 2nd arg to be vector

WANT
- reuse (1) values and (2) functions at the type level
- (3) get more contrained kinds

