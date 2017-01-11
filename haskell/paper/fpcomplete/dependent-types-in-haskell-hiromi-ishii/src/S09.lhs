> {-# LANGUAGE GADTs, ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S09 where
>
> import Prelude hiding (tail, head, replicate, map)

Ordinals
--------

Focus on implementation of vectors

implement `index`ing withboundary condition statically checked.

must implement type representing "natural numbers below `n`"
- such a type is called a *finite set* or *ordinal*
- e.g., `Ordinal Z` has no inhabitant and `Ordinal (S (S Z))` has two elements corresponding to `0` and `1`.

If n > 0, `Ordinal n` has 0 as inhabitant

If `k :: Ordinal n`, then `k + 1 :: Ordinal n` might be failed (in the case k = n - 1)
- but `k + 1 :: Ordinal (n + 1)` always holds

data Ordinal (n :: Nat) where
  OZ :: Ordinal (S n)
  OS :: Ordinal n -> Ordinal (S n)

`OS (OS OZ) :: Ordinal Three` and
`OS (OS OZ) :: Ordinal Five` pass type-checking

`OS (OS OZ) :: Ordinal Two` does not.

> data Nat = Z | S Nat

> infixl 6 :+
> infixl 7 :*

> type family   (n :: Nat) :+ (m :: Nat) :: Nat
> type instance Z     :+ m = m
> type instance (S n) :+ m = S (n :+ m)

> type family   (n :: Nat) :* (m :: Nat) :: Nat
> type instance Z     :* m = Z
> type instance (S n) :* m = (n :* m) :+ m

> data Vector a n where
>   Nil  :: Vector a Z
>   (:-) :: a -> Vector a n -> Vector a (S n)
> infixr 5 :-

> deriving instance Eq a => Eq (Vector a n)

> toList :: Vector a n -> [a]
> toList Nil = []
> toList (x :- xs) = x : toList xs

> instance Show a => Show (Vector a n) where
>   showsPrec d = showsPrec d . toList

> data SNat n where
>   SZ :: SNat Z
>   SS :: SNat n -> SNat (S n)

> data Ordinal (n :: Nat) where
>   OZ :: Ordinal (S n)
>   OS :: Ordinal n -> Ordinal (S n)

> sIndex :: Ordinal n -> Vector a n -> a
> sIndex OZ     (x :- _)  = x
> sIndex (OS n) (_ :- xs) = sIndex n xs

> s09_e1 = sIndex (OS OZ) (0 :- 1 :- 2 :- 3 :- Nil)
> s09_e2 = sIndex OZ (0 :- 1 :- Nil)

> -- compilation error:
> -- s09_e3 = sIndex (OS OZ) (0 :- Nil)

Use `type-natural` package quasiquoter to eliminate writing
- `OS (OS (OS (OS (OS (OS OZ)))))`
- `[od|6|]`

Exercises:

1. Implement the `sElemIndices :: Eq a => a -> Vector a n -> [Ordinal n]`, which returns all the index with element `a`.
2. Implement the ordinal addition with the *correct* type signature.
4. We can provide `Num` instance for `Ordinal n` (and in fact `type-natural` provides that), but we use quasiquotes here. Why?

Hint: consider the type of`fromInteger`.

What's Next?
============

We have now the `Vector` type, which is much alike lists. One missing famous function is `reverse` for them:

```active haskell
{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
data Nat = Z | S Nat
type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z   :+ m = m
type instance S n :+ m = S (n :+ m)
data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
-- show
reverse :: Vector a n -> Vector a n
reverse xs0 = go Nil xs0
  where
    go :: Vector a m -> Vector a k -> Vector a (k :+ m)
    go acc Nil = acc
    go acc (x :- xs) = go (x:- acc) xs
-- /show
main = return ()
```

Unfortunately, the above code won't type-check!

```
Couldn't match type `n' with `n :+ 'Z'
```

This means that GHC cannot infer that `n = n + 0`. So we have to tell  the compiler that fact - in other words, we have to *prove* that fact!

In the next article, we will see how to write such proofs comfortablly and the way to express the relation such as equality and ordering.
