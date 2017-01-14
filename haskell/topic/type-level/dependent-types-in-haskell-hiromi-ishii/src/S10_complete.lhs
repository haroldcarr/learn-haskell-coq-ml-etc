> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S10_complete where

already implemented in
- `type-natural` http://hackage.haskell.org/package/type-natural
- `sized-vector` http://hackage.haskell.org/package/sized-vector

Type-level naturals
--------------------

**Peano numerals** : http://www.haskell.org/haskellwiki/Peano_numbers

> -- value-level
> data Nat = Z | S Nat

with DataKinds, also type-level
- then GHC defines *type constructors* `Z` and `S` with *kinds* `Nat` and `Nat -> Nat`

relation between kinds and types is the same as
                 types and values
- "kind" means "type of type"
- Usual types have kind `*` : `Int :: *`, `() :: *` and `Bool :: *`
- Parametric types expressed by arrows: `[] :: * -> *`, `Either :: * -> * -> *` and `StateT :: * -> (* -> *) -> * -> *`
- DataKinds adds new basic kinds (per module; if need promoted types defined other modules, enable `DataKinds`,
  or provide "type synonym" for the promoted types and export them)
  Haskell-cafe : export DataKinds : http://www.haskell.org/pipermail/haskell-cafe/2014-February/112930.html

Only types with kind `*` have inhabitants (i.e.,  values)
- type introduced by `DataKinds` (e.g. types `Z` or `S (S n)`) have NO inhabitants.

Type-level functions
--------------------

`TypeFamilies` extension

`TypeOperators` extension: enables infix type operator

> infixl 6 :+
> infixl 7 :*

> type family   (n :: Nat) :+ (m :: Nat) :: Nat
> type instance  Z    :+ m =         m
> type instance (S n) :+ m = S (n :+ m)

> type family   (n :: Nat) :* (m :: Nat) :: Nat
> type instance  Z    :* m = Z
> type instance (S n) :* m = (n :* m) :+ m

note: other possible implementations
- which argument to recur on
- order of recursion and addition

name resolution
- Haskell has separate name spaces for types and values
- sometimes ambiguous
  - e.g., what doess `()` at type-level context mean
  - 1. Unit type of the kind `*
  - 2. Promoted type of the promoted kind `()`
- use `'` to resolve - indicates to 2
  - in type context, `'()` is promoted type from data constructor `()` and has kind `()`, and `()` is the unit type of the kind `*`.
  - `'[]` stands for the promoted *empty list* of kind `[k]`, and `[]` for the type constructor of the kind `* -> *`. The same convension applies to the alphabetical names.

GADTs
-----
How to control length in types?

usual data-type declarations
- type parameters shared in both sides
GADTs
- specify form of type parameter by explicitly specify constructor's type sigunature

> data Vector a n where
>   Nil  ::                    Vector a  Z
>   (:-) :: a -> Vector a n -> Vector a (S n)
>
> infixr 5 :-

`StandaloneDeriving`

> deriving instance Eq a => Eq (Vector a n)

operations on vectors

> toList :: Vector a n -> [a]
> toList       Nil = []
> toList (x :- xs) = x : toList xs

> instance Show a => Show (Vector a n) where
>   showsPrec d = showsPrec d . toList

> vhead :: Vector a (S n) -> a
> vhead (x :- _) = x

> vtail :: Vector a (S n) -> Vector a n
> vtail (_ :- xs) = xs

> vmap :: (a -> b) -> Vector a n -> Vector b n
> vmap _       Nil = Nil
> vmap f (x :- xs) = f x :- vmap f xs

> vappend :: Vector a n -> Vector a m -> Vector a (n :+ m)
> vappend      Nil  ys =                 ys
> vappend (x :- xs) ys = x :- vappend xs ys

1. how to pass type-level natural as function argument
2. how to pattern matching type-level natural to enable writing recursive functions

define data type carrying `Nat` as its parameter
- the structure of its data constructors reflect the one of corresponding type-level natural

> data SNat n where
>   SZ ::           SNat  Z
>   SS :: SNat n -> SNat (S n)

Now have way to pass type-level argument as function argument:

> vreplicate :: SNat n -> a -> Vector a n
> vreplicate  SZ    _ = Nil
> vreplicate (SS n) a = a :- vreplicate n a

> sLength :: Vector a n -> SNat n
> sLength       Nil = SZ
> sLength (_ :- xs) = SS (sLength xs)

Singleton for promoted types
--------------------------

for each type-level natural `n`, there is exactly one term with the type `SNat n`
- its structure is isomorphic to `n`
- `SNat Z` has `SZ` as its only inhabitant

Define operation between singlton types to treat the type-level arithmetic
- singleton function for natural addition `:+` :

> infixl 6 %:+
>
> (%:+) :: SNat n -> SNat m -> SNat (n :+ m)
> SZ   %:+ m =           m
> SS n %:+ m = SS (n %:+ m)

Exercise: define singleton function for the natural number multiplication `:*`
- care about recuring side and addition-multiplication order.

> class SingRep n where
>   sing :: SNat n

> instance SingRep Z where
>   sing = SZ

> instance SingRep n => SingRep (S n) where
>   sing = SS (sing :: SNat n)

> data SingInstance (n :: Nat) where
>   SingInstance :: SingRep n => SingInstance n

> singInstance :: SNat n -> SingInstance n
> singInstance  SZ    = SingInstance
> singInstance (SS n) =
>   case singInstance n of
>     SingInstance -> SingInstance

> vreplicate' :: forall a n. SingRep n => a -> Vector a n
> vreplicate' = vreplicate (sing :: SNat n)

previous exercise or copying from `sized-vector` package
- have `sLength` function to calculate `SNat k`
- can implement `vtranspose`

> vtranspose :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
> vtranspose                Nil = vreplicate' Nil
> vtranspose       (Nil :-   _) = Nil
> vtranspose ((x :- xs) :- xss) =
>   case singInstance (sLength xs) of
>     SingInstance -> (x :- vmap vhead xss) :- vtranspose (xs :- vmap vtail xss)

vtranspose ((1 :- 2 :- 3 :- Nil) :- (2 :- 3 :- 4 :- Nil) :- Nil)
=> [[1,2],[2,3],[3,4]]

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

> data Ordinal (n :: Nat) where
>   OZ ::              Ordinal (S n)
>   OS :: Ordinal n -> Ordinal (S n)

> sIndex :: Ordinal n -> Vector a n -> a
> sIndex  OZ    (x :-  _) = x
> sIndex (OS n) (_ :- xs) = sIndex n xs

> s09_e1 = sIndex (OS OZ) (0 :- 1 :- 2 :- 3 :- Nil)
> s09_e2 = sIndex     OZ  (0 :- 1 :- Nil)

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
