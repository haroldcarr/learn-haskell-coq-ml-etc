> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S01 where

already implemented in
- `type-natural` http://hackage.haskell.org/package/type-natural
- `sized-vector` http://hackage.haskell.org/package/sized-vector

Sized-index Vectors

`Vector` type *depends* on its size *value*.

*simulate* dependent types in Haskell

Type-level naturals
--------------------

**Peano numerals** : http://www.haskell.org/haskellwiki/Peano_numbers

> -- value-level
> data Nat = Z | S Nat deriving (Eq, Show)

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

> type family   Plus (n :: Nat) (m :: Nat) :: Nat
> type instance Plus  Z    m =           m
> type instance Plus (S n) m = S (Plus n m)

`TypeOperators` extension: enables infix type operator

> infixl 6 :+
>
> type family   (n :: Nat) :+ (m :: Nat) :: Nat
> type instance  Z    :+ m =         m
> type instance (S n) :+ m = S (n :+ m)

To implement multiplication need `UndecidableInstances`

> infixl 7 :*
>
> type family   (n :: Nat) :* (m :: Nat) :: Nat
> type instance Z     :* m = Z
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
> -- deriving instance Show a => Show (Vector a n)

operations on vectors

> toList :: Vector a n -> [a]
> toList       Nil = []
> toList (x :- xs) = x : toList xs

> instance Show a => Show (Vector a n) where
>   showsPrec d = showsPrec d . toList

> vhead :: Vector a (S n) -> a
> vhead (x :- _) = x

> tail :: Vector a (S n) -> Vector a n
> tail (_ :- xs) = xs

> vappend :: Vector a n -> Vector a m -> Vector a (n :+ m)
> vappend      Nil  ys =                 ys
> vappend (x :- xs) ys = x :- vappend xs ys

Exercise:
- answers in `sized-vector` : http://hackage.haskell.org/package/sized-vector
1. Implement the `toList` and `fromList`.
2. Implement the `Vector` version of `map`, `uncons`, `init` and `last`.
3. Implement the `zipWithSame`, with the following type signature:
   ```haskell
   zipWithSame :: (a -> b -> c) -> Vector a n -> Vector b n -> Vector c n
   ```
   This is the version of the `zipWith` for vectors with the same length.
4. Implement the `min` function for type-level natural numbers.
   Use it to implement `zipWith` which takes vectors with possibly different length.

