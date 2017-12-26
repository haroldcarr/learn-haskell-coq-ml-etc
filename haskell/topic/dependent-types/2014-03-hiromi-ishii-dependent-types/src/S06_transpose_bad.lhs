> {-# LANGUAGE GADTs, ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S06_transpose_bad where
>
> import Prelude hiding (tail, head, replicate, map)

Implicit argument and instance dictionary
-----------------------------------------

Sometimes vector length known from context: can omit singleton arg.

`singletons` `SingI` is constraint type-classes

-- | The singleton kind-indexed data family.
data family Sing (a :: k)

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI'.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

implicit arg version of `replicate`

> -- vreplicate' :: forall n a . SingI n => a -> Vector a n
> -- vreplicate' = vreplicate (sing :: SNat n)

`ScopedTypeVariables` enables refering to type-variable `n` in function type signature from function body.
- must quantify over all free variables occuring in type signature
- enables binding type variables with patterns, expression type signature and type-class and instance declarations
- http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/other-type-extensions.html#scoped-type-variables

`SingE (KindParam :: OfKind k)` converts singleton to original non-morphic type (e.g. `SNat n` to `Nat`)
- `KindParam` and `OfKind` :  proxy representing whole kind `k`.

`SingKind` and `SingInstance` data constructor:

If given vector empty (`m = Z`) return `n`-copies of `Nil`

> transpose :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
> transpose Nil = replicate' Nil
> transpose (Nil :- _) = Nil

Must pass `n` as implicit argument to `replicate'`: need the `SingRep n` constraint

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

> replicate :: SNat n -> a -> Vector a n
> replicate SZ     _ = Nil
> replicate (SS n) a = a :- replicate n a

> replicate' :: forall a n. SingRep n => a -> Vector a n
> replicate' = replicate (sing :: SNat n)

> head :: Vector a (S n) -> a
> head (x :- _) = x

> tail :: Vector a (S n) -> Vector a n
> tail (_ :- xs) = xs

> map :: (a -> b) -> Vector a n -> Vector b n
> map _ Nil       = Nil
> map f (x :- xs) = f x :- map f xs

> class SingRep n where
>   sing :: SNat n

> instance SingRep Z where
>   sing = SZ

> instance SingRep n => SingRep (S n) where
>   sing = SS (sing :: SNat n)

> transpose' :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
> transpose' Nil = replicate' Nil
> transpose' (Nil :- _) = Nil
> transpose' ((x :- xs) :- xss) =
>   (x :- map head xss) :- transpose' (xs :- map tail xss)

    • Could not deduce (SingRep n2) arising from a use of ‘transpose'’
      from the context: SingRep n
        bound by the type signature for:
                   transpose' :: SingRep n =>
                                 Vector (Vector a n) m -> Vector (Vector a m) n
      or from: m ~ 'S n1
        bound by a pattern with constructor:
                   :- :: forall a (n :: Nat). a -> Vector a n -> Vector a ('S n),
                 in an equation for ‘transpose'’
      or from: n ~ 'S n2
        bound by a pattern with constructor:
                   :- :: forall a (n :: Nat). a -> Vector a n -> Vector a ('S n),
                 in an equation for ‘transpose'’
      Possible fix:
        add (SingRep n2) to the context of the data constructor ‘:-’

GHC : "don't know the length of `xs`".

`transpose` signature requires `SingRep` instance for inner-vector's length, but GHC could not find it.

GHC suggests : "add constraint `SingRep n2`"
- impossible : can't refer to length of `xs` at type signature
- must construct dictionary for instance by hand and give to compiler

`SingKind` class and `SingInstance` data-type solves this problem

data SingInstance (n :: Nat) where
  SingInstance :: SingRep n => SingInstance n

singInstance :: SNat n -> SingInstance n
singInstance SZ     = SingInstance
singInstance (SS n) =
  case singInstance n of
    SingInstance -> SingInstance

version specialized to kind `Nat` for simplicity
`singletons` package provides more kind polymorphic version
and `singInstance` function is the member of `SingKind` type-class

`SingInstance n` is *witness* that there is `SingRep` instance for `n`.

With `SingInstance n` value, then can retrieve instance ditionary by pattern-matching.

function `singInstance` recurs on singleton to inductively retrieve the instance dictionary
- and save the witness into `SingInstance`

