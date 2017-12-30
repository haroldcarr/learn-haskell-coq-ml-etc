{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators  #-}

module P104_vect where

import Data.Finite
import Data.Proxy
import GHC.Types
import GHC.TypeNats
import GHC.TypeLits.Witnesses

infixr 7 :>

data Vec :: Nat -> Type -> Type where
  V0   ::                 Vec  0      a
  (:>) :: a -> Vec n a -> Vec (n + 1) a
{-
instance Eq a  => Eq (Vec n a) where
  (==)        V0       V0  = True
  (==) (l :> ls) (r :> rs) = l == r && ls == rs
-}
instance Show a => Show (Vec n a) where
  show V0        = "V0"
  show (l :> ls) = show l ++ " :> " ++ show ls

t1 = 3 :> 2 :> 1 :> V0

v :: forall a m n. (KnownNat m, KnownNat n) => Vec m a -> Vec n a -> Proxy (m + n)
-- v xs ys = withNatOp (%+) (Proxy @n) (Proxy @m) $ Proxy::Proxy (m + n)
v _ _ = Proxy::Proxy (m + n)

{-
vappend :: forall a m n. (KnownNat m, KnownNat n) => Vec m a -> Vec n a -> Vec (m + n) a
vappend       V0  ys =                 ys
vappend (x :> xs) ys =
  withNatOp (%+) (Proxy @n) (Proxy @m) $ natVal (Proxy::Proxy (m + n))

append : Vec n elem -> Vec m elem -> Vec (n + m) elem
append       V0  ys = ys
append (x :> xs) ys = x :> append xs ys

zip : Vec n a -> Vec n b -> Vec n (a, b)
zip       V0        V0  = V0
zip (x :> xs) (y :> ys) = (x, y) :> zip xs ys

{-
Fin n : unsigned number with non-inclusive upper bound of n.
name "Fin" says number is finitely bounded.

FZ and FS are constructors of Fin, corresponding to Z and S Nat constructors.
Can use numeric literals, as with Nat.
-}
index : Fin n -> Vec n a -> a
index  FZ    (x :>  _) = x
index (FS i) (_ :> ys) = index i ys

tryIndex' : Integer -> Vec n a -> Maybe a
tryIndex' _       V0  = Nothing
tryIndex' 0 (x :>  _) = Just x
tryIndex' i (_ :> xs) = tryIndex' (i - 1) xs

tryIndex : Integer -> Vec n a -> Maybe a
tryIndex {n} i xs =
    case integerToFin i n of
        Nothing    => Nothing
        (Just idx) => Just (index idx xs)

vectTake : (m : Nat) -> Vec (m + n) a -> Vec m a
vectTake  Z           _  = V0
vectTake (S k) (x :> xs) = x :> vectTake k xs

tvt : Vec (S Z) Integer
tvt = vectTake (S Z) (1:>2:>3:>4:>5:>6:>7:>V0)


-----------------------



The type-level numbers in vappend are compiletime: flow of control determined from constructors of first vector.

> t2 = U.t "t2"
>    (vappend (6 :> 5 :> 4 :> V0) (3 :> 2 :> 1 :> V0))
>    (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0)

Sometimes numbers needed at runtime. E.g., chopping a vector in two:

vchop :: Vec (m :+ n) x -> (Vec m x, Vec n x)

Need m at runtime.

But Haskell’s dependent ∀ quantifier is for implicit and static things.

Standard solution : define runtime replica of static data as a singleton GADT.

> data Natty :: Nat -> * where
>   Zy :: Natty Z
>   Sy :: Natty n -> Natty (S n)

Each type level n in Nat kind has corresponding type Natty n.

The ‘Π-types’, often written (x : S) -> T, of dependent type theory abstract dependently over explicit dynamic things.

Simulate in Haskell by abstracting dependently at the type level and non-dependently over singleton representative.

> vchop :: Natty m
>       -> Vec (m :+ n) a
>       -> (Vec m a, Vec n a)
> vchop  Zy          xs  = (V0     , xs)
> vchop (Sy k) (x :> xs) = (x :> ys, zs)
>   where (ys, zs) = vchop k xs

> t3 = U.t "t3"
>    (vchop (Sy (Sy Zy)) (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0))
>    ( 6 :> 5 :> V0
>    , 4 :> 3 :> 2 :> 1 :> V0
>    )

To write:

vtake :: Nat -> Vec (m :+ n) a -> Vec m a
vtake  Z          xs  = V0
vtake (S k) (x :> xs) = x :> vtake k xs

Need to tell GHC how to instantiate n in the recursive call
(to reason about addition : (m :+ ) is injective.
- To GHC, it is just an unknown axiomatised function.
- Not a problem in vchop, because relaying the suffix, zs, from the recursive output to the result makes clear that the same n is needed in both places.
- n is not needed at run time, but without it there is no way to see that the program makes sense.
- summary: there are data that, despite being static, must be made explicit. One way : ‘proxy types’

PolyKinds : TODO

> data Proxy :: k -> * where
>   Proxy :: Proxy i

> vtake :: Natty m -> Proxy n -> Vec (m :+ n) a -> Vec m a
> vtake  Zy    n       xs  = V0
> vtake (Sy m) n (x :> xs) = x :> vtake m n xs

> t4 = U.t "t4"
>    (vtake (Sy (Sy Zy))
>           (Proxy :: Proxy anything)
>           (6 :> 5 :> 4 :> 3 :> 2 :> 1 :> V0))
>    (6 :> 5 :> V0)


-}
