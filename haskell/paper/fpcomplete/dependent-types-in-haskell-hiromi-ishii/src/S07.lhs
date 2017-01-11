> {-# LANGUAGE GADTs, ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S07 where
>
> import Prelude hiding (tail, head, replicate, map)

previous exercise or copying from `sized-vector` package
- have `sLength` function to calculate `SNat k`
- can implement `transpose`

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

> data SingInstance (n :: Nat) where
>   SingInstance :: SingRep n => SingInstance n

> singInstance :: SNat n -> SingInstance n
> singInstance SZ     = SingInstance
> singInstance (SS n) =
>   case singInstance n of
>     SingInstance -> SingInstance

> sLength :: Vector a n -> SNat n
> sLength Nil = SZ
> sLength (_ :- xs) = SS $ sLength xs

> transpose :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
> transpose Nil = replicate' Nil
> transpose (Nil :- _) = Nil
> transpose ((x :- xs) :- xss) =
>   case singInstance (sLength xs) of
>     SingInstance -> (x :- map head xss) :- transpose (xs :- map tail xss)

transpose ((1 :- 2 :- 3 :- Nil) :- (2 :- 3 :- 4 :- Nil) :- Nil)
=> [[1,2],[2,3],[3,4]]
