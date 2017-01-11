> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE TypeOperators #-}
>
> module S04 where
>
> import S01

1. how to pass type-level natural as function argument
2. how to pattern matching type-level natural to enable writing recursive functions

define data type carrying `Nat` as its parameter
- the structure of its data constructors reflect the one of corresponding type-level natural

> data SNat n where
>   SZ ::           SNat  Z
>   SS :: SNat n -> SNat (S n)


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

Now have way to pass type-level argument as function argument:

> vreplicate :: SNat n -> a -> Vector a n
> vreplicate SZ     _ = Nil
> vreplicate (SS n) a = a :- vreplicate n a

> sLength :: Vector a n -> SNat n
> sLength       Nil = SZ
> sLength (_ :- xs) = SS (sLength xs)
