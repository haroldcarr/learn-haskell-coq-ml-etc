> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeSynonymInstances  #-} -- TODO: why is this necessary?
> {-# LANGUAGE FlexibleInstances     #-} -- TODO: why is this necessary?
>
> module Ch05_11_multiparam_type_classes where

Abstracting/Generalizing type-classes

multiparameter type-classes: extend number of type parameters

regular type-classes (e.g., `Ord a`, `Monad a`) specifies a set of types

multiparameter classes specify a set of type relations

E.g., specify a relation between two type parameters

> class Coerce a b where
>   coerce :: a -> b
>
> instance Coerce Int String where
>   coerce = show
>
> instance Coerce Int [Int] where
>   coerce x = [x]

:t coerce
coerce :: Coerce a b => a -> b

states that coerce is function a -> b
- if a is coerce-able to b
- that is, if the relation (Coerce a b) exists

in this case, coerce will work for (Int -> String) and (Int -> [Int]).

but type inference doesn't work

compiler rejects

> -- ch05_11_e1 = coerce 12 :: String

have to add type annotations:

> ch05_11_e2 = coerce (12::Int) :: String
> ch05_11_e3 = coerce (12::Int) :: [Int]

that's the reason multiparameter type-classes did not make it into Haskell 98, despite being part of the GHC since 1997

solved via specifing relations between type parameters by way of functional dependencies (next)




