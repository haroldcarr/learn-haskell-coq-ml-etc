> {-# LANGUAGE GADTs                     #-}
>
> module Ch05_08_dynamic_types_02 where

from Ch05_08_dynamic_types_01

> data Dynamic where
>   Dyn :: Show t => Rep t -> t -> Dynamic
>
> instance Show Dynamic where
>   show (Dyn rep v) = showT rep v

> data Rep t where
>   RInt  :: Rep Int
>   RChar :: Rep Char
>   RList :: Show a => Rep a -> Rep [a]
>   RDyn  :: Rep Dynamic -- added compared to Ch05_07

> showT :: Show t => Rep t -> t -> String
>
> showT RInt i  = (show i) ++ " :: INT"
> showT RChar i = (show i) ++ " :: Char"
>
> showT (RList rep) [] = "THE END"
> showT (RList rep) (x:xs)
>    = (showT rep x) ++ ", " ++
>      (showT (RList rep) xs)
> showT RDyn (Dyn rep v) = showT rep v -- added compared to Ch05_07


> ch05_08_1_e1 = showT RInt 17
> ch05_08_1_e2 = showT (RList RInt) [12,13,14]
> ch05_08_1_e3 = showT (RList RDyn) [ Dyn RChar 'x', Dyn RInt 3 ]

Dynamic types have type info to enable type safe casting:

> toInt :: Dynamic -> Maybe Int
> toInt (Dyn RInt i) = Just i
> toInt (Dyn _ _)    = Nothing

> ch05_08_1_e4 = toInt (Dyn RInt 3)

See
- Fun with Phantom Types, Hinze
  - http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf,
- Generalized Algebraic Data Types in Haskell, Anton Dergunov
  - https://themonadreader.files.wordpress.com/2013/08/issue221.pdf

