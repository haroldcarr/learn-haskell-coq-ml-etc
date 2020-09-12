> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE ExistentialQuantification #-}
>
> module Ch05_08_dynamic_types_01 where
>
> import Ch05_07_typecase
>
> import Test.HUnit      as U
> import Test.HUnit.Util as U

> -- uses existential quantification for `t`
> data DynamicExQuan =
>     forall t. Show t => DynExQuan (Rep t) t -- Rep from Ch05_07_typecase

`DynExQuan` values have opaque type, but are well typed.

Use to create heterogeneous lists:

> dynExQuanList = [ DynExQuan RChar 'x'
>                 , DynExQuan RInt  3
>                 ]
>
> showDynExQuan (DynExQuan rep v) = showT rep v

But lists are in a different universe, so use regular `map`:

> ch05_08_1_e1 = U.t "ch05_08_1_e1"
>   (map showDynExQuan dynExQuanList)
>   ["'x' :: Char","3 :: INT"]

Since GADTs generalize existentials, can also write a "dynamic GADT":

> data Dynamic where
>   Dyn :: Show t => Rep t -> t -> Dynamic
>
> instance Show Dynamic where
>   show (Dyn rep v) = showT rep v

Use to create heterogeneous lists:

> dynList :: [Dynamic]
> dynList = [ Dyn RChar 'x'
>           , Dyn RInt 3
>           ]
>

`Show Dynamic` acts on dynamic values

generic `showT` acts on "generic data"

But lists are in a different universe, so:

> ch05_08_1_e2 = U.t "ch05_08_1_e2"
>   (map show dynList)
>   ["'x' :: Char","3 :: INT"]

See next for adding list in same universe.

> runTests_Ch05_08_01 = runTestTT $ TestList $ ch05_08_1_e1 ++ ch05_08_1_e2
