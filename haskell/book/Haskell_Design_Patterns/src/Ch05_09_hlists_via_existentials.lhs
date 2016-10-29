> {-# LANGUAGE ExistentialQuantification #-}
>
> module Ch05_09_hlists_via_existentials where

Earlier showed GADTs generalize phantom types and existentials.

HLists using existentials

> data LI_Eq1 = forall  a. LI_Eq1 a
>
> hListEq1 :: [LI_Eq1]
> hListEq1 = [LI_Eq1 3, LI_Eq1 "5"]

But, as shown earlier, can't do anything with this list.
e.g., to show list items must package a show function with each item

> data LI_Eq2 = forall  a. LI_Eq2 a (a -> String)
>
> hListEq2 :: [LI_Eq2]
> hListEq2 =  [ LI_Eq2  3  (show :: Int -> String)  -- type signatures here for clarity, but can be inferred
>             , LI_Eq2 "5" (show :: String -> String)
>             ]
>
> showEq2 (LI_Eq2 v showF) = showF v
>
> ch05_09_e1 = map showEq2 hListEq2

Passing `show` is a bad pattern.

Using type-classes makes the code more compact: instead of embedding functions, apply by constraining them to type-classes:

> -- type-class constraint specified in existential is called bounded quantification (bounded by type-class)
> data LI_Eq3 = forall a. Show a => LI_Eq3 a
>
> hListEq3 :: [LI_Eq3]
> hListEq3 =  [ LI_Eq3  3
>             , LI_Eq3 "5"
>             ]
>
> showEq3 (LI_Eq3 v) = show v
>
> ch05_09_e2 = map showEq3 hListEq3


