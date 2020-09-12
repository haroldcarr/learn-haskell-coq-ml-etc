> {-# LANGUAGE GADTs #-}
>
> module Ch05_10_hlists_via_gadts where

Can express HLists in same two styles used with existentials.

1st style : pass `show`

> data LI_Gadt1 where
>   {MkShow1 :: a -> (a -> String) -> LI_Gadt1}
>
> hListGadt1 :: [LI_Gadt1]
> hListGadt1 = [ MkShow1 "3" show, MkShow1 5 show]
>
> showGadt1 (MkShow1 v showF) = showF v
>
> ch05_10_e1 = map showGadt1 hListGadt1

2nd style bounded quantification:

> data LI_Gadt2 where
>   {MkShow2 :: Show a => a -> LI_Gadt2}
>
> hListGadt2 :: [LI_Gadt2]
> hListGadt2 = [MkShow2 "3", MkShow2 5]
>
> showGadt2 (MkShow2 v) = show v
>
> ch05_10_e2 = map showGadt2 hListGadt2
