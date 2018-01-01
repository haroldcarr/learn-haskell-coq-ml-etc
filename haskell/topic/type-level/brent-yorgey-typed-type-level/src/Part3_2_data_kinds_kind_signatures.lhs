> {-# LANGUAGE DataKinds      #-}
> {-# LANGUAGE GADTs          #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeFamilies   #-}
>
> module Part3_2_data_kinds_kind_signatures where

> data Nat = Z | S Nat -- DataKinds
>
> type family Plus (m :: Nat) (n :: Nat) :: Nat -- KindSignatures, TypeFamilies
> type instance Plus Z n = n
> type instance Plus (S m) n = S (Plus m n)
>
> data LOL :: Nat -> * -> * where  -- GADTs
>   KThxBye ::                 LOL  Z    a
>   Moar    :: a -> LOL n a -> LOL (S n) a
>
> append :: LOL m a -> LOL n a -> LOL (Plus m n) a
> append  KThxBye    v =                   v
> append (Moar x xs) v = Moar x (append xs v)
