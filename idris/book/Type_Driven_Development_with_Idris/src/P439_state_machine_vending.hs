{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module P439_state_machine_vending where

import           Data.Kind

data Nat = Z | S Nat

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance Z   :+ n =         n
type instance S m :+ n = S (m :+ n)

data Natty :: Nat -> * where
  Zy ::            Natty  Z
  Sy :: Natty n -> Natty (S n)
{-
data NattyOf = NattyOf (forall n . Natty n)
intToNattyOf :: Int -> NattyOf
intToNattyOf 0 = NattyOf 0
intToNattyOf n = let NattyOf x = intToNattyOf (n - 1) in
-}
{-
type VendState = '('Nat, 'Nat)

data MachineCmd :: * -> VendState -> VendState -> * where
     InsertCoin :: MachineCmd () (  pounds,   chocs) (S pounds, chocs)
     Vend       :: MachineCmd () (S pounds, S chocs) (  pounds, chocs)
     GetCoins   :: MachineCmd () (  pounds,   chocs) (Z,        chocs)
     Bind       :: MachineCmd () state1              state2
                -> MachineCmd () state2              state3
                -> MachineCmd () state1              rstate3

vend :: MachineCmd () (Z, S Z) (Z, Z)
  InsertCoin `Bind` Vend
-}

data MachineCmd' :: * -> Nat -> Nat -> Nat -> Nat -> * where
     InsertCoin' :: MachineCmd' ()    pounds     chocs  (S pounds) chocs
     Vend'       :: MachineCmd' () (S pounds) (S chocs)    pounds  chocs
     GetCoins'   :: MachineCmd' ()    pounds     chocs     Z       chocs
     Refill      :: Natty bars
                 -> MachineCmd' ()    Z          chocs     Z       (bars :+ chocs)
     Bind'       :: MachineCmd' ()    s1p        s1c       s2p     s2c
                 -> MachineCmd' ()    s2p        s2c       s3p     s3c
                 -> MachineCmd' ()    s1p        s1c       s3p     s3c

vend1 :: MachineCmd' () Z (S Z) Z Z
vend1  = InsertCoin' `Bind'` Vend'

vend2 :: MachineCmd' () Z Z Z (S Z)
vend2  = Refill (Sy (Sy Zy)) `Bind'` InsertCoin' `Bind'` Vend'

{-
addNumbers = do
  "80"
  "60"
  "10"
  where (>>) = (++)
        return = \x -> x
-}
