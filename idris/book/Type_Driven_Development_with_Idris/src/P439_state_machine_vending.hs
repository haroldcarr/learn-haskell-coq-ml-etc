{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module P439_state_machine_vending where

import           Data.Kind

data Nat = Z | S Nat deriving (Show)

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance Z   :+ n =         n
type instance S m :+ n = S (m :+ n)

data SNat :: Nat -> * where
  SZ ::           SNat  Z
  SS :: SNat n -> SNat (S n)

snatToNat :: SNat n -> Nat
snatToNat SZ     = Z
snatToNat (SS n) = (S (snatToNat n))

{-
data SNatOf = SNatOf (forall n . SNat n)
intToSNatOf :: Int -> SNatOf
intToSNatOf 0 = SNatOf 0
intToSNatOf n = let SNatOf x = intToSNatOf (n - 1) in
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
     Refill      :: SNat bars
                 -> MachineCmd' ()    Z          chocs     Z       (bars :+ chocs)
     Bind'       :: MachineCmd' ()    s1p        s1c       s2p     s2c
                 -> MachineCmd' ()    s2p        s2c       s3p     s3c
                 -> MachineCmd' ()    s1p        s1c       s3p     s3c

vend1 :: MachineCmd' () Z (S Z) Z Z
vend1  = InsertCoin' `Bind'` Vend'

vend2 :: MachineCmd' () Z Z Z (S Z)
vend2  = Refill (SS (SS SZ)) `Bind'` InsertCoin' `Bind'` Vend'

{-
addNumbers = do
  "80"
  "60"
  "10"
  where (>>) = (++)
        return = \x -> x
-}
