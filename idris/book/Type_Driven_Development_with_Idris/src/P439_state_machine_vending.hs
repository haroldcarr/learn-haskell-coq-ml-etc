{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module P439_state_machine_vending where

import           Data.Kind

import           Data.Proxy
import           Data.Reflection hiding (Z)

data Nat = Z | S Nat deriving (Show)

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance Z   :+ n =         n
type instance S m :+ n = S (m :+ n)

data SNat :: Nat -> * where
  SZ ::           SNat  Z
  SS :: SNat n -> SNat (S n)

deriving instance Show (SNat nat)

snatToNat :: SNat n -> Nat
snatToNat  SZ    =  Z
snatToNat (SS n) = (S (snatToNat n))

{-
natToSNat  Z    = SZ
natToSNat (S n) = SS (natToSNat n)

data SNatOf = SNatOf (forall n . SNat n)
intToSNatOf :: Int -> SNatOf
intToSNatOf 0 = SNatOf 0
intToSNatOf n = let SNatOf x = intToSNatOf (n - 1) in
-}
{-
I can't get the tuple to work at type level (so unpacked versions below).
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

deriving instance Show (MachineCmd' any poundsBefore chocsBefore poundsAfter chocsAfter)

vend0 :: MachineCmd' () pounds chocs ('S pounds) chocs
vend0  = InsertCoin'

vend1 :: MachineCmd' () s1p s3c ('S ('S s1p)) s3c
vend1  = InsertCoin' `Bind'` InsertCoin'

vend2 :: MachineCmd' () ('S pounds) ('S chocs) pounds chocs
vend2  = Vend'

vend2' = vend0 `Bind'` vend2

vend3 :: MachineCmd' () ('S ('S s3p)) ('S ('S s3c)) s3p s3c
vend3  = Vend' `Bind'` Vend'

vend4 :: MachineCmd' () 'Z chocs 'Z ('S ('S chocs))
vend4  = Refill (SS (SS SZ))

vend5 :: MachineCmd' () 'Z ('S 'Z) 'Z 'Z
vend5  = InsertCoin' `Bind'` Vend'

vend6 :: MachineCmd' () 'Z 'Z 'Z ('S 'Z)
vend6  = Refill (SS (SS SZ)) `Bind'` InsertCoin' `Bind'` Vend'

vend7 :: MachineCmd' () 'Z chocs 'Z ('S ('S chocs))
vend7  = reify (SS (SS SZ)) $ \p -> Refill (reflect p)

{-
addNumbers = do
  "80"
  "60"
  "10"
  where (>>) = (++)
        return = \x -> x
-}
