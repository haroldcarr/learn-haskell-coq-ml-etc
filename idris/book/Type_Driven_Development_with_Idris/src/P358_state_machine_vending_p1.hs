{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}

module P358_state_machine_vending_p1 where

import           Data.Kind

import           Data.Proxy
import           Data.Reflection hiding (Z)

data Nat = Z | S Nat deriving (Eq, Read, Show) -- TODO: this complains on RebindableSyntax

type family (m :: Nat) :+ (n :: Nat) :: Nat
type instance Z   :+ n =         n
type instance S m :+ n = S (m :+ n)

data SNat :: Nat -> * where
  SZ ::           SNat  Z
  SS :: SNat n -> SNat (S n)

deriving instance Show (SNat nat)

{-
-- TODO: I can't get the tuple to work at type level (so unpacked versions below).
type VendState = '( Nat , Nat )

data MachineCmd' :: * -> VendState -> VendState -> * where
     InsertCoin' :: MachineCmd' () (  pounds,   chocs) (S pounds, chocs)
     Vend'       :: MachineCmd' () (S pounds, S chocs) (  pounds, chocs)
     GetCoins'   :: MachineCmd' () (  pounds,   chocs) (Z,        chocs)
     Bind'       :: MachineCmd' () state1              state2
                 -> MachineCmd' () state2              state3
                 -> MachineCmd' () state1              rstate3

vend :: MachineCmd' () (Z, S Z) (Z, Z)
vend  = InsertCoin' `Bind'` Vend'
-}

data Input = COIN | VEND | CHANGE | REFILL Nat deriving (Eq, Read, Show)

data MachineCmd ::            * ->  Nat ->     Nat -> Nat ->     Nat    -> * where
     InsertCoin :: MachineCmd ()    pounds     chocs  (S pounds) chocs
     Vend       :: MachineCmd () (S pounds) (S chocs)    pounds  chocs
     GetCoins   :: MachineCmd ()    pounds     chocs     Z       chocs
     -- Refill only valid if NO coins in machine.
     Refill     :: SNat bars
                -> MachineCmd ()    Z          chocs     Z       (bars :+ chocs)
     -- Display/GetInput do not effect state.
     Display    :: String
                -> MachineCmd ()    pounds     chocs     pounds  chocs
     GetInput   :: MachineCmd (Maybe Input)
                                    pounds     chocs     pounds  chocs
     Pure       :: r -- result
                -> MachineCmd r     pound      chocs     pounds  chocs
     Bind       :: MachineCmd ()    s1p        s1c       s2p     s2c
                -> MachineCmd ()    s2p        s2c       s3p     s3c
                -> MachineCmd ()    s1p        s1c       s3p     s3c

-- this works if Pure is commented out
-- deriving instance Show (MachineCmd any poundsBefore chocsBefore poundsAfter chocsAfter)

-- | Infinite sequence of machine state transitions.
-- The two Nats are the starting state of the machine.
data MachineIO :: Nat -> Nat -> * where
  Do :: MachineCmd a pb cb pa ca -> (a -> MachineIO pa ca)

-- | for do notation for infinite sequences of machine state transitions
machineDoBind :: MachineCmd a pb cb pa ca -> (a -> MachineIO pa ca)
machineDoBind  = Do

vend :: MachineIO p c
vend  = undefined

refill :: SNat bars -> MachineIO p c
refill  = undefined

vend0 :: MachineCmd () pounds chocs ('S pounds) chocs
vend0  = InsertCoin

vend1 :: MachineCmd () s1p s3c ('S ('S s1p)) s3c
vend1  = InsertCoin `Bind` InsertCoin

vend2 :: MachineCmd () ('S pounds) ('S chocs) pounds chocs
vend2  = Vend

vend2':: MachineCmd () s3p ('S s3c) s3p s3c
vend2' = vend0 `Bind` vend2

vend3 :: MachineCmd () ('S ('S s3p)) ('S ('S s3c)) s3p s3c
vend3  = Vend `Bind` Vend

vend4 :: MachineCmd () 'Z chocs 'Z ('S ('S chocs))
vend4  = Refill (SS (SS SZ))

vend5 :: MachineCmd () 'Z ('S 'Z) 'Z 'Z
vend5  = InsertCoin `Bind` Vend

vend6 :: MachineCmd () 'Z 'Z 'Z ('S 'Z)
vend6  = Refill (SS (SS SZ)) `Bind` InsertCoin `Bind` Vend

vend7 :: MachineCmd () 'Z chocs 'Z ('S ('S chocs))
vend7  = reify (SS (SS SZ)) $ \p -> Refill (reflect p)

vend8 :: SNat bars -> MachineCmd () 'Z chocs 'Z (bars :+ chocs)
vend8 n = reify n $ \p -> Refill (reflect p)

vend9 :: (bars :+ s1c) ~ 'S s3c
      => SNat bars
      -> MachineCmd () 'Z s1c 'Z s3c
vend9 n = reify n $ \p -> Refill (reflect p) `Bind` InsertCoin `Bind` Vend

vend10 :: MachineIO pa ca
vend10  = GetInput `machineDoBind` Just COIN
