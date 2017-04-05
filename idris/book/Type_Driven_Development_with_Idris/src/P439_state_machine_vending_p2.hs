{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RebindableSyntax   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE TypeOperators      #-}


module P439_state_machine_vending_p2 where

import           P439_state_machine_vending_p1

import           Data.Proxy
import           Data.Reflection               hiding (Z)
import           Prelude                       hiding ((>>), (>>=))

(>>) = Bind

v0 :: MachineCmd () pounds chocs ('S pounds) chocs
v0  = InsertCoin

v1 :: MachineCmd () s1p s3c ('S ('S s1p)) s3c
v1  = do
  InsertCoin
  InsertCoin

v2 :: MachineCmd () ('S pounds) ('S chocs) pounds chocs
v2  = Vend

v2':: MachineCmd () s3p ('S s3c) s3p s3c
v2' = do
  vend0
  vend2

v3 :: MachineCmd () ('S ('S s3p)) ('S ('S s3c)) s3p s3c
v3  = do
  Vend
  Vend

v4 :: MachineCmd () 'Z chocs 'Z ('S ('S chocs))
v4  = Refill (SS (SS SZ))

v5 :: MachineCmd () 'Z ('S 'Z) 'Z 'Z
v5  = do
  InsertCoin
  Vend

v6 :: MachineCmd () 'Z 'Z 'Z ('S 'Z)
v6  = do
  Refill (SS (SS SZ))
  InsertCoin
  Vend

v7 :: MachineCmd () 'Z chocs 'Z ('S ('S chocs))
v7  = reify (SS (SS SZ)) $ \p -> Refill (reflect p)

v8 :: SNat bars -> MachineCmd () 'Z chocs 'Z (bars :+ chocs)
v8 n = reify n $ \p -> Refill (reflect p)

v9 :: (bars :+ s1c) ~ 'S s3c
   => SNat bars
   -> MachineCmd () 'Z s1c 'Z s3c
v9 n = reify n $ \p -> do
  Refill (reflect p)
  InsertCoin
  Vend

