{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module P102_dependent_indexed_types_vehicle where

import GHC.Types

{-
Dependent data types.

GADTs define FAMILIES of TYPES.

Vehicle definition is defining two types
- Vehicle Pedal
- Vehicle Petrol
- power source is index of the Vehicle family : say which Vehicle type to use
-}

data PowerSource = Petrol | Pedal deriving Show

data Vehicle :: PowerSource -> Type where
  Bicycle ::        Vehicle Pedal
  Car     :: Int -> Vehicle Petrol
  Bus     :: Int -> Vehicle Petrol

instance Show (Vehicle a) where
  show Bicycle = "Bicycle"
  show (Car f) = "(Car " ++ show f ++ ")"
  show (Bus f) = "(Car " ++ show f ++ ")"

wheels :: Vehicle a -> Int
wheels Bicycle = 2
wheels (Car _) = 4
wheels (Bus _) = 4

fuel :: Vehicle Petrol -> Int
fuel (Car f) = f
fuel (Bus f) = f
-- fuel Bicycle = undefined
--     * Couldn't match type ‘'Petrol’ with ‘'Pedal’

refuel :: Vehicle Petrol -> Vehicle Petrol
refuel (Car _) = Car 100
refuel (Bus _) = Bus 200

refuel' :: Vehicle Petrol -> (Vehicle Petrol, Int)
refuel' c@(Car f) = let c' = refuel c in (c', fuel c' - f)
refuel' b@(Bus f) = let b' = refuel b in (b', fuel b' - f)

