{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module P102_dependent_indexed_types_vehicle where

data PowerSource = Petrol | Pedal deriving Show

data Vehicle a where
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

refuel :: Vehicle Petrol -> Vehicle Petrol
refuel (Car _) = Car 100
refuel (Bus _) = Bus 200

refuel' :: Vehicle Petrol -> (Vehicle Petrol, Int)
refuel' c@(Car f) = (refuel c, fuel (refuel c) - f)
refuel' b@(Bus f) = (refuel b, fuel (refuel b) - f)

