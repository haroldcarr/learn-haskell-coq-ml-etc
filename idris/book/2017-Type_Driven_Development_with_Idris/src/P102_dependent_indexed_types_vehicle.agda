module P102_dependent_indexed_types_vehicle where

open import Data.Nat
open import Data.Product

Nat = ℕ

data PowerSource : Set where
  Petrol Pedal : PowerSource

{-
4.2 Defining dependent data types
---------------------------------

A dependent data type is a *type* computed from a value.

E.g., : Vect : type of Vect depends on its length.

core idea : because there is no syntactic distinction between types and expressions,
            types can be computed from any expression.

Families of types
-----------------

Vehicle : defines two (i.e., "family") types in one declaration : Vehicle Pedal and Vehicle Petrol.
Defining multiple related types at the same time.
Power source is an index of the Vehicle family.
The index tells you exactly which Vehicle type you mean.
-}
data Vehicle : PowerSource -> Set where
  Bicycle :                 Vehicle Pedal
  Car     : (fuel : Nat) -> Vehicle Petrol
  Bus     : (fuel : Nat) -> Vehicle Petrol

wheels : {ps : PowerSource} -> Vehicle ps -> Nat
wheels Bicycle    = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

fuel : Vehicle Petrol -> Nat
fuel (Car f) = f
fuel (Bus f) = f

refuel : Vehicle Petrol -> Vehicle Petrol
--refuel Bicycle ()
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

refuel' : Vehicle Petrol -> (Vehicle Petrol × Nat)
refuel' c@(Car f) = let c' = refuel c in (c' , fuel c' ∸ f)
refuel' b@(Bus f) = let b' = refuel b in (b' , fuel b' ∸ f)
