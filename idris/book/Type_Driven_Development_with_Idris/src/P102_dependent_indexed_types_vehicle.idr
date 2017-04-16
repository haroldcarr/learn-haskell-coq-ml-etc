module Main

%default total

data PowerSource = Petrol | Pedal

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
data Vehicle : PowerSource -> Type where
  Bicycle :                 Vehicle Pedal
  Car     : (fuel : Nat) -> Vehicle Petrol
  Bus     : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle _ -> Nat
wheels Bicycle    = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel Bicycle impossible
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
