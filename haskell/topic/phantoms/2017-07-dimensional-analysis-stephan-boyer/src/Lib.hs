{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import           Protolude  hiding (Product)
import           Test.Hspec
{-
https://www.stephanboyer.com/post/131/type-safe-dimensional-analysis-in-haskell
Type safe dimensional analysis in Haskell
July 16, 2017

physical units as types

main idea : represent units of measure as function spaces
e.g., m/s encoded as Seconds -> Meters (numerator is return type, denominator is arg type)

function types can be composed to form, e.g., Seconds -> Seconds -> Meters (i.e., acceleration)

--------------------------------------------------
Units as function spaces
-}
-- (phantom) types for base units
data Meter
data Kilogram
data Second

-- represent quantities with units

-- for base units
-- phantom type 'a' tracks base unit (e.g., BaseQuantity Meter type represents length)
newtype BaseQuantity a = BaseQuantity Double

-- Quotient (e.g., m / s)  is BaseQuantity Second -> BaseQuantity Meter
type Quotient a b = b -> a

-- BaseQuantity for dimensionless quantities like 'pi'
type Dimensionless = BaseQuantity ()

-- multiplicative inverse a^-1 as the quotient 1 / a
type Inverse a = Quotient Dimensionless a

-- product a * b represented as a / b^-1:
type Product a b = Quotient a (Inverse b)

-- synonym to make square units like m^2 easier to read:
type Square a = Product a a

-- all quantities have some numeric value
class Quantity a where
  construct :: Double -> a
  destruct  :: a -> Double

-- instance for base quantities just wraps a Double:
instance Quantity (BaseQuantity a) where
  construct                 = BaseQuantity
  destruct (BaseQuantity x) = x

-- quotients of quantities are quantities
instance (Quantity q, Quantity r) => Quantity (q -> r) where
  construct x y = construct (x * destruct  y)
  destruct  x   = destruct  (x  (construct 1))

-- enables rearranging quotients
-- a / (b / c) = c / (b / a)
quotientAxiom
  :: (Quantity a, Quantity b, Quantity c)
  => Quotient a (Quotient b c)
  -> Quotient c (Quotient b a)
quotientAxiom = construct . destruct

-- arithmetic operations on units

infixl 6 .+.
(.+.) :: Quantity a => a -> a -> a
(.+.) x y = construct (destruct x + destruct y)

infixl 6 .-.
(.-.) :: Quantity a => a -> a -> a
(.-.) x y = construct (destruct x - destruct y)

infixl 7 .*.
(.*.) :: Quantity a => a -> b -> Product a b
(.*.) x y z = construct $ destruct (z y) * destruct x

infixl 7 ./.
(./.) :: (Quantity a, Quantity b) => a -> b -> Quotient a b
(./.) x y z = construct (destruct z * destruct x / destruct y)

--------------------------------------------------
-- Examples

type Length   = BaseQuantity Meter
type Mass     = BaseQuantity Kilogram
type Time     = BaseQuantity Second
type Area     = Square Length
type Velocity = Quotient Length Time

-- Example 1: Tracking units in types

-- calculate area of table

tableWidth :: Length
tableWidth  = construct 1.5

tableHeight :: Length
tableHeight  = construct 2.5

tableArea :: Area
tableArea  = tableWidth .*. tableHeight

-- mass of table:
tableMass :: Mass
tableMass  = construct 150
{-
-- Couldn't match type ‘Kilogram’ with ‘Meter’
tableAreaBad :: Area
tableAreaBad  = tableWidth .*. tableMass
-}

--------------------------------------------------
-- Example 2: Quantities as functions

-- distance, given velocity and duration
trainVelocity :: Velocity
trainVelocity  = construct 30

tripDuration :: Time
tripDuration  = construct 5000
{-
This demonstrates the correspondence between quantities and functions.
Velocity is a synonym for Length / Time, but it’s also a function from Time to Length.
Given a Time, apply" a Velocity to it to get a Length:
-}
tripDistance :: Length
tripDistance = trainVelocity tripDuration

-- multiplication of a / b by b is function application

--------------------------------------------------
-- Example 3: Manual proofs of unit equality

{-
Couldn't match type ‘Velocity -> Length’
                     with ‘BaseQuantity Second’
      Expected type: Time
        Actual type: Quotient Length Velocity
calculateDuration :: Length -> Velocity -> Time
calculateDuration distance velocity = distance ./. velocity

Haskell doesn’t know that Length / Velocity = Time : distance ./. velocity :: Length / Velocity

Velocity is a type synonym for Length / Time : distance ./. velocity :: Length / (Length / Time)

-- apply quotientAxiom to get: quotientAxiom (distance ./. velocity) :: Time / (Length / Length)

Under the interpretation of units as function spaces, we have:
                 quotientAxiom (distance ./. velocity) :: (Length -> Length) -> Time

apply id to cancel the Lengths and get a Time.

Putting it all together:
-}
calculateDuration :: Length -> Velocity -> Time
calculateDuration distance velocity = quotientAxiom (distance ./. velocity) identity

-- calculate how long Example 2 trip takes if train travels 40 m/s instead of 30 m/s

fasterVelocity :: Velocity
fasterVelocity  = construct 40

shorterDuration :: Time
shorterDuration  = calculateDuration tripDistance fasterVelocity

--------------------------------------------------
-- Example 4

x1 :: Spec
x1  = describe "destructing the results" $ do
  it "tableArea"       $ destruct tableArea       `shouldBe`      3.75 -- m^2
  it "tripDistance"    $ destruct tripDistance    `shouldBe` 150000.0  -- m
  it "shorterDuration" $ destruct shorterDuration `shouldBe`   3750.0  -- s
{-
--------------------------------------------------
Conclusion

type safe dimensional analysis by encoding units as function spaces

basic pattern:

Construct needed quantities using construct function.
Type annotation used to specify units (or type inference)

Do type safe operations on these quantities using .+., .*., etc.

Might need to rearrange units or cancel them to satisfy the type checker.
Possible if units are correct.

When calculation done, use destruct to convert them to Doubles.

CON: sometimes have to provide manual proofs of unit equivalence.
-}
