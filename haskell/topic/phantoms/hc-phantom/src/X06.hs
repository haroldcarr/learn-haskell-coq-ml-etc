{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE StandaloneDeriving  #-}

module X06 where

import           Protolude
import           Test.Hspec

data Length u where
  Centimeters :: Double -> Length "centimeters"
  Inches      :: Double -> Length "inches"
deriving instance Eq   (Length u)
deriving instance Show (Length u)

oneHalfInch    :: Length "inches"
oneHalfInch     = Inches 0.5

fourCentimeters :: Length "centimeters"
fourCentimeters  = Centimeters 4.0

-- The 'Double' given to 'mkLen' is expected to be in centimeters.
class    MkLen a             where mkLen :: Double -> Length a
instance MkLen "centimeters" where mkLen   = Centimeters
instance MkLen "inches"      where mkLen d = Inches (d/2.54)

add0 :: MkLen a
     => Length "inches" -> Length "centimeters"
     -> Length a
add0 (Inches x) (Centimeters y) = mkLen (x + y)

x1 :: MkLen a => Length a
x1  = add0 oneHalfInch fourCentimeters
{-
-- No instance for (MkLen "ounces") arising from a use of ‘add0’
x2 :: Length "ounces"
x2  = add0 oneHalfInch fourCentimeters
-}
add :: MkLen c
    => Length a -> Length b
    -> Length c
add x y =
  let Centimeters x' = toCentimeters x
      Centimeters y' = toCentimeters y
   in mkLen (x' + y')

toCentimeters :: Length a -> Length "centimeters"
toCentimeters = \case
  la@Centimeters {} -> la
  Inches         a  -> Centimeters (a * 2.54)

ic = add oneHalfInch     fourCentimeters :: Length "inches"
cc = add fourCentimeters fourCentimeters :: Length "centimeters"

x06 :: Spec
x06  = describe "X06" $ do
  it "ic" $ ic `shouldBe` Inches      2.074803149606299
  it "cc" $ cc `shouldBe` Centimeters 8.0
