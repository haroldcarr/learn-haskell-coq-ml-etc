{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module X04 where

import           Data.Proxy
import           Data.Tagged
import           Data.Typeable
import           GHC.TypeLits
import           Protolude
import           Test.Hspec

xx :: [Tagged "m" Double] -> Tagged "m" Double
xx  = sum

m :: Tagged "m" Double
m  = Tagged 10.0

s :: Tagged "s" Double
s  = Tagged 20.0

addUnitsAny
  :: Num a
  => Tagged u1 a
  -> Tagged u2 a
  -> Tagged u1 a
addUnitsAny (Tagged x) (Tagged y) = Tagged (x + y)

x04_1 :: Spec
x04_1  = describe "X04_1" $ do
  it "   au m s" $
            addUnitsAny m s  `shouldBe`
    Tagged 30.0
  it "   au s m" $
            addUnitsAny s m  `shouldBe`
    Tagged 30.0

  it "to au m s" $
    typeOf (addUnitsAny m s) `shouldBe`
    typeOf              m
  it "to au s m" $
    typeOf (addUnitsAny s m) `shouldBe`
    typeOf              s

i :: Tagged "inches"      Double
i  = Tagged 10.0

c :: Tagged "centimeters" Double
c  = Tagged 20.0

divUnitsAny
  :: Fractional a
  => Tagged u1 a
  -> Tagged u2 a
  -> Tagged (u1 `AppendSymbol` u2) a
divUnitsAny (Tagged x) (Tagged y) = Tagged (x / y)

x04_3 :: Spec
x04_3  = describe "X04_3" $ do
  it "   au m s" $
            divUnitsAny m s  `shouldBe`
    Tagged 0.5
  it "   au s m" $
            divUnitsAny s m  `shouldBe`
    Tagged 2.0

  it "to au m s" $
    typeOf (divUnitsAny m s) `shouldBe`
    typeRep (Proxy::Proxy (Tagged "ms" Double))
  it "to au s m" $
    typeOf (divUnitsAny s m) `shouldBe`
    typeRep (Proxy::Proxy (Tagged "sm" Double))

addUnitsInches
  :: Num a
  => Tagged "inches" a
  -> Tagged "inches" a
  -> Tagged "inches" a
addUnitsInches = addUnitsAny

x04_2 :: Spec
x04_2  = describe "X04_2" $ do
  it "   au i i" $
            addUnitsInches i i  `shouldBe`
    Tagged 20.0
  it "to au i i" $
    typeOf (addUnitsInches i i) `shouldBe`
    typeOf                 i
-- Couldn't match type ‘"centimeters"’ with ‘"inches"’
--it "   au i c" $         addUnitsInches i c  `shouldBe` Tagged 30.0

