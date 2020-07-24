{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module XSpec where

import           Gauge
import           GaugeUse
import           MissionControl
import           MissionControlUse
import           ScratchPad.FuelLevel
import           ScratchPad.Sorted
------------------------------------------------------------------------------
import           Test.Hspec

spec :: Spec
spec  = describe "xx" $ do
  chkMF1
  chkMF2
  chkD
  chkDMF
  fl0t
  fl1t
  fl2t
  g1t
  gc1t
  s0t
  gaugeUse
  rocket

gaugeUse :: Spec
gaugeUse  = describe "gauge use" $ do
  mkGood "l1g"     l1g     10.0
  mkBad  "l1ge"    l1ge    "ExceedsMaxFlow"
  --mkBad  "l2b"  l2b  "NotDecr" -- infinite loop
  mkBad  "l2ge"    l2ge    "NotDecr"
  mkBad  "ldslb"   ldslb   "Prelude.head: empty list"
  mkGood "ldsneg"  ldsneg  10.0
  mkBad  "ldsnebb" ldsnebb "NotDecr"
  mkGood "lNTg"    lNTg    10.0
  mkGood "lPTg"    lPTg    10.0
  mkGood "lRTg"    lRTg    10.0
  mkGood "lRTwSg"  lRTwSg  (10.0,   0.33521595799981796)
  mkGood "lOOPg1"  lOOPg1  (10.0,   0.33521595799981796)
  mkGood "lOOPg2"  lOOPg2  (10.0, 110.79432945026694)
 where
  mkGood :: (Show a, Eq a) => String -> IO a -> a -> Spec
  mkGood name f v = do x <- runIO f; it name $ x `shouldBe` v

  mkBad :: String -> IO a -> String -> Spec
  mkBad name f e = it name $ f `shouldThrow` errorCall e

rocket :: Spec
rocket  = describe "rocket" $
  it "countdown" $ countdown mkMissionControl `shouldBe` "Rocket Launched!"

