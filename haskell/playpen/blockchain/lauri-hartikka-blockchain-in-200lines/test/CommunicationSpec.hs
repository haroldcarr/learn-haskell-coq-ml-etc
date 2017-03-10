{-# LANGUAGE OverloadedStrings #-}

module CommunicationSpec (spec) where

import           Util

import           Data.Aeson (decode, encode)
import           Test.Hspec

spec :: Spec
spec = do
  describe "host port partition window" $
    let input        = [  "0.0.0.0", "9061",   "0.0.0.0", "9062", "0.0.0.0", "9063", "0.0.0.0", "9064"]
        pairOut      = [ ("0.0.0.0",  9061),  ("0.0.0.0",  9062),("0.0.0.0",  9063),("0.0.0.0",  9064)]
        partitionOut = ([("0.0.0.0",  9061)],[("0.0.0.0",  9062),("0.0.0.0",  9063),("0.0.0.0",  9064)])
        windowsOut   = [[("0.0.0.0",9062),("0.0.0.0",9063)],[("0.0.0.0",9063),("0.0.0.0",9064)]]
    in do
      it "mkHostPortPairs" $ mkHostPortPairs input              `shouldBe` pairOut
      it "mkPartition"     $ mkPartition (head pairOut) pairOut `shouldBe` partitionOut
      it "window"          $ windows 2 (snd partitionOut)       `shouldBe` windowsOut
