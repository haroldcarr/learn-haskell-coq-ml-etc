{-# LANGUAGE OverloadedStrings #-}

module CommunicationSpec (spec) where

import           Util

import           Data.Aeson (decode, encode)
import           Test.Hspec

spec :: Spec
spec = do
  describe "host port partition" $
    let input      = ["0.0.0.0", "9061", "0.0.0.0", "9062", "0.0.0.0", "9063", "0.0.0.0", "9064"]
        pairOutput = [("0.0.0.0", 9061),("0.0.0.0",  9062),("0.0.0.0",  9063),("0.0.0.0",  9064)]
    in do
      it "mkHostPortPairs" $ mkHostPortPairs input `shouldBe` pairOutput
      it "mkPartition" $ mkPartition pairOutput
        `shouldBe` [([("0.0.0.0",9061)],[("0.0.0.0",9062),("0.0.0.0",9063),("0.0.0.0",9064)])
                   ,([("0.0.0.0",9062)],[("0.0.0.0",9061),("0.0.0.0",9063),("0.0.0.0",9064)])
                   ,([("0.0.0.0",9063)],[("0.0.0.0",9061),("0.0.0.0",9062),("0.0.0.0",9064)])
                   ,([("0.0.0.0",9064)],[("0.0.0.0",9061),("0.0.0.0",9062),("0.0.0.0",9063)])]
