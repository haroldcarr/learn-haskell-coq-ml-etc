{-# LANGUAGE OverloadedStrings #-}

module CommunicationSpec (spec) where

import           Util

import           Data.Aeson (decode, encode)
import           Test.Hspec

spec :: Spec
spec = do
  describe "host port partition" $ do
    it "mkHostPortPairs" $
      mkHostPortPairs ["0.0.0.0", "9061", "0.0.0.0", "9062", "0.0.0.0", "9063"]
      `shouldBe`      [("0.0.0.0", 9061),("0.0.0.0",  9062),("0.0.0.0",  9063)]
    it "mkPartition" $ mkPartition [("0.0.0.0",9061),("0.0.0.0",9062),("0.0.0.0",9063)]
      `shouldBe` [([("0.0.0.0",9061)],[("0.0.0.0",9062),("0.0.0.0",9063)])
                 ,([("0.0.0.0",9062)],[("0.0.0.0",9061),("0.0.0.0",9063)])
                 ,([("0.0.0.0",9063)],[("0.0.0.0",9061),("0.0.0.0",9062)])]
