{-# LANGUAGE OverloadedStrings #-}

module CommunicationSpec (spec) where

import           Data.Aeson (decode, encode)
import           Test.Hspec

spec :: Spec
spec = do
  describe "placeholder" $ do
    it "placeholder" $ True `shouldBe` True
