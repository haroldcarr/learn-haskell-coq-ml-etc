{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

module XSpec where

------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  =
  it "xxx"  $ True `shouldBe` True

