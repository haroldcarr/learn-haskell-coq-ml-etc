module Main where

import           Lib
------------------------------------------------------------------------------
import           Test.Hspec

main :: IO ()
main = hspec $ describe "NumArgs" $ do
  it "numArgs Foo" $ numArgs (Foo 10 20 30) `shouldBe` 3
  it "numArgs Bar" $ numArgs (Bar True)     `shouldBe` 1
