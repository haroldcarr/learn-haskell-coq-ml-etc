{-# LANGUAGE ScopedTypeVariables #-}

import           Assert
import           Lib
import           TestUtil

import           Control.Exception (SomeException, evaluate, try)
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Assertions" $ do

    -- this could pass test but be wrong exception or message
    it "anyException" $ do
      evaluate failWithMsg `shouldThrow` anyException

    -- this reports correctly when all conditions are true
    it "assert1 - expected line/col/hash and msg" $ do
      r <- try (evaluate failWithMsg)
      r `shouldSatisfy` assertionFailureContaining1 "failWithMsg"

    -- this reports failure, but can't tell if it is the wrong message or no line/col/hash
    it "assert1 - should fail because no line/col/hash" $ do
      r <- try (evaluate failWithoutMsg)
      r `shouldSatisfy` assertionFailureContaining1 "failWithoutMsg"

    -- this reports failure, but can't tell if it is the wrong message or no line/col/hash
    it "assert1 - should fail because wrong message" $ do
      r <- try (evaluate failWithMsg)
      r `shouldSatisfy` assertionFailureContaining1 "should fail"

    -- the following are more fine-grained to tell why something failed

    it "assert2 - FNN because not an exception" $ do
      let r' = assertionFailureContaining2 "foo" (Right 3)
      r' `shouldSatisfy` (\x -> x == (False, Nothing, Nothing))

    it "assert2 - TFN because does not contain line/col/hash" $ do
      r <- try (evaluate failWithoutMsg)
      let r' = assertionFailureContaining2 "foo" r
      r' `shouldSatisfy` (\x -> x == (True, Just False, Nothing))

    it "assert2 - TTF because wrong message" $ do
      r <- try (evaluate failWithMsg)
      let r' = assertionFailureContaining2 "foo" r
      r' `shouldSatisfy` (\x -> x == (True, Just True, Just False))

    it "assert2 - TTT because excepted exception, line/col/hash and msg" $ do
      r <- try (evaluate failWithMsg)
      let r' = assertionFailureContaining2 "failWithMsg" r
      r' `shouldSatisfy` (\x -> x == (True, Just True, Just True))
