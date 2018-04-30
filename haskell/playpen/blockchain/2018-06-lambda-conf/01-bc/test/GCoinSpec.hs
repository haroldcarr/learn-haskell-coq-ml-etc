module GCoinSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           GCoin

spec :: Spec
spec =
  testIsValidCoin isValidCoinTest addToChainTest emptyChainTest
