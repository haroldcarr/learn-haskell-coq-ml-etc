{-# LANGUAGE NoImplicitPrelude #-}

module BCSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           BC
import           GCoin (testIsValidCoin)

spec :: Spec
spec = do
  testAddTxToPool
  testMine
  testProofOfWork
  testEvidence
  testLongestChain
  testIsValidChain
  testIsValidBlock
  ----------------
  testIsValidTX
  testIsValidCoin isValidCoin addToChainBCSpec emptyChainBCSpec
  testMkUTXO addToChainBCSpec emptyChainBCSpec

