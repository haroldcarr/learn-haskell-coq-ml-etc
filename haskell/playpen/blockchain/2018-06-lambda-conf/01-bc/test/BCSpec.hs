{-# LANGUAGE NoImplicitPrelude #-}

module BCSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           BC
import           GCoin (testIsValidCoin)

spec :: Spec
spec = do
  t01
  t02
  t03
  testMine
  testProofOfWork
  testEvidence
  testLongestChain1
  testLongestChain2
  testLongestChain3
  testLongestChainNegative
  testIsValidChain
  testIsValidBlock
  ----------------
  testIsValidTX
  testIsValidCoin isValidCoin addToChainBCSpec emptyChainBCSpec
  testMkUTXO addToChainBCSpec emptyChainBCSpec

