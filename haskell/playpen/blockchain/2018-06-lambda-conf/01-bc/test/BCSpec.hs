{-# LANGUAGE NoImplicitPrelude #-}

module BCSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           BC

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

------------------------------------------------------------------------------
{-
:set -XOverloadedStrings
import           Data.List                            ((\\))
txsInChain = foldl (\txs b -> txs ++ bTXs b) []
myPool   = bcTXPool e1NotLost
myTXs    = txsInChain (bcChain e1NotLost)
theirTXs = txsInChain (bcChain e2NotLost)
myPool \\ theirTXs -- remove TXs from my pool that are in their chain
myTXs  \\ theirTXs -- add TXs from my chain that are not in their chain
-}
