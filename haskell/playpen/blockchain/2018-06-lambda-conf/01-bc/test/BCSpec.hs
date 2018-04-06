{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BCSpec where

import           Prelude    ((!!))
import           Protolude
import           Test.Hspec
------------------------------------------------------------------------------
import           BC

spec = do
  testAddTransaction
  testMine
  testProofOfWork
  testEvidence
  testResolveConflicts
  testIsValidChain

------------------------------------------------------------------------------
testAddTransaction =
  describe "testAddTransaction" $do
    it "empty bcTXPool in initialState" $
      initialBCState
      `shouldBe`
      BCState { bcTXPool = []
              , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
              , bcProofDifficulty = 4}
    it "adds to bcTXPool" $
      addTransaction initialBCState "TX1"
      `shouldBe`
      BCState { bcTXPool = ["TX1"]
              , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
              , bcProofDifficulty = 4}

------------------------------------------------------------------------------
testMine =
  describe "testMine" $do
    it "addBlock" $
      let (s, _) = mine (addTransaction (addTransaction initialBCState "TX1") "TX2")
       in s `shouldBe`
          BCState { bcTXPool = []
                  , bcChain = [Block { bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}
                              ,Block { bPrevHash = "\202\169KU\139\ETX\212\&3W\145`\229\224S\159\177\253\nF\167\158\227\250\255\244\v\207\228z\233\171\237"
                                     , bIndex = 1
                                     , bTXs = ["TX1","TX2"]
                                     , bProof = 134530}]
                  , bcProofDifficulty = 4}

------------------------------------------------------------------------------
(bcTX1,_) = mine (addTransaction initialBCState "TX1")

testProofOfWork =
  describe "proofOfWork" $ do
    it "from genesisBlock" $
      proofOfWork 4 genesisBlock `shouldBe`  bProof (bcChain bcTX1 !! 1)
    it "from genesisBlock + 1" $
      proofOfWork 4 (bcChain bcTX1 !! 1) `shouldBe` 52668

------------------------------------------------------------------------------
testEvidence =
  describe "evidence" $ do
    it "from genesisBlock" $
      evidence (bProof genesisBlock) (hashBlock genesisBlock) (proofOfWork 4 genesisBlock)
      `shouldBe`
      "0000DDF9FB09F9A9C5A0EF57DC3E2916633BEDB95B38D54BDBFFF0B7D4D6E515"
    it "from genesisBlock + 1" $
      evidence (bProof (bcChain bcTX1 !! 1)) (hashBlock (bcChain bcTX1 !! 1))  52668
      `shouldBe`
      "000072456DC7CC3975C0CC3543B6BA201E6F4D056679970C3644D2DDEB4EEA67"

------------------------------------------------------------------------------
eTX0 :: BCState
(eTX0,_) = mine (addTransaction initialBCState "TX-0")

eLongerChainAndPoolUpdateIn :: BCState
eLongerChainAndPoolUpdateIn = initialBCState { bcTXPool = ["TX-0","TX-should-stay"] }

e1LongerChainAndPoolUpdateOut :: BCState
e1LongerChainAndPoolUpdateOut = eTX0 { bcTXPool = ["TX-should-stay"] }

e1BadPHash :: BCState
e1BadPHash = eTX0 { bcChain = makeChain "X" 658 }

e1BadProof :: BCState
e1BadProof = eTX0 { bcChain = makeChain (hashBlock genesisBlock) 0 }

makeChain :: BHash -> Proof -> Chain
makeChain ph p =
  [genesisBlock
  ,Block { bPrevHash = ph
         , bIndex = 1, bTXs = ["TX-0"]
         , bProof = p}]

e1NotLost :: BCState
(e1NotLost,_) = mine (addTransaction (addTransaction initialBCState "TX1") "TX2")

e2NotLost :: BCState
e2NotLost =
  let (etx1,_) = mine (addTransaction initialBCState "TX1")
      (etx2,_) = mine (addTransaction etx1       "TX3")
   in  etx2

e1NotLastAfterResolve :: BCState
e1NotLastAfterResolve = e2NotLost { bcTXPool = ["TX2"] }

testResolveConflicts =
  describe "ResolveConflicts" $ do
    it "found longer chain" $
      resolveConflicts initialBCState  [bcChain eTX0]
      `shouldBe` (eTX0 , (True , ""))
    it "no    longer chain" $
      resolveConflicts eTX0          [bcChain initialBCState]
      `shouldBe` (eTX0 , (False, ""))
    it "found longer chain and pool update" $
      resolveConflicts eLongerChainAndPoolUpdateIn  [bcChain eTX0]
      `shouldBe` (e1LongerChainAndPoolUpdateOut, (True , ""))
    it "invalid previous hash" $
      resolveConflicts initialBCState  [bcChain e1BadPHash]
      `shouldBe` (initialBCState
                 , (False, "resolveConflicts: invalid chain invalid bPrevHash at 1"))
    it "invalid proof of work" $
      resolveConflicts initialBCState  [bcChain e1BadProof]
      `shouldBe` (initialBCState
                 , (False, "resolveConflicts: invalid chain invalid bProof at 1"))
    it "should not drop TX" $
      resolveConflicts e1NotLost   [bcChain e2NotLost]
      `shouldBe` (e1NotLastAfterResolve
                 , (True, ""))

------------------------------------------------------------------------------
testIsValidChain =
  describe "isValidChain" $ do
    it "invalid empty" $
      isValidChain 4 [] `shouldBe` Left "empty blockchain"
    it "valid genesis" $
      isValidChain 4 [genesisBlock] `shouldBe` Right ()
    it "invalid genesis" $
      let bg = genesisBlock { bIndex = 6 }
      in isValidChain 4 [bg] `shouldBe` Left "invalid genesis block"
    it "valid eTX0" $
      isValidChain 4 (bcChain eTX0) `shouldBe` Right ()
    it "invalid previous hash" $
      isValidChain 4 (bcChain e1BadPHash) `shouldBe` Left "invalid bPrevHash at 1"
    it "invalid proof" $
      isValidChain 4 (bcChain e1BadProof) `shouldBe` Left "invalid bProof at 1"

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
