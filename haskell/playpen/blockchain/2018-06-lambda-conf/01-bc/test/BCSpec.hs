{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BCSpec where

import           Prelude    ((!!))
import           Protolude
import           Test.Hspec
------------------------------------------------------------------------------
import           BC

spec :: Spec
spec = do
  testEvidence
  testProofOfWork
  testIsValidChain
  testResolveConflicts

testEvidence :: Spec
testEvidence =
  describe "evidence" $ do
    it "from genesisBlock" $
      evidence (bProof genesisBlock) (hashBlock genesisBlock) (proofOfWork 4 genesisBlock)
      `shouldBe`
      "0000DDF9FB09F9A9C5A0EF57DC3E2916633BEDB95B38D54BDBFFF0B7D4D6E515"
    it "from genesisBlock + 1" $
      evidence (bProof (eChain e1 !! 1)) (hashBlock (eChain e1 !! 1))  3805
      `shouldBe`
      "0000C5DD35BB87E83CFF98E2CE3D27C3CF7704362045D7D111B2F2D6D6C9DC03"

testProofOfWork :: Spec
testProofOfWork =
  describe "proofOfWork" $ do
    it "from genesisBlock" $
      proofOfWork 4 genesisBlock `shouldBe`          bProof (eChain e1 !! 1)
    it "from genesisBlock + 1" $
      proofOfWork 4 (eChain e1 !! 1) `shouldBe`      3805

testIsValidChain :: Spec
testIsValidChain =
  describe "isValidChain" $ do
    it "invalid empty" $
      isValidChain 4 [] `shouldBe` Left "empty blockchain"
    it "valid genesis" $
      isValidChain 4 [genesisBlock] `shouldBe` Right ()
    it "invalid genesis" $
      let bg = genesisBlock { bIndex = 6 }
      in isValidChain 4 [bg] `shouldBe` Left "invalid genesis block"
    it "valid e1" $
      isValidChain 4 (eChain e1) `shouldBe` Right ()
    it "invalid previous hash" $
      isValidChain 4 (eChain e1BadPHash) `shouldBe` Left "invalid bPrevHash at 1"
    it "invalid proof" $
      isValidChain 4 (eChain e1BadProof) `shouldBe` Left "invalid bProof at 1"

testResolveConflicts :: Spec
testResolveConflicts =
  describe "ResolveConflicts" $ do
    it "found longer chain" $
      resolveConflicts initialEnv  [eChain e1]
      `shouldBe` (e1 , (True , ""))
    it "no    longer chain" $
      resolveConflicts e1          [eChain initialEnv]
      `shouldBe` (e1 , (False, ""))
    it "found longer chain and pool update" $
      resolveConflicts e0'         [eChain e1]
      `shouldBe` (e1', (True , ""))
    it "invalid previous hash" $
      resolveConflicts initialEnv  [eChain e1BadPHash]
      `shouldBe` (initialEnv
                 , (False, "resolveConflicts: invalid chain invalid bPrevHash at 1"))
    it "invalid proof of work" $
      resolveConflicts initialEnv  [eChain e1BadProof]
      `shouldBe` (initialEnv
                 , (False, "resolveConflicts: invalid chain invalid bProof at 1"))
    it "should not drop TX" $
      resolveConflicts e1NotLost   [eChain e2NotLost]
      `shouldBe` (e1NotLastAfterResolve
                 , (True, ""))

------------------------------------------------------------------------------
-- test data

e0' :: Env
e0' = initialEnv { eTXPool = ["TX-0","TX-should-stay"] }

makeChain :: BHash -> Proof -> [Block]
makeChain ph p =
  [genesisBlock
  ,Block { bPrevHash = ph
         , bIndex = 1
         , bTXs = ["TX-0"]
         , bProof = p
         }
  ]

e1 :: Env
(e1,_) = mine (addTransaction initialEnv "TX-0")

e1' :: Env
e1' = e1 { eTXPool = ["TX-should-stay"] }

e1BadPHash :: Env
e1BadPHash = e1 { eChain = makeChain "X" 658 }

e1BadProof :: Env
e1BadProof = e1 { eChain = makeChain (hashBlock genesisBlock) 0 }

------------------------------------------------------------------------------
-- data to ensure TXs not lost

e1NotLost :: Env
e1NotLost = Env
  { eTXPool = []
  , eChain =
    [ genesisBlock
    , Block { bPrevHash = hashBlock genesisBlock
            , bIndex = 1
            , bTXs = ["TX1","TX2"]
            , bProof = bProof (eChain e1 !! 1)
            }
    ]
  , eProofDifficulty = 4, eNodes = [], eThisNode = ""
  }

e2NotLost :: Env
e2NotLost = Env
  { eTXPool = []
  , eChain =
    [ genesisBlock
    , Block { bPrevHash = hashBlock genesisBlock
            , bIndex = 1
            , bTXs = ["TX1"]
            , bProof = bProof (eChain e1 !! 1)
            }
    , Block { bPrevHash = "\233\237\222\222E\138\254\241k\232\151\DLE\177\175g\191\134e\248q\158\153V^\182\244r\156\187.\244\193"
            , bIndex = 2
            , bTXs = ["TX3"]
            , bProof = 52668
            }
    ]
  , eProofDifficulty = 4, eNodes = [], eThisNode = ""
  }

e1NotLastAfterResolve :: Env
e1NotLastAfterResolve = e2NotLost { eTXPool = ["TX2"] }

{-
:set -XOverloadedStrings
import           Data.List                            ((\\))
txsInChain = foldl (\txs b -> txs ++ bTXs b) []
myPool   = eTXPool e1NotLost
myTXs    = txsInChain (eChain e1NotLost)
theirTXs = txsInChain (eChain e2NotLost)
myPool \\ theirTXs -- remove TXs from my pool that are in their chain
myTXs  \\ theirTXs -- add TXs from my chain that are not in their chain
-}
