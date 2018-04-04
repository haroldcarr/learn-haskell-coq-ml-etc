{-# LANGUAGE OverloadedStrings #-}

module BCSpec where

import           Test.Hspec
------------------------------------------------------------------------------
import           BC

spec :: Spec
spec =
  testResolveConflicts

e0 :: Env
e0 = Env { eCurrentTransactions = []
         , eChain = [Block { bPreviousHash = "1"
                           , bIndex = 0
                           , bTimestamp = "2018-04-01"
                           , bTransactions = []
                           , bProof = 100
                           }
                    ]
         , eNodes = []
         , eThisNode = ""
         }

e0' :: Env
e0' = e0 { eCurrentTransactions = ["TX-0","TX-should-stay"] }

makeChain :: BHash -> Proof -> [Block]
makeChain ph p =
  [Block { bPreviousHash = "1"
         , bIndex = 0
         , bTimestamp = "2018-04-01"
         , bTransactions = []
         , bProof = 100
         }
  ,Block { bPreviousHash = ph
         , bIndex = 1
         , bTimestamp = "timestamp"
         , bTransactions = ["TX-0","sender=0;recipient=3000;amount=1"]
         , bProof = p
         }
  ]

e1 :: Env
e1 = Env { eCurrentTransactions = []
         , eChain = makeChain "B\175\211(+q\SOHW3\ETX2?\NAK\244\241P\244\198\209\241\157\200!\212\a\226\219\227\164\175\186\202" 658
         , eNodes = []
         , eThisNode = ""
         }

e1' :: Env
e1' = e1 { eCurrentTransactions = ["TX-should-stay"] }

e1'' :: Env
e1'' = e1 { eChain = makeChain "X" 658 }

e1''' :: Env
e1''' = e1 { eChain = makeChain "B\175\211(+q\SOHW3\ETX2?\NAK\244\241P\244\198\209\241\157\200!\212\a\226\219\227\164\175\186\202" 0 }

testResolveConflicts :: Spec
testResolveConflicts =
  describe "ResolveConflicts" $
    let r0 = resolveConflicts e0  [eChain e1]
        r1 = resolveConflicts e1  [eChain e0]
        r3 = resolveConflicts e0' [eChain e1]
        r4 = resolveConflicts e0  [eChain e1'']
        r5 = resolveConflicts e0  [eChain e1''']
    in do
      it "found longer chain" $
        r0 `shouldBe` (e1 , (True , ""))
      it "no    longer chain" $
        r1 `shouldBe` (e1 , (False, ""))
      it "found longer chain and pool update" $
        r3 `shouldBe` (e1', (True , ""))
      it "invalid previous hash" $
        r4 `shouldBe` (e0 , (False, "resolveConflicts: invalid chain invalid bPrevHash at 1"))
      it "invalid proof of work" $
        r5 `shouldBe` (e0 , (False, "resolveConflicts: invalid chain invalid bProof at 1"))
