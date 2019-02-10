{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE OverloadedStrings          #-}

module XXSpec where

import qualified Crypto.Hash     as Hash
import qualified Data.ByteString as BS
import qualified Data.List       as DL
import qualified Data.Map.Strict as Map
import           Data.Ord        (Down (..))
import           Data.Word       (Word64)
import           Test.Hspec

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral)

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord)

type Port = Word64

data NodeId = NodeId { _host :: !String, _port :: !Port, _fullAddr :: !String }
  deriving (Eq,Ord,Read)

instance Show NodeId where
  show nid = show (_fullAddr nid)

mkNid :: String -> NodeId
mkNid s = NodeId "host" 8080 s

newtype CumulativeHash = CumulativeHash { unCumulativeHash :: Hash.Digest Hash.SHA256 }
  deriving (Eq, Ord)

instance Show CumulativeHash where
  show (CumulativeHash h) = take 7 (show h)

type AppendEntriesResponse = Int

type CommitProofMap = Map.Map (LogIndex, Term)
                              (Map.Map NodeId
                                       (Map.Map CumulativeHash
                                                AppendEntriesResponse))

commitProofMapSummary :: CommitProofMap -> [(Term, LogIndex, [(Int, CumulativeHash)])]
commitProofMapSummary = countCommon . cpms

cpms                  :: CommitProofMap -> [(Term, LogIndex, [[CumulativeHash]])]
cpms     cpm           =         Map.foldlWithKey' goOuter  []      cpm

goOuter               :: [(a1, b, [[a2]])] -> (b, a1) -> Map.Map k (Map.Map a2 p)
                      -> [(a1, b, [[a2]])]
goOuter  acc (li, t) v = (t, li, Map.foldl'        goMiddle [] v) : acc

goMiddle              :: [[a]] -> Map.Map a p -> [[a]]
goMiddle acc         v =         Map.foldlWithKey' goInner  [] v  : acc

goInner               :: [a] -> a -> p -> [a]
goInner  acc hash    _ = hash                                     : acc

countCommon           :: [(Term, LogIndex, [[CumulativeHash]])]
                      -> [(Term, LogIndex, [(Int, CumulativeHash)])]
countCommon            = map (\(t, li, xs) -> (t, li, cc xs))

cc                    :: [[CumulativeHash]] -> [(Int, CumulativeHash)]
cc                     = DL.sortOn (Down . fst)
                       . map (\g -> (length g, head g))
                       . DL.group . DL.sort . DL.concat

------------------------------------------------------------------------------

spec :: Spec
spec = do
  ccSpec
  commitProofMapSummarySpec

ccSpec :: Spec
ccSpec = describe "cc" $ do

  it "distinct" $
    cc         [   [cHash "1"],    [cHash "2"],    [cHash "3"]]
    `shouldBe` [(1, cHash "3"), (1, cHash "1"), (1, cHash "2")]

  it "2 2s" $
    cc         [   [cHash "1"],    [cHash "2"],    [cHash "3", cHash "2"]]
    `shouldBe` [(2, cHash "2"), (1, cHash "3"), (1, cHash "1")]

  it "a lot" $
    cc         [ [cHash "1", cHash "3"],[cHash "2"],[cHash "3", cHash "2"],[cHash "3", cHash "99"]]
    `shouldBe` [(3,cHash "3"),(2,cHash "2"),(1,cHash "1"),(1,cHash "99")]

commitProofMapSummarySpec :: Spec
commitProofMapSummarySpec = describe "commitProofMapSummary" $ do

  it "(LogIndex 0, Term 0)" $
    commitProofMapSummary
      [( (LogIndex 0, Term 0)
       , [ ( mkNid "1", [ (cHash "1", 1), (cHash "2", 2) ] )
         , ( mkNid "2", [ (cHash "2", 2), (cHash "3", 3), (cHash "99", 99) ] )
         , ( mkNid "3", [ (cHash "3", 3) ] )
         , ( mkNid "4", [ (cHash "3", 3) ] )
         ]
       )
      ]
    `shouldBe`
    [ (Term 0,LogIndex 0,[(3,cHash "3"),(2,cHash "2"),(1,cHash "1"),(1,cHash "99")])
    ]

  it "(LogIndex 0, Term 0), (LogIndex 0, Term 1)" $
    commitProofMapSummary
      [( (LogIndex 0, Term 0)
       , [ ( mkNid "1", [ (cHash "1", 1), (cHash "2", 2) ] )
         , ( mkNid "2", [ (cHash "2", 2), (cHash "3", 3), (cHash "99", 99) ] )
         , ( mkNid "3", [ (cHash "3", 3) ] )
         , ( mkNid "4", [ (cHash "3", 3) ] )
         ]
       )
      ,( (LogIndex 0, Term 1)
       , [ ( mkNid "1", [ (cHash "1", 1), (cHash "2", 2) ] )
         , ( mkNid "2", [ (cHash "2", 2), (cHash "3", 3), (cHash "99", 99) ] )
         , ( mkNid "3", [ (cHash "3", 3) ] )
         , ( mkNid "4", [ (cHash "3", 3) ] )
         ]
       )
      ]
    `shouldBe`
    [ (Term 1,LogIndex 0,[(3,cHash "3"),(2,cHash "2"),(1,cHash "1"),(1,cHash "99")])
    , (Term 0,LogIndex 0,[(3,cHash "3"),(2,cHash "2"),(1,cHash "1"),(1,cHash "99")])
    ]

------------------------------------------------------------------------------

cHash :: BS.ByteString -> CumulativeHash
cHash = CumulativeHash . Hash.hash
