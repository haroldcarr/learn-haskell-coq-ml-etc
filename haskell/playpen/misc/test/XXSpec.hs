{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module XXSpec where

import qualified Crypto.Hash     as Hash
import qualified Data.ByteString as BS
import           Data.Foldable   (foldl', foldr')
import qualified Data.Map.Strict as Map
import           Data.Word       (Word64)
import           Test.Hspec

{-# ANN module ("HLint: ignore Eta reduce"::String) #-}

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral)

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord)

type Port = Word64

data NodeId = NodeId { _host :: !String, _port :: !Port, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Show)

newtype CumulativeHash = CumulativeHash { unCumulativeHash :: Hash.Digest Hash.SHA256 }
  deriving (Eq, Ord)

instance Show CumulativeHash where
  show (CumulativeHash h) = take 7 (show h)

type AppendEntriesResponse = Int

type CommitProofMap = Map.Map (LogIndex, Term)
                              (Map.Map NodeId
                                       (Map.Map CumulativeHash
                                                AppendEntriesResponse))

getCommitProofMapAERs :: CommitProofMap -> [AppendEntriesResponse]
getCommitProofMapAERs
  = concatMap Map.elems -- inner map
  . concatMap Map.elems -- middle map
  . Map.elems           -- outer map

commitProofMapSummaryString :: CommitProofMap  -> String
commitProofMapSummaryString cpm
  = show
    ( cpm
    , map (\(Term t, LogIndex li, hs) -> (t, li, map go hs))
      $ commitProofMapSummary cpm
    )
 where
  go (i, CumulativeHash h) = (i, take 7 (show h))

commitProofMapSummary :: CommitProofMap -> [(Term, LogIndex, [(Int, CumulativeHash)])]
commitProofMapSummary = countCommon . cpms

cpms                  :: CommitProofMap -> [(Term, LogIndex, [[CumulativeHash]])]
cpms     cpm           =         Map.foldlWithKey' goOuter  []      cpm

goOuter :: [(a1, b, [[a2]])] -> (b, a1) -> Map.Map k (Map.Map a2 p) -> [(a1, b, [[a2]])]
goOuter  acc (li, t) v = (t, li, Map.foldl'        goMiddle [] v) : acc

goMiddle :: [[a]] -> Map.Map a p -> [[a]]
goMiddle acc         v =         Map.foldlWithKey' goInner  [] v  : acc

goInner :: [a] -> a -> p -> [a]
goInner  acc hash    _ = hash                                     : acc

countCommon           :: [(Term, LogIndex, [[CumulativeHash]])]
                      -> [(Term, LogIndex, [(Int, CumulativeHash)])]
countCommon            = map (\(t, li, xs) -> (t, li, cc [] xs))

cc                    :: [(Int, CumulativeHash)]                     -> [[CumulativeHash]]
                      -> [(Int, CumulativeHash)]
cc acc []       = acc
cc acc (hs:hss) = cc (ccx acc hs hss) hss

ccx                   :: [(Int, CumulativeHash)] -> [CumulativeHash] -> [[CumulativeHash]]
                      -> [(Int, CumulativeHash)]
ccx acc []    _        = acc
ccx acc (h:hs) hss     = if h `isIn` acc
                         then ccx acc                      hs hss
                         else ccx (inOtherLists acc h hss) hs hss

isIn                  :: CumulativeHash -> [(Int, CumulativeHash)] -> Bool
isIn h acc             = foldr' (\(_, h') acc' -> h == h' || acc') False acc

inOtherLists          :: [(Int, CumulativeHash)] -> CumulativeHash -> [[CumulativeHash]]
                        -> [(Int, CumulativeHash)]
inOtherLists acc h hss = foldl' (gol h) acc hss

gol                   :: CumulativeHash -> [(Int, CumulativeHash)] -> [CumulativeHash]
                      -> [(Int, CumulativeHash)]
gol h acc l            = if h `elem` l then updateAcc h acc else acc

updateAcc             :: CumulativeHash -> [(Int, CumulativeHash)]
                      -> [(Int, CumulativeHash)]
updateAcc h acc        =
  let acc'' = foldr' (\(i, h') acc' ->
                         if h == h' then (i+1, h') : acc'
                                    else (i  , h') : acc')
                     []
                     acc
   in if h `isIn` acc'' then acc'' else (1, h) : acc''

------------------------------------------------------------------------------

spec :: Spec
spec = do
  isInSpec
  updateAccSpec
  ccSpec

isInSpec :: Spec
isInSpec = describe "isIn" $ do

  it "False" $
    isIn (cHash "0") [(1, cHash "1"), (1, cHash "2"), (1, cHash "3")]
    `shouldBe` False

  it "front" $
    isIn (cHash "1") [(1, cHash "1"), (1, cHash "2"), (1, cHash "3")]
    `shouldBe` True

  it "middle" $
    isIn (cHash "2") [(1, cHash "1"), (1, cHash "2"), (1, cHash "3")]
    `shouldBe` True

  it "end" $
    isIn (cHash "3") [(1, cHash "1"), (1, cHash "2"), (1, cHash "3")]
    `shouldBe` True

updateAccSpec :: Spec
updateAccSpec = describe "updateAcc" $ do

  it "new empty" $
    updateAcc (cHash "1") []
    `shouldBe`            [(1, cHash "1")]

  it "new existing results" $
    updateAcc (cHash "1") [                (2, cHash "2")]
    `shouldBe`            [(1, cHash "1"), (2, cHash "2")]

  it "existing" $
    updateAcc (cHash "1") [(1, cHash "1")]
    `shouldBe`            [(2, cHash "1")]

  it "existing front" $
    updateAcc (cHash "2") [(1, cHash "2"), (3, cHash "3"), (4, cHash "4")]
    `shouldBe`            [(2, cHash "2"), (3, cHash "3"), (4, cHash "4")]

  it "existing middle" $
    updateAcc (cHash "2") [(1, cHash "1"), (1, cHash "2"), (3, cHash "3")]
    `shouldBe`            [(1, cHash "1"), (2, cHash "2"), (3, cHash "3")]

  it "existing end" $
    updateAcc (cHash "3") [(1, cHash "1"), (2, cHash "2"), (2, cHash "3")]
    `shouldBe`            [(1, cHash "1"), (2, cHash "2"), (3, cHash "3")]

ccSpec :: Spec
ccSpec = describe "cc" $
  it "x" $
    cc []      [   [cHash "1"],    [cHash "2"],    [cHash "3"]]
    `shouldBe` [(1, cHash "1"), (1, cHash "2"), (1, cHash "3")]

------------------------------------------------------------------------------

cHash :: BS.ByteString -> CumulativeHash
cHash = CumulativeHash . Hash.hash
