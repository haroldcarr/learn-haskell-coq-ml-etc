{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XXSpec where

import qualified Crypto.Hash     as Hash
import           Data.Foldable   (foldl', foldr')
import qualified Data.Map.Strict as Map
import           Data.Word       (Word64)
import           Test.Hspec

newtype LogIndex = LogIndex Int
  deriving (Show, Read, Eq, Ord, Enum, Num, Real, Integral)

newtype Term = Term Int
  deriving (Show, Read, Eq, Enum, Num, Ord)

type Port = Word64

data NodeId = NodeId { _host :: !String, _port :: !Port, _fullAddr :: !String }
  deriving (Eq,Ord,Read,Show)

newtype CumulativeHash = CumulativeHash { unCumulativeHash :: Hash.Digest Hash.SHA256 }
  deriving (Show, Eq, Ord)

type AppendEntriesResponse = Int

type CommitProofMap = Map.Map (LogIndex, Term)
                              (Map.Map NodeId
                                       (Map.Map CumulativeHash
                                                AppendEntriesResponse))

spec :: Spec
spec =
  describe "XX" $
    it "gets correct shardID" $
      True `shouldBe` False

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
 where
  cpms                  :: CommitProofMap -> [(Term, LogIndex, [[CumulativeHash]])]
  cpms     cpm           =         Map.foldlWithKey' goOuter  []      cpm
  goOuter  acc (li, t) v = (t, li, Map.foldl'        goMiddle [] v) : acc
  goMiddle acc         v =         Map.foldlWithKey' goInner  [] v  : acc
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
  inOtherLists acc h hss = foldl' (go h) acc hss

  go                    :: CumulativeHash -> [(Int, CumulativeHash)] -> [CumulativeHash]
                        -> [(Int, CumulativeHash)]
  go h acc l             = if h `elem` l then updateAcc h acc else acc

  updateAcc             :: CumulativeHash -> [(Int, CumulativeHash)]
                        -> [(Int, CumulativeHash)]
  updateAcc h acc        =
    let acc'' = foldl' (\acc' (i, h') ->
                           if h == h' then (i+1, h) : acc'
                                      else (i  , h) : acc')
                       []
                       acc
     in if h `isIn` acc'' then acc'' else (1, h) : acc''

