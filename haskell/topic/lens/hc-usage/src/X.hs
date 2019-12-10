{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module X where

------------------------------------------------------------------------------
import           Init
import           Types
------------------------------------------------------------------------------
import           Control.Lens
-- import qualified Control.Lens.Internal.FieldTH  as FTH
-- import           Control.Lens.TH
import           Control.Monad.Trans.RWS.Strict
-- import qualified Language.Haskell.TH.Datatype   as THD
-- import qualified Language.Haskell.TH.Lib        as THL
-- import qualified Language.Haskell.TH.Syntax     as THS
import qualified Prelude
import           Protolude                      hiding (get, gets, round, to)
------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}
{-# ANN module ("HLint: ignore Redundant return" :: Prelude.String) #-}

makeClassyFor "RWAuthor" "lAuthor" [("_getAuthor", "getAuthor")] ''Author
makeClassyFor "RWEpoch" "lEpoch" [("_epochEpoch", "getEpoch")] ''Epoch
makeClassyFor "RWHashValue" "lHashValue" [("_getHashValue", "getHashValue")] ''HashValue
makeClassyFor "RWRound" "lRound" [("_getRound", "getRound")] ''Round
makeFields ''Author
makeFields ''Epoch
makeFields ''HashValue
makeFields ''Round

makeClassyFor "RWBlockInfo" "lBlockInfo"
  [ ("_blockInfoAuthor" , "biAuthor")
  , ("_blockInfoEpoch"  , "biEpoch")
  , ("_blockInfoRound"  , "biRound")
  , ("_blockInfoId"     , "biId") ]
  ''BlockInfo
makeFields ''BlockInfo

makeClassyFor "RWVoteData" "lVoteData"
  [ ("_voteDataProposed", "vdProposed")
  , ("_voteDataParent"  , "vdParent") ]
  ''VoteData
makeFields ''VoteData

makeClassyFor "RWBlockTree" "lBlockTree"
  [ ("_blockTreeVoteData", "btVoteData")
  , ("_blockTreeRootId",   "btRootId") ]
  ''BlockTree
makeFields ''BlockTree

makeClassyFor "RWBlockStore" "lBlockStore"
  [ ("_blockStoreInner", "bsInner") ]
  ''BlockStore
makeFields ''BlockStore

makeClassyFor "RWPacemaker" "lPacemaker"
  [ ("_pacemakerHighestCommittedRound", "psHighestCommittedRound")
  , ("_pacemakerCurrentRound", "psCurrentRound") ]
  ''Pacemaker

makeClassyFor "RWVote" "lVote"
  [ ("_voteVoteData", "vVoteData")
  , ("_voteAuthor", "vAuthor") ]
  ''Vote
makeFields ''Vote

makeClassyFor "RWEventProcessor" "lEventProcessor"
  [ ("_eventProcessorBlockStore", "epsBlockStore")
  , ("_eventProcessorPacemaker", "epsPacemaker")
  , ("_eventProcessorLastVoteSend",  "epsLastVoteSend") ]
  ''EventProcessor
makeFields ''EventProcessor

foo
  :: ( Monad m
     , HasBlockStore s (BlockStore a)
     , HasPacemaker s Pacemaker
     , HasLastVoteSend s (Maybe (Vote, Round))
     , Show s )
  => RWST () [Text] s m ()
foo  = do
  ep <- get
  bs <- use blockStore
  pm <- use pacemaker
  lv <- use lastVoteSend
  tell ["EP " <> show ep]
  tell ["BS " <> show bs]
  tell ["PM " <> show pm]
  tell ["LV " <> show lv]
  pure ()

bar :: ( Monad m
       , HasInner s (BlockTree a) )
    => RWST () [Text] s m ()
bar  = do
  rid <- use (inner.rootId)
  tell ["INNER" <> show rid]
  pure ()

rfoo :: Monad m => m ((), EventProcessor ByteString, [Text])
rfoo  = runRWST foo () epT
{-
foo
  :: (RWBlockInfo x, RWVoteData x)
  => Author -> Epoch -> HashValue -> Round -> BlockInfo -> VoteData
  -> Text
foo author epoch hashValue round blockInfo voteData =
  show (author, epoch, hashValue, round, blockInfo, voteData)
  <>
  show ( author^.getAuthor, epoch^.getEpoch, hashValue^.getHashValue, round^.getRound
       , blockInfo^.biAuthor
       , voteData^.vdProposed)
  <>
  bar voteData


bar :: (RWBlockInfo x, RWVoteData x) => x -> Text
bar  x = show (x^.vdProposed) <> show (baz x)

baz :: RWBlockInfo x => x -> Text
baz  x = show (x^.biAuthor.getAuthor)

qux :: HasEpoch s Int => s -> Int
qux x = x^.epoch

r :: Text
r  = foo (Author "author") (Epoch 1) (HashValue "HashValue") (Round 2)
         (BlockInfo (Author "biauthor0") (Epoch 10) (Round 20) (HashValue "bid0"))
         (VoteData (BlockInfo (Author "biauthor1") (Epoch 11) (Round 21) (HashValue "bid1"))
                   (BlockInfo (Author "biauthor2") (Epoch 12) (Round 22) (HashValue "bid2")))
-}
{-
obmMakeClassyFor
  :: Prelude.String -> Prelude.String -> [(Prelude.String, Prelude.String)] -> THS.Name
  -> THL.DecsQ
obmMakeClassyFor clsName funName fields = FTH.makeFieldOptics $
  classyRulesFor (const (Just (clsName, funName))) fields

classyRulesFor
  :: (Prelude.String -> Maybe (Prelude.String, Prelude.String)) {- ^ Type Name -> Maybe (Class Name, Method Name) -} ->
  [(Prelude.String, Prelude.String)] {- ^ [(Field Name, Method Name)] -} ->
  LensRules
classyRulesFor classFun fields = classyRules
  & lensClass .~ (over (mapped . both) THS.mkName . classFun . THS.nameBase)
  & lensField .~ lookingupNamer fields

-- makeFieldOptics :: LensRules -> THS.Name -> THL.DecsQ
-- makeFieldOptics rules = (`evalStateT` Set.empty) . makeFieldOpticsForDatatype rules <=< THD.reifyDatatype
-}
{-
:set -XTemplateHaskell
THS.Q x = THD.reifyDatatype ''VoteData
THD.datatypeName x
-}
