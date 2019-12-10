{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Z where

------------------------------------------------------------------------------
import           MakeClassy
------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Trans.RWS.Strict
import qualified Data.Map.Strict                as Map
import           Protolude                      hiding (get, gets, round, to)
------------------------------------------------------------------------------

newtype Author    = Author   { _aAuthor     :: Text       } deriving (Eq, Ord,      Show)
newtype Epoch     = Epoch    { _eEpoch      :: Int        } deriving (Eq, Num, Ord, Show)
newtype HashValue = HashValue{ _hvHashValue :: ByteString } deriving (Eq, Ord,      Show)
newtype Round     = Round    { _rRound      :: Int        } deriving (Eq, Num, Ord, Show)

data BlockInfo = BlockInfo
  { _biAuthor :: !Author
  , _biEpoch  :: !Epoch
  , _biRound  :: !Round
  , _biId     :: !HashValue
  } deriving (Eq, Show)

data VoteData = VoteData
  { _vdProposed :: !BlockInfo
  , _vdParent   :: !BlockInfo
  } deriving (Eq, Show)

data BlockTree a = BlockTree
  { _btVoteDataMap :: !(Map HashValue VoteData)
  , _btRootId      :: !HashValue
  } deriving (Eq, Show)

data BlockStore a = BlockStore
  { _bsInner :: BlockTree a
  , _bsStuff :: Text
  } deriving (Eq, Show)

data Pacemaker = Pacemaker
  { _pHighestCommittedRound :: !Round
  , _pCurrentRound          :: !Round
  } deriving (Eq, Show)

data Vote = Vote
  { _vVoteData :: !VoteData
  , _vAuthor   :: !Author
  } deriving (Eq, Show)

data EventProcessor a = EventProcessor
  { _epAuthor       :: !Author
  , _epBlockStore   :: !(BlockStore a)
  , _epPacemaker    :: !Pacemaker
  , _epLastVoteSend :: !(Maybe (Vote, Round))
  } deriving (Eq, Show)

obmMakeClassy ''Author
obmMakeClassy ''Epoch
obmMakeClassy ''HashValue
obmMakeClassy ''Round
obmMakeClassy ''EventProcessor
obmMakeClassy ''BlockStore
obmMakeClassy ''Pacemaker
obmMakeClassy ''BlockTree

instance RWBlockStore (EventProcessor a) a where
  lBlockStore = lens _epBlockStore (\x y -> x { _epBlockStore = y})

instance RWBlockTree (EventProcessor a) a where
  lBlockTree = lens (^.epBlockStore.bsInner) (\x y -> x & epBlockStore.bsInner .~ y)

instance RWPacemaker (EventProcessor a) where
  lPacemaker = lens _epPacemaker (\x y -> x { _epPacemaker = y})

instance RWAuthor (EventProcessor a) where
  lAuthor = lens _epAuthor (\x y -> x { _epAuthor = y})

------------------------------------------------------------------------------
-- EventProcessor

ep :: (RWAuthor s, RWPacemaker s, RWBlockStore s a, RWBlockTree s a)
   => RWS () [Text] s ()
ep  = do
  pm
  author <- use lAuthor
  bs author

------------------------------------------------------------------------------
-- Pacemaker

pm :: RWPacemaker s
   => RWS () [Text] s ()
pm  = do
  r <- use (lPacemaker.pHighestCommittedRound)
  tell ["PM " <> show r]

------------------------------------------------------------------------------
-- BlockStore

bs :: (RWBlockStore s a, RWBlockTree s a)
   => Author -> RWS () [Text] s ()
bs author = do
  s <- use (lBlockStore.bsStuff)
  tell ["BS " <> show s]
  bt author

------------------------------------------------------------------------------
-- BlockTree

bt :: (RWBlockTree s a)
    => Author -> RWS () [Text] s ()
bt author = do
  vdm <- use (lBlockTree.btVoteDataMap)
  let vd = Map.lookup (HashValue "junk") vdm
  tell ["BT " <> show author <> " " <> show vd]

------------------------------------------------------------------------------
-- run

run :: EventProcessor a -> [Text]
run ep0 = let (_,_,t) = runRWS ep () ep0 in t

runEpT :: [Text]
runEpT  = run epT

------------------------------------------------------------------------------

biT :: BlockInfo
biT  = BlockInfo (Author "biauthor") (Epoch 0) (Round 0) (HashValue "0")

epT :: EventProcessor ByteString
epT  = EventProcessor
        (Author "epauthor")
        (BlockStore (BlockTree Map.empty (HashValue "btrootid")) "bsstuff")
        (Pacemaker (Round 100) (Round 101))
        (Just ( Vote
                  (VoteData biT biT)
                  (Author "epauthor")
              , Round 45))

