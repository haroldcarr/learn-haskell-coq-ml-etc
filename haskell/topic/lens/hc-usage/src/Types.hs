{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Types where

------------------------------------------------------------------------------
import           Protolude
------------------------------------------------------------------------------

newtype Author    = Author   { _authorAuthor       :: Text       } deriving (Eq, Ord,      Show)
newtype Epoch     = Epoch    { _epochEpoch         :: Int        } deriving (Eq, Num, Ord, Show)
newtype HashValue = HashValue{ _hashValueHashValue :: ByteString } deriving (Eq, Ord,      Show)
newtype Round     = Round    { _roundRound         :: Int        } deriving (Eq, Num, Ord, Show)

data BlockInfo = BlockInfo
  { _blockInfoAuthor :: !Author
  , _blockInfoEpoch  :: !Epoch
  , _blockInfoRound  :: !Round
  , _blockInfoId     :: !HashValue
  } deriving (Eq, Show)

data VoteData = VoteData
  { _voteDataProposed :: !BlockInfo
  , _voteDataParent   :: !BlockInfo
  } deriving (Eq, Show)

data BlockTree a = BlockTree
  { _blockTreeVoteData      :: !(Map HashValue VoteData)
  , _blockTreeRootId        :: !HashValue
  } deriving (Eq, Show)

newtype BlockStore a = BlockStore
  { _blockStoreInner         :: BlockTree a
  } deriving (Eq, Show)

data Pacemaker = Pacemaker
  { _pacemakerHighestCommittedRound :: !Round
  , _pacemakerCurrentRound          :: !Round
  } deriving (Eq, Show)

data Vote = Vote
  { _voteVoteData         :: !VoteData
  , _voteAuthor           :: !Author
  } deriving (Eq, Show)

data EventProcessor a = EventProcessor
  { _eventProcessorBlockStore        :: !(BlockStore a)
  , _eventProcessorPacemaker         :: !Pacemaker
  , _eventProcessorLastVoteSend      :: !(Maybe (Vote, Round))
  } deriving (Eq, Show)
