{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Seq where

import           Control.Arrow        ((&&&))
import           Data.Foldable
import           Data.Sequence as Seq
import qualified Data.Map      as Map (Map, delete, update, fromList)

x :: Seq Integer
x = Seq.fromList [1,2,3]

y :: (Seq Integer, Seq Integer)
y = Seq.splitAt  1 x

z :: Seq Integer
z = Seq.take  1 x

-- | Add entries at specified index.
-- Returning the modified log and a sequence containing any removed entries,
-- where "removed" means an entries that existed in slots greater than the specified index.

type NodeID = Int
type Signature = Int
type CommandResult = Int
type ReplayMap = Map.Map (NodeID, Signature) (Maybe CommandResult)

newtype Command = Command
  { _cmdClientId :: NodeID
  } deriving (Show, Eq)

newtype LogEntry = LogEntry
  { _leCommand :: Command
  } deriving (Show, Eq)

removeUnfinishedReplaysM :: Seq LogEntry
                         -> ReplayMap
                         -> ReplayMap
removeUnfinishedReplaysM removedEntries replays =
    let replayKeys =
          fmap ((_cmdClientId &&& getCmdSigOrInvariantError "removeUnfinishedReplays") . _leCommand)
               removedEntries
        removeIfNothing Nothing = Nothing
        removeIfNothing (Just c) = Just (Just c)
    in
      foldl' (flip $ Map.update removeIfNothing) replays replayKeys

-- Remove entries from replay map for uncompleted commands,
-- where "uncompleted" means command that will never be committed
-- (unless the client resends the command after it is removed from the ReplayMap).
-- This can happen if a leader gets deposed and a new leader causes uncommitted
-- log entries to be removed.  In that case, the replay entries corresponding
-- to the removed entries must also be removed.
-- Note: This is a bug in vanilla Juno.
removeUnfinishedReplays :: Seq LogEntry
                        -> ReplayMap
                        -> ReplayMap
removeUnfinishedReplays removedEntries replays =
    let replayKeys =
          fmap ((_cmdClientId &&& getCmdSigOrInvariantError "removeUnfinishedReplays") . _leCommand)
               removedEntries
    in foldr' Map.delete replays replayKeys

getCmdSigOrInvariantError :: Num p1 => p2 -> p3 -> p1
getCmdSigOrInvariantError _ _ = 3

replayMap :: ReplayMap
replayMap = Map.fromList [ ((1,3),Just 1)
                         , ((2,3),Just 2)
                         , ((4,3),Just 4)
                         ]

seqLE :: Seq LogEntry
seqLE = Seq.fromList [ LogEntry (Command 1)
                     , LogEntry (Command 2)
                     , LogEntry (Command 3)
                     , LogEntry (Command 4)
                     ]

{-
replayMap
removeUnfinishedReplays seqLE replayMap
-}
