{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module U where

-- https://www.youtube.com/watch?v=mhKUHpQZIoc

import qualified Data.Map.Strict as M
import Protolude

------------------------------------------------------------------------------
{-
Application or Protocol Update
- vote starts when someone proposes an update
- everybody gets to vote
- at the deadline (some number of slots) proposal passes/failed
  based on simple majority of the people who voted.
- details about stake, timing, etc., are for you to flush out
-}
------------------------------------------------------------------------------
-- Note: no "time" used in my initial solution.

type Address       = Int
type UXTO          = Int
type AddressToUXTO = M.Map Int Int
type AccountId     = Int
data Account = Account
  { accountId :: AccountId
  , uxto      :: [Address]
  }

update :: [Account] -> AddressToUXTO -> Bool
update as aToU = inFavor > against
 where
  (against, inFavor) = foldr go (0,0) as
  go a (n,p) = case vote a aToU of
    Nothing -> (n,p)
    Just v  -> if accountId a == 1 || accountId a == 2 then (n+v,p) else (n,p+v)

vote :: Account -> AddressToUXTO -> Maybe UXTO
vote a aToU = foldr (go . Just) (Just 0) (uxto a)
 where
  go (Just x) (Just acc) = do
    v <- M.lookup x aToU
    return (v + acc)
  go _ _ = Nothing

addressToUXTO :: AddressToUXTO
addressToUXTO = foldr (\x acc -> M.insert x x acc) M.empty [1 .. 99]

accounts :: [Account]
accounts =
  [ Account 1 [1,99,2,98]
  , Account 2 [3,97,4,96]
  , Account 3 [50, 51]
  , Account 4 [60]
  ]

result :: Bool
result = update accounts addressToUXTO

------------------------------------------------------------------------------
{-
After watching presentation
- people voting more than once
- people voting after deadline
- end early if majority reached
- use a list of input/output events at each slot number
- the only output event I know of is success/failure
-}

type SlotNumber = Int
-- | vote is amount of stake, negative if against, positive otherwise
type Vote       = Int

data Event = Event
  { slot :: SlotNumber
  , typ  :: EventType
  } deriving (Eq, Show)

data EventType
  = Vote AccountId Int
  | Deadline
  deriving (Eq, Show)

--         total   events     aleadyVoted   valid    voteTotal
update' :: UXTO -> [Event] -> ([AccountId], [Event], UXTO)
--         slot vote ended  valid       voteTotal     pass/fail
        -> (SlotNumber,     [Event],    UXTO,         Bool)
update' totalStake es0 au@(alreadyVoted, vs, u) = case es0 of
  []     -> (-1, vs, u, majority u)
  (e:es) -> case typ e of
    Deadline ->
      (slot e, e:vs, u, majority u)
    Vote aid stake ->
      if | aid `elem` alreadyVoted -> update' totalStake es au
         | majority (u + stake) -> (slot e, e:vs, u + stake, True)
         | otherwise -> update' totalStake es (aid:alreadyVoted, e:vs, u + stake)
 where
  majority u0 = u0 > totalStake `div` 2

events :: [Event]
events = [Event 0 $ Vote 1 34
         ,Event 0 $ Vote 2 43
         ,Event 1 $ Vote 1 (-34) -- ignored, no voting twice
         ,Event 2 $ Vote 3 (-40)
         ,Event 3   Deadline
         ,Event 4 $ Vote 4 50    -- ignored, after deadline
         ]

result1,result2,result3 :: (SlotNumber, [Event], UXTO, Bool)
result1 = update'  67 events ([], [], 0)
result2 = update'  68 events ([], [], 0)
result3 = update' 200 events ([], [], 0)
