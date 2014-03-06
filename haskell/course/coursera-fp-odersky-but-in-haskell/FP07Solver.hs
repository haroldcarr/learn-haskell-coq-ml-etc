{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2014 Mar 06 (Thu) 13:50:49 by Harold Carr.
-}

module FP07Solver where

import           FP07GameDef

done :: Game -> Block -> Bool
done game b = b == Block (goal game) (goal game)

neighborsWithHistory :: Game -> Block -> [Move] -> [(Block, [Move])]
neighborsWithHistory game b0 history =
      map (\(b,m) -> (b, m:history)) $ legalNeighbors game b0

-- TODO (Set Block)
newNeighborsOnly :: [(Block, [Move])] -> [Block] -> [(Block, [Move])]
newNeighborsOnly neighbors0 explored =
      filter (\(b,_) -> (b `notElem` explored)) neighbors0

from :: Game -> [(Block, [Move])] -> [Block] -> [(Block, [Move])]
from game (p@(b,h):t) explored =
    let nn = newNeighborsOnly (neighborsWithHistory game b h) explored
    in p : from game (t ++ nn) (explored ++ [b])
from _ _ _ = []

pathsFromStart :: Game -> [(Block, [Move])]
pathsFromStart game = from game [(startBlock game, [])] []

pathsToGoal :: Game -> [(Block, [Move])]
pathsToGoal game = filter (\(b,_) -> done game b) (pathsFromStart game)

solution :: Game -> [Move]
solution game =
    case pathsToGoal game of
        (_,s):_ -> reverse s
        _       -> []

startBlock :: Game -> Block
startBlock game = Block (startPos game) (startPos game)

-- End of file
