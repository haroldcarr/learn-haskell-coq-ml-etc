{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 30 (Wed) 17:45:33 by carr.
-}

module X07Solver where

import X07GameDef
import X07StringParserTerrain

done game b = b == Block (goal game) (goal game)

neighborsWithHistory :: Game -> Block -> [Move] -> [(Block, [Move])]
neighborsWithHistory game b history =
      map (\(b,m) -> (b, m:history)) $ legalNeighbors game b

-- TODO (Set Block)
newNeighborsOnly :: [(Block, [Move])] -> [Block] -> [(Block, [Move])]
newNeighborsOnly neighbors explored =
      filter (\(b,_) -> (b `notElem` explored)) neighbors

from :: Game -> [(Block, [Move])] -> [Block] -> [(Block, [Move])]
from game (p@(b,h):t) explored =
    let nn = newNeighborsOnly (neighborsWithHistory game b h) explored
    in p : from game (t ++ nn) (explored ++ [b])
from _ _ _ = []

pathsFromStart game = from game [(startBlock game, [])] []

pathsToGoal game = filter (\(b,m) -> done game b) (pathsFromStart game)

solution game =
    case pathsToGoal game of
        (_,x):t -> reverse x
        _       -> []

startBlock game = Block (startPos game) (startPos game)

-- End of file
