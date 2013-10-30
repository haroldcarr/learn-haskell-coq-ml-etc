{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 29 (Tue) 23:45:05 by carr.
-}

module X07Solver where

import X07GameDef
import X07StringParserTerrain

done level b = b == Block (goal level) (goal level)

neighborsWithHistory :: String -> Block -> [Move] -> [(Block, [Move])]
neighborsWithHistory level b history =
      map (\(b,m) -> (b, m:history)) $ legalNeighbors level b

-- TODO (Set Block)
newNeighborsOnly :: [(Block, [Move])] -> [Block] -> [(Block, [Move])]
newNeighborsOnly neighbors explored =
      filter (\(b,_) -> not (b `elem` explored)) neighbors

from :: String -> [(Block, [Move])] -> [Block] -> [(Block, [Move])]
from level ((b,h):t) explored =
    let nn = newNeighborsOnly (neighborsWithHistory level b h) explored
    in (b,h) : (from level (t ++ nn) (explored ++ [b]))
from _ _ _ = []

pathsFromStart level startBlock = from level [(startBlock, [])] []

pathsToGoal level startBlock = filter (\(b,m) -> done level b) (pathsFromStart level startBlock)

solution level startBlock =
    case pathsToGoal level startBlock of
        (_,x):t -> reverse x
        _       -> []

-- End of file
