{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 30 (Wed) 13:18:10 by carr.
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
      filter (\(b,_) -> (b `notElem` explored)) neighbors

from :: String -> [(Block, [Move])] -> [Block] -> [(Block, [Move])]
from level (p@(b,h):t) explored =
    let nn = newNeighborsOnly (neighborsWithHistory level b h) explored
    in p : from level (t ++ nn) (explored ++ [b])
from _ _ _ = []

pathsFromStart level = from level [(startBlock level, [])] []

pathsToGoal level = filter (\(b,m) -> done level b) (pathsFromStart level)

solution level =
    case pathsToGoal level of
        (_,x):t -> reverse x
        _       -> []

startBlock level = Block (startPos level) (startPos level)

-- End of file
