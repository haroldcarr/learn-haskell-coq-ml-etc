{-# LANGUAGE TupleSections #-}

module Interleave where

import Control.Arrow ((>>>))

-- >>> mkVarNumsPerShard [1,3,5]
-- [[1],[1,2,3],[1,2,3,4,5]]
mkVarNumsPerShard :: [Int] -> [[Int]]
mkVarNumsPerShard  = map (\x -> [1..x])

-- >>> associateCharPerShard [[1],[1,2,3],[1,2,3,4,5]]
-- [('a',[1]),('b',[1,2,3]),('c',[1,2,3,4,5])]
associateCharPerShard :: [[Int]] -> [(Char, [Int])]
associateCharPerShard  = zip ['a'..]

-- >>> mkShardVars [('a',[1]),('b',[1,2,3]),('c',[1,2,3,4,5])]
-- [["a1"],["b1","b2","b3"],["c1","c2","c3","c4","c5"]]
mkShardVars :: [(Char, [Int])] -> [[String]]
mkShardVars = foldr (\(c,ns) acc -> prepend c ns : acc) []
 where
  prepend :: Show a => Char -> [a] -> [String]
  prepend c = map ((c :) . show)

-- >>> assignShardNums [["a1"],["b1","b2","b3"],["c1","c2","c3","c4","c5"]]
-- [[(0,"a1")],[(1,"b1"),(1,"b2"),(1,"b3")],[(2,"c1"),(2,"c2"),(2,"c3"),(2,"c4"),(2,"c5")]]
assignShardNums :: (Enum a, Num a) => [[b]] -> [[(a, b)]]
assignShardNums bs = map (\(s,vs) -> map (s,) vs) (zip [0..] bs)

-- >>> groupNonOverlapping [[(0,"a1")],[(1,"b1"),(1,"b2"),(1,"b3")],[(2,"c1"),(2,"c2"),(2,"c3"),(2,"c4"),(2,"c5")]]
-- [[(0,"a1"),(1,"b1"),(2,"c1")],[(1,"b2"),(2,"c2")],[(1,"b3"),(2,"c3")],[(2,"c4")],[(2,"c5")]]
groupNonOverlapping :: [[a]] -> [[a]]
groupNonOverlapping [] = []
groupNonOverlapping xs = map head xs : groupNonOverlapping (tailsFilterNull xs)
 where
  tailsFilterNull :: [[a]] -> [[a]]
  tailsFilterNull = filter (not . null) . map tail

-- >>> mkShardSendGroups [1,3,5]
-- [[(0,"a1"),(1,"b1"),(2,"c1")],[(1,"b2"),(2,"c2")],[(1,"b3"),(2,"c3")],[(2,"c4")],[(2,"c5")]]
mkShardSendGroups :: [Int] -> [[(Integer, String)]]
mkShardSendGroups =   mkVarNumsPerShard
                  >>> associateCharPerShard
                  >>> mkShardVars
                  >>> assignShardNums
                  >>> groupNonOverlapping

{-
interleave2 :: [a] -> [a] -> [a]
interleave2 xs ys = concat (transpose [xs, ys])

-- NO
interleave :: [[a]] -> [a]
interleave = concat . transpose

----

mkVarNumsPerShard [1,3,5]
associateCharPerShard [[1],[1,2,3],[1,2,3,4,5]]
mkShardVars
assignShardNums

groupNonOverlapping

NO
interleave [[(0,"a1"),(0,"a2"),(0,"a3")],[(1,"b1"),(1,"b2"),(1,"b3"),(1,"b4"),(1,"b5"),(1,"b6")],[(2,"c1"),(2,"c2"),(2,"c3"),(2,"c4"),(2,"c5"),(2,"c6"),(2,"c7"),(2,"c8"),(2,"c9")]]
[(0,"a1"),(1,"b1"),(2,"c1"),(0,"a2"),(1,"b2"),(2,"c2"),(0,"a3"),(1,"b3"),(2,"c3"),(1,"b4"),(2,"c4"),(1,"b5"),(2,"c5"),(1,"b6"),(2,"c6"),(2,"c7"),(2,"c8"),(2,"c9")]

------
map head [[(0,"a1"),(0,"a2"),(0,"a3")],[(1,"b1"),(1,"b2"),(1,"b3"),(1,"b4"),(1,"b5"),(1,"b6")],[(2,"c1"),(2,"c2"),(2,"c3"),(2,"c4"),(2,"c5"),(2,"c6"),(2,"c7"),(2,"c8"),(2,"c9")]]
map head $ tailsFilterNull $ tailsFilterNull $ tailsFilterNull $ tailsFilterNull $ tailsFilterNull $ tailsFilterNull [[(0,"a1"),(0,"a2"),(0,"a3")],[(1,"b1"),(1,"b2"),(1,"b3"),(1,"b4"),(1,"b5"),(1,"b6")],[(2,"c1"),(2,"c2"),(2,"c3"),(2,"c4"),(2,"c5"),(2,"c6"),(2,"c7"),(2,"c8"),(2,"c9")]]

[1,1,2,2,3,3,4,4,5,5,6,7,8,9,10]
                                  [0,0,0,0,0]    [1,1,1,1,1,1,1,1,1,1]
[(0,1),(1,1),(0,2),(1,2),(0,3),(1,3),(0,4),(1,4),(0,5),(1,5),(1,6),(1,7),(1,8),(1,9),(1,10)]
-}
