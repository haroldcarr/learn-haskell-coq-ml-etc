{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 30 (Wed) 13:32:54 by carr.
-}

module X07StringParserTerrain where

import Data.Vector as V
import SplitLines
import X07GameDef

{-# ANN terrainFunction "HLint: ignore Redundant if" #-}
terrainFunction :: Vector (Vector Char) -> Pos -> Bool
terrainFunction levelVector pos =
    if x pos >= V.length levelVector || y pos >= V.length (levelVector ! 0)
    || x pos <  0                    || y pos <  0
    then False
    else '-' /= (levelVector ! x pos) ! y pos

findChar :: Char -> Vector (Vector Char) -> Pos
findChar c levelVector = checkRow 0
  where
    max = V.length levelVector
    checkRow x =
        if x >= max then error "NoSuchElementException"
        else case elemIndex c (levelVector ! x) of
                 Just y  -> Pos x y
                 Nothing -> checkRow $ x + 1

vector :: String -> Vector (Vector Char)
vector level = V.fromList $ Prelude.map V.fromList (splitLines level)

terrain  level = terrainFunction $ vector level
startPos level = findChar 'S' $ vector level
goal     level = findChar 'T' $ vector level

-- from GameDef
isLegal level b = terrain level (b1 b) && terrain level (b2 b)

-- from GameDef
legalNeighbors level b = Prelude.filter (\(n,_) -> isLegal level n) (neighbors b)

-- End of file.
