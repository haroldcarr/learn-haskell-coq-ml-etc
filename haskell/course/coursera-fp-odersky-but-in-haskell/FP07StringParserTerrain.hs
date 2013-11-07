{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Nov 06 (Wed) 18:25:20 by carr.
-}

module FP07StringParserTerrain where

import Data.Vector as V
import SplitLines
import FP07GameDef

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

mkGame level = Game terrain startPos goal
  where
    vlevel   = vector level
    terrain  = terrainFunction vlevel
    startPos = findChar 'S'    vlevel
    goal     = findChar 'T'    vlevel

-- End of file.
