{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2014 Mar 06 (Thu) 13:54:29 by Harold Carr.
-}

module FP07StringParserTerrain where

import           Data.Vector as V
import           FP07GameDef
import           SplitLines

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
    max' = V.length levelVector
    checkRow x0 =
        if x0 >= max' then error "NoSuchElementException"
        else case elemIndex c (levelVector ! x0) of
                 Just y0  -> Pos x0 y0
                 Nothing -> checkRow $ x0 + 1

vector :: String -> Vector (Vector Char)
vector level = V.fromList $ Prelude.map V.fromList (splitLines level)

mkGame :: String -> Game
mkGame level = Game terrain0 startPos0 goal0
  where
    vlevel0   = vector level
    terrain0  = terrainFunction vlevel0
    startPos0 = findChar 'S'    vlevel0
    goal0     = findChar 'T'    vlevel0

-- End of file.
