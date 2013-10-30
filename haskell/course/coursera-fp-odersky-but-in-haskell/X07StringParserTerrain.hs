{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 29 (Tue) 21:58:56 by carr.
-}

module X07StringParserTerrain where

import Data.Vector as V
import X07GameDef

terrainFunction :: Vector (Vector Char) -> (Pos -> Bool)
terrainFunction levelVector =
    \p -> if x p >= V.length levelVector || y p >= V.length (levelVector ! 0)
          || x p < 0                     || y p < 0
          then False
          else '-' /= (levelVector ! (x p)) ! (y p)

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
vector level =
    V.fromList $ Prelude.map (\str -> V.fromList str) (splitLines level)
  -- from Real World Haskell
  where
    splitLines :: String -> [String]
    splitLines [] = []
    splitLines cs =
        let (pre, suf) = Prelude.break isLineTerminator cs
        in pre : case suf of
                     ('\r':'\n':rest) -> splitLines rest
                     ('\r':rest)      -> splitLines rest
                     ('\n':rest)      -> splitLines rest
                     _                -> []
    isLineTerminator c = c == '\r' || c == '\n'

terrain  level = terrainFunction $ vector level
startPos level = findChar 'S' $ vector level
goal     level = findChar 'T' $ vector level

-- from GameDef
isLegal level b = terrain level (b1 b) && terrain level (b2 b)

-- from GameDef
legalNeighbors level b = Prelude.filter (\(n,_) -> isLegal level n) (neighbors b)

-- End of file.
