{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 30 (Wed) 13:25:32 by carr.
-}

module X07GameDef where

data Pos = Pos { x :: Int, y :: Int } deriving (Eq, Read, Show)

{-# ANN dyp "HLint: ignore Redundant bracket" #-}
dxp p d = Pos (x p + d)  (y p)
dyp p d = Pos (x p)     ((y p) + d)

type Terrain = Pos -> Bool

data Move = MLeft | MRight | MUp | MDown deriving (Eq, Read, Show)

data Block = Block { b1 :: Pos,  b2 :: Pos } deriving (Eq, Read, Show)

{- TODO the effect of:
newBlock b1 b2 =
    if (x b1) > (x b2) || (y b1) > (y b2)
    then error "Invalid block position: b1=" ++ (show b1) ++ ", b2=" + (show b2)
    else Block b1 b2
-}

dxb :: Block -> Int -> Int -> Block
dxb b d1 d2 = Block (dxp (b1 b) d1) (dxp (b2 b) d2)
dyb :: Block -> Int -> Int -> Block
dyb b d1 d2 = Block (dyp (b1 b) d1) (dyp (b2 b) d2)

left  b | isStanding b         = dyb b (-2) (-1)
        | x (b1 b) == x (b2 b) = dyb b (-1) (-2)
        | otherwise            = dyb b (-1) (-1)

right b | isStanding b         = dyb b 1 2
        | x (b1 b) == x (b2 b) = dyb b 2 1
        | otherwise            = dyb b 1 1

up    b | isStanding b         = dxb b (-2) (-1)
        | x (b1 b) == x (b2 b) = dxb b (-1) (-1)
        | otherwise            = dxb b (-1) (-2)

down  b | isStanding b         = dxb b 1 2
        | x (b1 b) == x (b2 b) = dxb b 1 1
        | otherwise            = dxb b 2 1

neighbors b = [(left b, MLeft), (right b, MRight), (up b, MUp), (down b, MDown)]

-- legalNeighbors : moved to StringParserTerrain

isStanding b = b1 b == b2 b

-- isLegal : moved to StringParserTerrain

-- End of file.
