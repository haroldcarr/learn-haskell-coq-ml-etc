{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2014 Mar 06 (Thu) 13:46:19 by Harold Carr.
-}

module FP07GameDef where

data Game = Game { terrain  :: Pos -> Bool
                 , startPos :: Pos
                 , goal     :: Pos
                 }

data Pos = Pos { x :: Int, y :: Int } deriving (Eq, Read, Show)

{-# ANN dyp "HLint: ignore Redundant bracket" #-}
dxp, dyp :: Pos -> Int -> Pos
dxp p d = Pos (x p + d)  (y p)
dyp p d = Pos (x p)     ((y p) + d)

data Move = MLeft | MRight | MUp | MDown deriving (Eq, Read, Show)

data Block = Block { b1 :: Pos,  b2 :: Pos } deriving (Eq, Read, Show)

{- TODO the effect of:
newBlock b1 b2 =
    if (x b1) > (x b2) || (y b1) > (y b2)
    then error "Invalid block position: b1=" ++ (show b1) ++ ", b2=" + (show b2)
    else Block b1 b2
-}

dxb, dyb :: Block -> Int -> Int -> Block
dxb b d1 d2 = Block (dxp (b1 b) d1) (dxp (b2 b) d2)
dyb b d1 d2 = Block (dyp (b1 b) d1) (dyp (b2 b) d2)

move :: (Block -> t1 -> t2 -> t) -> t1 -> t2 -> t1 -> t2 -> t1 -> t2 -> Block -> t
move dxyb s1 s2 e1 e2 o1 o2 b
    | isStanding b         = dxyb b s1 s2
    | x (b1 b) == x (b2 b) = dxyb b e1 e2
    | otherwise            = dxyb b o1 o2

left, right, up, down :: Block -> Block
left  = move dyb (-2) (-1)  (-1) (-2)  (-1) (-1)
right = move dyb   1    2     2    1     1    1
up    = move dxb (-2) (-1)  (-1) (-1)  (-1) (-2)
down  = move dxb   1    2     1    1     2    1

neighbors :: Block -> [(Block, Move)]
neighbors b = [(left b, MLeft), (right b, MRight), (up b, MUp), (down b, MDown)]

legalNeighbors :: Game -> Block -> [(Block, Move)]
legalNeighbors game b = Prelude.filter (\(n,_) -> isLegal game n) (neighbors b)

isStanding :: Block -> Bool
isStanding b = b1 b == b2 b

isLegal :: Game -> Block -> Bool
isLegal game b = terrain game (b1 b) && terrain game (b2 b)

-- End of file.
