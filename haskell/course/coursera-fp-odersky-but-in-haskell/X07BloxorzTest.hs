{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Oct 30 (Wed) 13:11:25 by carr.
-}

module X07BloxorzTest where

import Test.HUnit
import AssertError
import Data.Vector as V
import X07GameDef
import X07Solver
import X07StringParserTerrain

solve :: String -> [Move] -> Block
solve level = Prelude.foldl step (startBlock level)
  where
    step blockAcc move =
        (case move of
             MLeft  -> left
             MRight -> right
             MUp    -> up
             MDown  -> down) blockAcc

level =
    "ooo-------\n\
    \oSoooo----\n\
    \ooooooooo-\n\
    \-ooooooooo\n\
    \-----ooToo\n\
    \------ooo-"

optsolution = [MRight, MRight, MDown, MRight, MRight, MRight, MDown]

tests = TestList
    [teq "vector" (vector level) (V.fromList [V.fromList "ooo-------"
                                             ,V.fromList "oSoooo----"
                                             ,V.fromList "ooooooooo-"
                                             ,V.fromList "-ooooooooo"
                                             ,V.fromList "-----ooToo"
                                             ,V.fromList "------ooo-"
                                             ])
    ,teq "terrain function level 1 - 0, 0" (terrain level (Pos 0  0))                  True
    ,teq "terrain function level 1 - 4,11" (terrain level (Pos 4 11))                  False
    ,teq "findChar level 1 - start"        (startPos level)                            (Pos 1 1)
    ,teq "block isStanding 1"              (isStanding $ Block (Pos 1 1) (Pos 1 1))    True
    ,teq "block isStanding 2"              (isStanding $ Block (Pos 1 1) (Pos 1 2))    False
    ,teq "isLegal 1"                       (isLegal level $ Block (Pos 0 0) (Pos 0 0)) True
    ,teq "isLegal 2"                       (isLegal level $ Block (Pos 0 0) (Pos 0 1)) True
    ,teq "isLegal 3"                       (isLegal level $ Block (Pos 0 1) (Pos 0 1)) True
    ,teq "isLegal 4"                       (isLegal level $ Block (Pos 0 1) (Pos 0 2)) True
    ,teq "isLegal 5"                       (isLegal level $ Block (Pos 0 2) (Pos 0 2)) True
    ,teq "isLegal 6"                       (isLegal level $ Block (Pos 0 2) (Pos 0 3)) False
    ,teq "isLegal 7"                       (isLegal level $ Block (Pos 0 2) (Pos 0 2)) True
    ,teq "isLegal 8"                       (isLegal level $ Block (Pos 2 8) (Pos 2 8)) True
    ,teq "isLegal 9"                       (isLegal level $ Block (Pos 2 8) (Pos 2 9)) False
    ,teq "startBlock x b1"                 (x (b1 (startBlock level)))                 1
    ,teq "startBlock x b2"                 (x (b2 (startBlock level)))                 1
    ,teq "startBlock y b1"                 (y (b1 (startBlock level)))                 1
    ,teq "startBlock y bb"                 (y (b2 (startBlock level)))                 1
    ,teq "neighbors"                       (neighbors (startBlock level))              [(Block (Pos   1 (-1)) (Pos 1 0), MLeft)
                                                                                       ,(Block (Pos   1   2)  (Pos 1 3), MRight)
                                                                                       ,(Block (Pos (-1)  1)  (Pos 0 1), MUp)
                                                                                       ,(Block (Pos   2   1)  (Pos 3 1), MDown)
                                                                                       ]
    ,teq "legalNeighbors"                  (legalNeighbors level (startBlock level))   [(Block (Pos   1   2)  (Pos 1 3), MRight)
                                                                                       ,(Block (Pos   2   1)  (Pos 3 1), MDown)
                                                                                       ]
    ,teq "done"                            (done level (Block (Pos 4 7) (Pos 4 7)))    True
    ,teq "not done"                        (done level (Block (Pos 4 7) (Pos 4 8)))    False
    ,teq "neighborsWithHistory1"           (neighborsWithHistory level (Block (Pos 1 1) (Pos 1 1)) [MLeft,MUp])
                                           [(Block (Pos 1 2) (Pos 1 3), [MRight,MLeft,MUp])
                                           ,(Block (Pos 2 1) (Pos 3 1), [MDown,MLeft,MUp])
                                           ]
    ,teq "neighborsWithHistory2"           (neighborsWithHistory level (Block (Pos 1 2) (Pos 1 3)) [MRight])
                                           [(Block (Pos 1 1) (Pos 1 1), [MLeft, MRight])
                                           ,(Block (Pos 1 4) (Pos 1 4), [MRight, MRight])
                                           ,(Block (Pos 2 2) (Pos 2 3), [MDown, MRight])
                                           ]
    ,teq "neighborsWithHistory3"           (neighborsWithHistory level (Block (Pos 2 1) (Pos 3 1)) [MDown])
                                           [(Block (Pos 2 2) (Pos 3 2), [MRight, MDown])
                                           ,(Block (Pos 1 1) (Pos 1 1), [MUp, MDown])
                                           ]
    ,teq "newNeighborsOnly1"               (newNeighborsOnly [(Block (Pos 1 2) (Pos 1 3), [MRight,MLeft,MUp])
                                                             ,(Block (Pos 2 1) (Pos 3 1), [MDown, MLeft,MUp])
                                                             ]
                                                             [Block (Pos 1 2) (Pos 1 3)
                                                             ,Block (Pos 1 1) (Pos 1 1)
                                                             ])
                                           [(Block (Pos 2 1) (Pos 3 1), [MDown,MLeft,MUp])]
    ,teq "misc"                            (isStanding $ Block (Pos 3 7) (Pos 5 8))    False
    ,teq "from"                            (from level [(startBlock level, [])] [])
                                           [(Block {b1 = Pos {x = 1, y = 1}, b2 = Pos {x = 1, y = 1}},[]),(Block {b1 = Pos {x = 1, y = 2}, b2 = Pos {x = 1, y = 3}},[MRight]),(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 3, y = 1}},[MDown]),(Block {b1 = Pos {x = 1, y = 4}, b2 = Pos {x = 1, y = 4}},[MRight,MRight]),(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 2, y = 3}},[MDown,MRight]),(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 3, y = 2}},[MRight,MDown]),(Block {b1 = Pos {x = 2, y = 4}, b2 = Pos {x = 3, y = 4}},[MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 2, y = 1}},[MLeft,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 4}, b2 = Pos {x = 2, y = 4}},[MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 2}, b2 = Pos {x = 3, y = 3}},[MDown,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 3, y = 3}},[MRight,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 2}, b2 = Pos {x = 1, y = 2}},[MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 3, y = 3}},[MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 5}, b2 = Pos {x = 3, y = 5}},[MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 0, y = 1}, b2 = Pos {x = 1, y = 1}},[MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 5}, b2 = Pos {x = 2, y = 6}},[MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 1}, b2 = Pos {x = 3, y = 1}},[MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 4}, b2 = Pos {x = 3, y = 4}},[MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 3}, b2 = Pos {x = 1, y = 3}},[MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 0}, b2 = Pos {x = 1, y = 1}},[MLeft,MUp,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 3}, b2 = Pos {x = 1, y = 4}},[MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 3}, b2 = Pos {x = 1, y = 3}},[MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 6}, b2 = Pos {x = 3, y = 6}},[MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 1, y = 5}, b2 = Pos {x = 1, y = 5}},[MUp,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 5}, b2 = Pos {x = 4, y = 5}},[MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 0, y = 0}, b2 = Pos {x = 1, y = 0}},[MLeft,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 0, y = 2}, b2 = Pos {x = 1, y = 2}},[MRight,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 7}, b2 = Pos {x = 2, y = 7}},[MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 3, y = 6}},[MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 1}, b2 = Pos {x = 2, y = 1}},[MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 3, y = 6}},[MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 4}, b2 = Pos {x = 2, y = 4}},[MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 1}, b2 = Pos {x = 1, y = 2}},[MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 4}, b2 = Pos {x = 1, y = 5}},[MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 0, y = 0}, b2 = Pos {x = 0, y = 1}},[MUp,MLeft,MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 0}, b2 = Pos {x = 2, y = 1}},[MDown,MLeft,MUp,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 5}, b2 = Pos {x = 1, y = 5}},[MRight,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 2, y = 4}},[MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 1}, b2 = Pos {x = 1, y = 2}},[MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 1, y = 4}, b2 = Pos {x = 1, y = 5}},[MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 7}, b2 = Pos {x = 3, y = 7}},[MRight,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 6}, b2 = Pos {x = 4, y = 6}},[MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 6}, b2 = Pos {x = 4, y = 7}},[MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 0}, b2 = Pos {x = 2, y = 0}},[MDown,MLeft,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 2, y = 2}},[MDown,MRight,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 7}, b2 = Pos {x = 4, y = 7}},[MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 7}, b2 = Pos {x = 3, y = 7}},[MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 5}, b2 = Pos {x = 4, y = 6}},[MDown,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 0}, b2 = Pos {x = 2, y = 0}},[MLeft,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 2}, b2 = Pos {x = 2, y = 2}},[MRight,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 0, y = 1}, b2 = Pos {x = 0, y = 1}},[MUp,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 7}, b2 = Pos {x = 3, y = 7}},[MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 5}, b2 = Pos {x = 4, y = 6}},[MDown,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 3}, b2 = Pos {x = 2, y = 3}},[MLeft,MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 5}, b2 = Pos {x = 2, y = 5}},[MRight,MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 0}, b2 = Pos {x = 1, y = 0}},[MLeft,MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 0, y = 1}, b2 = Pos {x = 0, y = 2}},[MUp,MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 2, y = 2}},[MDown,MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 4}, b2 = Pos {x = 2, y = 5}},[MDown,MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 0, y = 2}, b2 = Pos {x = 0, y = 2}},[MRight,MUp,MLeft,MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 2, y = 2}},[MRight,MDown,MLeft,MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 2, y = 2}},[MLeft,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 5}, b2 = Pos {x = 2, y = 5}},[MRight,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 3}, b2 = Pos {x = 3, y = 4}},[MDown,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 1, y = 0}, b2 = Pos {x = 1, y = 0}},[MLeft,MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 0, y = 1}, b2 = Pos {x = 0, y = 2}},[MUp,MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 2, y = 2}},[MDown,MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 4}, b2 = Pos {x = 2, y = 5}},[MDown,MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 8}, b2 = Pos {x = 3, y = 8}},[MRight,MRight,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 7}, b2 = Pos {x = 4, y = 7}},[MDown,MRight,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 7}, b2 = Pos {x = 4, y = 8}},[MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 8}},[MRight,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 7}},[MUp,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 5, y = 6}, b2 = Pos {x = 5, y = 7}},[MDown,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 2, y = 2}},[MRight,MDown,MLeft,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 4, y = 6}},[MLeft,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 8}, b2 = Pos {x = 4, y = 8}},[MRight,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 5, y = 7}, b2 = Pos {x = 5, y = 7}},[MDown,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 8}, b2 = Pos {x = 3, y = 9}},[MRight,MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 7}, b2 = Pos {x = 5, y = 7}},[MDown,MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 7}, b2 = Pos {x = 4, y = 7}},[MRight,MDown,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 0, y = 0}, b2 = Pos {x = 0, y = 0}},[MUp,MLeft,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 1, y = 3}, b2 = Pos {x = 2, y = 3}},[MRight,MRight,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 0, y = 2}, b2 = Pos {x = 0, y = 2}},[MUp,MRight,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 2}, b2 = Pos {x = 3, y = 2}},[MDown,MRight,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 8}, b2 = Pos {x = 3, y = 9}},[MRight,MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 7}, b2 = Pos {x = 5, y = 7}},[MDown,MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 7}, b2 = Pos {x = 4, y = 7}},[MRight,MDown,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 3}, b2 = Pos {x = 3, y = 3}},[MDown,MLeft,MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 3, y = 5}},[MDown,MRight,MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 0, y = 0}, b2 = Pos {x = 0, y = 0}},[MLeft,MUp,MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 2, y = 3}},[MRight,MDown,MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 1}, b2 = Pos {x = 3, y = 2}},[MDown,MDown,MLeft,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 2, y = 3}},[MLeft,MDown,MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 6}, b2 = Pos {x = 2, y = 6}},[MRight,MDown,MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 4}, b2 = Pos {x = 3, y = 5}},[MDown,MDown,MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 6}, b2 = Pos {x = 2, y = 7}},[MRight,MRight,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 4, y = 5}},[MDown,MRight,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 2}, b2 = Pos {x = 3, y = 2}},[MLeft,MDown,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 3, y = 5}},[MRight,MDown,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 0, y = 0}, b2 = Pos {x = 0, y = 0}},[MLeft,MUp,MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 2, y = 3}},[MRight,MDown,MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 1}, b2 = Pos {x = 3, y = 2}},[MDown,MDown,MLeft,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 2, y = 3}},[MLeft,MDown,MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 6}, b2 = Pos {x = 2, y = 6}},[MRight,MDown,MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 4}, b2 = Pos {x = 3, y = 5}},[MDown,MDown,MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 8}},[MDown,MRight,MRight,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 9}},[MRight,MDown,MRight,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 9}, b2 = Pos {x = 4, y = 9}},[MRight,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 7}, b2 = Pos {x = 3, y = 8}},[MUp,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 5, y = 7}, b2 = Pos {x = 5, y = 8}},[MDown,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 3, y = 5}},[MLeft,MUp,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 8}, b2 = Pos {x = 3, y = 8}},[MRight,MUp,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 6}, b2 = Pos {x = 2, y = 7}},[MUp,MUp,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 5, y = 8}, b2 = Pos {x = 5, y = 8}},[MRight,MDown,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 3}, b2 = Pos {x = 2, y = 3}},[MRight,MRight,MDown,MLeft,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 1}, b2 = Pos {x = 3, y = 2}},[MDown,MRight,MDown,MLeft,MUp,MLeft,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 5}, b2 = Pos {x = 4, y = 5}},[MLeft,MLeft,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 6}, b2 = Pos {x = 2, y = 6}},[MUp,MLeft,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 5, y = 6}, b2 = Pos {x = 5, y = 6}},[MDown,MLeft,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 9}, b2 = Pos {x = 4, y = 9}},[MRight,MRight,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 8}, b2 = Pos {x = 2, y = 8}},[MUp,MRight,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 5, y = 8}, b2 = Pos {x = 5, y = 8}},[MDown,MRight,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 9}},[MDown,MRight,MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 6}, b2 = Pos {x = 5, y = 6}},[MLeft,MDown,MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 5, y = 8}},[MRight,MDown,MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 9}},[MRight,MRight,MDown,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 3}, b2 = Pos {x = 3, y = 3}},[MDown,MRight,MRight,MUp,MLeft,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 9}},[MDown,MRight,MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 6}, b2 = Pos {x = 5, y = 6}},[MLeft,MDown,MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 5, y = 8}},[MRight,MDown,MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 4, y = 9}},[MRight,MRight,MDown,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 1}, b2 = Pos {x = 3, y = 2}},[MLeft,MDown,MLeft,MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 4}, b2 = Pos {x = 3, y = 5}},[MRight,MDown,MLeft,MUp,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 2, y = 7}, b2 = Pos {x = 2, y = 8}},[MRight,MRight,MDown,MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 6}},[MRight,MDown,MDown,MRight,MUp,MRight,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 8}, b2 = Pos {x = 2, y = 8}},[MRight,MRight,MRight,MDown,MRight,MUp,MRight,MDown]),(Block {b1 = Pos {x = 2, y = 7}, b2 = Pos {x = 2, y = 8}},[MRight,MRight,MDown,MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 6}},[MRight,MDown,MDown,MRight,MUp,MLeft,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 6}},[MLeft,MUp,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 3, y = 9}, b2 = Pos {x = 3, y = 9}},[MRight,MUp,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 7}, b2 = Pos {x = 2, y = 8}},[MUp,MUp,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 5, y = 6}, b2 = Pos {x = 5, y = 6}},[MLeft,MDown,MRight,MDown,MRight,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 4, y = 8}, b2 = Pos {x = 5, y = 8}},[MDown,MRight,MUp,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 8}, b2 = Pos {x = 2, y = 8}},[MRight,MUp,MUp,MRight,MDown,MRight,MDown,MRight,MRight]),(Block {b1 = Pos {x = 2, y = 7}, b2 = Pos {x = 2, y = 8}},[MRight,MUp,MLeft,MDown,MRight,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 6}},[MUp,MLeft,MDown,MRight,MDown,MRight,MRight,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 6}},[MUp,MLeft,MDown,MRight,MRight,MRight,MDown,MDown,MRight]),(Block {b1 = Pos {x = 3, y = 6}, b2 = Pos {x = 3, y = 6}},[MRight,MRight,MDown,MLeft,MUp,MRight,MDown,MDown,MRight])]

{- TODO
    ,teq "pathsFromStart" (pathsFromStart level)
                          [(Block (Pos 3 9) (Pos 3 9), [MRight, MUp, MRight, MDown, MRight, MRight, MDown, MRight, MRight])]
-}
    ,teq "pathsToGoal"    (pathsToGoal level)
                          [(Block (Pos 4 7) (Pos 4 7), [MDown, MRight, MRight, MRight, MDown, MRight, MRight])
                          ,(Block (Pos 4 7) (Pos 4 7), [MRight, MDown, MDown, MRight, MRight, MDown, MRight])
                          ,(Block (Pos 4 7) (Pos 4 7), [MRight, MDown, MRight, MRight, MDown, MDown, MRight])
                          ]

    ,teq "optimal solution for level 1"        (solve level (solution level))     (Block (goal level) (goal level))
    ,teq "optimal solution length for level 1" (Prelude.length (solution level))  (Prelude.length optsolution)
    ]

{- TODO - plugin 'terrain' function to enable infinite example:

trait InfiniteTerrain extends GameDef {
  val terrain: Terrain = (pos: Pos) => true
}

object InfiniteLevel extends Solver with InfiniteTerrain {
  val startPos = Pos(1,3)
  val goal = Pos(5,8)
}
println(InfiniteLevel.solution)

-}

main = runTestTT tests

-- End of file.
