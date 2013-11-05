{-
Created       : 2013 Oct 29 (Tue) 18:57:36 by carr.
Last Modified : 2013 Nov 04 (Mon) 21:15:32 by carr.
-}

module X07BloxorzTest where

import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util
import Data.Vector as V
import X07GameDef
import X07Solver
import X07StringParserTerrain

solve :: Game -> [Move] -> Block
solve game = Prelude.foldl step (startBlock game)
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

game = mkGame level

infiniteGame = Game (const True) (Pos 1 3) (Pos 5 8)

optsolution = [MRight, MRight, MDown, MRight, MRight, MRight, MDown]

tests = TestList
    [teq "vector" (vector level) (V.fromList [V.fromList "ooo-------"
                                             ,V.fromList "oSoooo----"
                                             ,V.fromList "ooooooooo-"
                                             ,V.fromList "-ooooooooo"
                                             ,V.fromList "-----ooToo"
                                             ,V.fromList "------ooo-"
                                             ])
    ,teq "terrain function level 1 - 0, 0" (terrain game (Pos 0  0))                  True
    ,teq "terrain function level 1 - 4,11" (terrain game (Pos 4 11))                  False

    ,teq "findChar level 1 - start"        (startPos game)                            (Pos 1 1)

    ,teq "block isStanding 1"              (isStanding $ Block (Pos 1 1) (Pos 1 1))   True
    ,teq "block isStanding 2"              (isStanding $ Block (Pos 1 1) (Pos 1 2))   False

    ,teq "isLegal 1"                       (isLegal game $ Block (Pos 0 0) (Pos 0 0)) True
    ,teq "isLegal 2"                       (isLegal game $ Block (Pos 0 0) (Pos 0 1)) True
    ,teq "isLegal 3"                       (isLegal game $ Block (Pos 0 1) (Pos 0 1)) True
    ,teq "isLegal 4"                       (isLegal game $ Block (Pos 0 1) (Pos 0 2)) True
    ,teq "isLegal 5"                       (isLegal game $ Block (Pos 0 2) (Pos 0 2)) True
    ,teq "isLegal 6"                       (isLegal game $ Block (Pos 0 2) (Pos 0 3)) False
    ,teq "isLegal 7"                       (isLegal game $ Block (Pos 0 2) (Pos 0 2)) True
    ,teq "isLegal 8"                       (isLegal game $ Block (Pos 2 8) (Pos 2 8)) True
    ,teq "isLegal 9"                       (isLegal game $ Block (Pos 2 8) (Pos 2 9)) False

    ,teq "startBlock x b1"                 (x (b1 (startBlock game)))                 1
    ,teq "startBlock x b2"                 (x (b2 (startBlock game)))                 1
    ,teq "startBlock y b1"                 (y (b1 (startBlock game)))                 1
    ,teq "startBlock y bb"                 (y (b2 (startBlock game)))                 1

    ,teq "neighbors"                       (neighbors (startBlock game))              [(Block (Pos   1 (-1)) (Pos 1 0), MLeft)
                                                                                      ,(Block (Pos   1   2)  (Pos 1 3), MRight)
                                                                                      ,(Block (Pos (-1)  1)  (Pos 0 1), MUp)
                                                                                      ,(Block (Pos   2   1)  (Pos 3 1), MDown)
                                                                                      ]

    ,teq "legalNeighbors"                  (legalNeighbors game (startBlock game))    [(Block (Pos   1   2)  (Pos 1 3), MRight)
                                                                                      ,(Block (Pos   2   1)  (Pos 3 1), MDown)
                                                                                      ]

    ,teq "done"                            (done game (Block (Pos 4 7) (Pos 4 7)))    True
    ,teq "not done"                        (done game (Block (Pos 4 7) (Pos 4 8)))    False

    ,teq "neighborsWithHistory1"           (neighborsWithHistory game (Block (Pos 1 1) (Pos 1 1)) [MLeft,MUp])
                                           [(Block (Pos 1 2) (Pos 1 3), [MRight,MLeft,MUp])
                                           ,(Block (Pos 2 1) (Pos 3 1), [MDown,MLeft,MUp])
                                           ]

    ,teq "neighborsWithHistory2"           (neighborsWithHistory game (Block (Pos 1 2) (Pos 1 3)) [MRight])
                                           [(Block (Pos 1 1) (Pos 1 1), [MLeft, MRight])
                                           ,(Block (Pos 1 4) (Pos 1 4), [MRight, MRight])
                                           ,(Block (Pos 2 2) (Pos 2 3), [MDown, MRight])
                                           ]

    ,teq "neighborsWithHistory3"           (neighborsWithHistory game (Block (Pos 2 1) (Pos 3 1)) [MDown])
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

    ,teq "misc"                            (isStanding $ Block (Pos 3 7) (Pos 5 8))   False

    ,teq "from"                            (Prelude.take 6 (from game [(startBlock game, [])] []))
                                           [(Block {b1 = Pos {x = 1, y = 1}, b2 = Pos {x = 1, y = 1}},[])
                                           ,(Block {b1 = Pos {x = 1, y = 2}, b2 = Pos {x = 1, y = 3}},[MRight])
                                           ,(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 3, y = 1}},[MDown])
                                           ,(Block {b1 = Pos {x = 1, y = 4}, b2 = Pos {x = 1, y = 4}},[MRight,MRight])
                                           ,(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 2, y = 3}},[MDown,MRight])
                                           ,(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 3, y = 2}},[MRight,MDown])
                                           ]

    ,teq "pathsFromStart"                  (Prelude.take 6 (pathsFromStart game))
                                           [(Block {b1 = Pos {x = 1, y = 1}, b2 = Pos {x = 1, y = 1}},[])
                                           ,(Block {b1 = Pos {x = 1, y = 2}, b2 = Pos {x = 1, y = 3}},[MRight])
                                           ,(Block {b1 = Pos {x = 2, y = 1}, b2 = Pos {x = 3, y = 1}},[MDown])
                                           ,(Block {b1 = Pos {x = 1, y = 4}, b2 = Pos {x = 1, y = 4}},[MRight,MRight])
                                           ,(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 2, y = 3}},[MDown,MRight])
                                           ,(Block {b1 = Pos {x = 2, y = 2}, b2 = Pos {x = 3, y = 2}},[MRight,MDown])
                                           ]

    ,teq "pathsToGoal"                     (pathsToGoal game)
                                           [(Block (Pos 4 7) (Pos 4 7), [MDown, MRight, MRight, MRight, MDown, MRight, MRight])
                                           ,(Block (Pos 4 7) (Pos 4 7), [MRight, MDown, MDown, MRight, MRight, MDown, MRight])
                                           ,(Block (Pos 4 7) (Pos 4 7), [MRight, MDown, MRight, MRight, MDown, MDown, MRight])
                                           ]

    ,teq "optimal solution"                (solve game         (solution game))           (Block (goal game)         (goal game))
    ,teq "optimal solution length"         (Prelude.length     (solution game))           (Prelude.length optsolution)

    ,teq "InfiniteTerrain"                 (solve infiniteGame (solution infiniteGame))   (Block (goal infiniteGame) (goal infiniteGame))
    ,teq "InfiniteTerrain"                 (Prelude.length     (solution infiniteGame))   (Prelude.length optsolution)
    ]

main = runTestTT tests

-- End of file.
