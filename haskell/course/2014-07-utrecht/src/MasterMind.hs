{-
Created       : by Andres Loh
Last Modified : 2014 Jul 08 (Tue) 11:45:34 by Harold Carr.
-}

module MasterMind where

import           Data.List       (foldl')
import           Data.Set        as S (empty, insert, size)
import           System.IO
import           System.Random

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

type Row      = [Int]
type Guess    = Row
type Solution = Row

stopMarker :: Row
stopMarker = [-1,-1,-1,-1]

colors, width :: Int
colors = 6
width  = 4

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
mmMain :: IO ()
mmMain =
  do
    s <- generateSolution -- initialization
    loop 1 s              -- game loop

-- The following function is given. It generates a random solution of the
-- given width, and using the given number of colors.
generateSolution :: IO [Int]
generateSolution =
  do
    g <- getStdGen
    let rs = take width (randoms g)
    return (map ((+1) . (`mod` colors)) rs)

-- The loop function is supposed to perform a single interaction. It
-- reads an input, compares it with the solution, produces output to
-- the user, and if the guess of the player was incorrect, loops.
loop :: Int -> Solution -> IO ()
loop l s =
  do
    print s
    i <- input            -- read (and parse) the user input
    if i == stopMarker
        then putStrLn "GOODBYE"
        else do let a@(_,_,g) = check s i
                putStrLn $ report a
                if g
                    then putStrLn ("YES in " ++ show l ++ " tries")
                    else loop (l + 1) s

-- TODO : white is NOT correct
check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess = (length blk, S.size wht, length blk == width)
  where
    (blk,wht) = foldl' go ([], S.empty) (zip solution guess)
    go a@(b, w) (s,g) | s == g                           = (s : b,            w)
                      | g `elem` solution && notElem g b = (    b, S.insert g w)
                      | otherwise                        = a

t0 :: T.Test
t0 = T.TestList
    [
      U.teq "00" (check [3,4,6,6] [1,1,2,2]) (0,0,False)
    , U.teq "01" (check [3,4,6,6] [3,3,4,4]) (1,1,False)
    , U.teq "02" (check [3,4,6,6] [3,5,3,6]) (2,0,False)
    , U.teq "03" (check [3,4,6,6] [3,4,6,6]) (4,0,True)

    , U.teq "04" (check [5,1,1,4] [1,2,3,4]) (1,1,False)
    , U.teq "05" (check [5,1,1,4] [1,3,5,6]) (0,2,False)
    , U.teq "06" (check [5,1,1,4] [5,2,1,5]) (2,0,False)
    , U.teq "07" (check [5,1,1,4] [5,2,4,1]) (1,2,False)
    , U.teq "08" (check [5,1,1,4] [5,4,1,1]) (2,2,False) -- 2,1
    , U.teq "09" (check [5,1,1,4] [5,1,1,4]) (4,0,True)
    ]

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
report = show

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- getLine
    if l == "STOP"
        then return stopMarker
        else let g = map readInt (words l)
             in if valid g
                    then return g
                    else do { putStrLn "invalid input"; input }

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess | (-1) `elem` guess      = False
            | length guess /= width  = False
            | not $ validColor guess = False
            | otherwise              = True
  where
    validColor g = and (foldr (\x acc -> (1 <= x && x <= colors) : acc) [] g)

t1 :: T.Test
t1 = T.TestList
    [
      U.teq "t100" (valid [1,2,3,4])    True
    , U.teq "t101" (valid [1,2,3,-1])   False
    , U.teq "t102" (valid [1,2,3])      False
    , U.teq "t103" (valid [1,2,3,4,5])  False
    , U.teq "t103" (valid [0,2,3,4])    False
    , U.teq "t103" (valid [1,2,3,9])    False
    ]

------------------------------------------------------------------------------

mm :: IO T.Counts
mm = do
    _ <- T.runTestTT t0
    T.runTestTT t1

-- End of file.
