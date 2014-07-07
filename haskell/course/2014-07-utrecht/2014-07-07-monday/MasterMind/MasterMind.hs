{-
Created       : by Andres Loh
Last Modified : 2014 Jul 07 (Mon) 11:22:27 by Harold Carr.
-}

module Main where

import           System.IO
import           System.Random

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

type Row = [Int]
type Guess = Row
type Solution = Row

colors, width :: Int
colors = 6
width  = 4

-- This is the complete main function. It just initializes the
-- game by generating a solution and then calls the game loop.
main :: IO ()
main =
  do
    s <- generateSolution -- initialization
    loop s                -- game loop

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
loop :: Solution -> IO ()
loop s =
  do
    putStrLn $ show s
    i <- input            -- read (and parse) the user input
    let a@(_,_,g) = check s i
    putStrLn $ report a
    if g
        then putStrLn "YES"
        else loop s

black, white :: Solution -> Guess -> Int
black []           _         = 0
black (s:ss) (g:gs) = (if s == g then 1 else 0) + black ss gs

white solution0 guess0 = white' solution0 guess0
  where
    white' _ [] = 0
    white' (s:ss) (g:gs) = (if g `elem` solution0 && g /= s then 1 else 0) + white' ss gs

check :: Solution -> Guess -> (Int,   -- number of black points,
                               Int,   -- number of white points
                               Bool)  -- all-correct guess?
check solution guess = (black     solution guess,
                        white     solution guess,
                        isCorrect solution guess)
  where
    isCorrect [] [] = True
    isCorrect (s:ss) (g:gs) = if s /= g then False else isCorrect ss gs

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
    , U.teq "08" (check [5,1,1,4] [5,4,1,1]) (2,2,False)
    , U.teq "09" (check [5,1,1,4] [5,1,1,4]) (4,0,True)
    ]

-- report is supposed to take the result of calling check, and
-- produces a descriptive string to report to the player.
report :: (Int, Int, Bool) -> String
--report a@(black0, white0, correct0) = show a
report a = show a

-- The function input is supposed to read a single guess from the
-- player. The first three instructions read a line. You're supposed
-- to (primitively) parse that line into a sequence of integers.
input :: IO Guess
input =
  do
    putStr "? "
    hFlush stdout -- ensure that the prompt is printed
    l <- getLine
    return (map read (words l)) -- TODO : limit length to width

-- The following function |readInt| is given and parses a single
-- integer from a string. It produces |-1| if the parse fails. You
-- can use it in |input|.
readInt :: String -> Int
readInt x =
  case reads x of
    [(n, "")] -> n
    _         -> -1

-- A function that indicates places on which you have to work:
tODO :: a -> a
tODO = id

-- The function valid tests a guess for validity. This is a bonus
-- exercise, so you do not have to implement this function.
valid :: Guess -> Bool
valid guess = tODO True

------------------------------------------------------------------------------
mm :: IO T.Counts
mm = do
    T.runTestTT t0

-- End of file.
