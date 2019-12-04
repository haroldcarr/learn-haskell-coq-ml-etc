{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module TestDay03 where

import           Control.Category        ((>>>))
import           Control.Lens
import           Control.Lens.Regex.Text
import qualified Data.Map                as Map
import           Data.Map.Lens           (toMapOf)
import qualified Data.Set                as Set
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import           Data.Text.Lens          (unpacked)
import           Linear                  hiding (identity)
import qualified Prelude
import           Protolude               hiding (to)
import           Test.HUnit              ((@?=))
import           Test.Tasty              hiding (Timeout)
import           Test.Tasty.HUnit        (testCase)

-- https://chrispenner.ca/posts/advent-of-optics-03
-- https://adventofcode.com/2019/day/3

readInputFile :: IO Text
readInputFile  = TIO.readFile "test/03.txt-Awjin"

{-
two sets of instructions, each representing paths of wires
find out where in the space they cross
determine the distances of those points from the origin

split instruction sets into one for each wire

<&> : pass contents of a monadic action through a bunch of operations
-}
splitIntoWires0 :: IO [Text] -- will only have two elements
splitIntoWires0  = readInputFile <&> T.lines

{-
since only two elements, use tuple

applying traverseOf to all of its arguments
applying view to each traversal (i.e. ix 0)
that creates a function over the list of wire texts
traverseOf then sequences the function as the effect and returns a new function:
   [Text] -> (Text, Text)
using view on a traversal (good because Text is a Monoid)
BAD: if input does not have at least two lines of input then silently continue without errors
  could use singular or preview to be safer
-}
splitIntoWires :: IO (Text, Text)
splitIntoWires  = splitIntoWires0 <&> traverseOf both view (ix 0, ix 1)

-- regex to find each "instruction", then grab the full match as Text
-- unpack into String use Read to parse Ints
-- _Cons prism splits string into first char (direction) and rest (distance to travel)
parsePaths0 :: IO ([(Int, V2 Int)], [(Int, V2 Int)])
parsePaths0  = splitIntoWires <&> both %~
  toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput)

-- converts the String into an Int with read
-- converts the cardinal direction into vector equivalent of that direction
-- V2 from linear package
parseInput :: (Char, Prelude.String) -> (Int, V2 Int)
parseInput (d, n) = (,) (Prelude.read n) $ case d of
  'U' -> V2 0  (-1)
  'D' -> V2 0    1
  'L' -> V2 (-1) 0
  'R' -> V2 1    0
  _   -> panic "parseInput: why is compiler complaining about non-exhaustive?"
{-
determine where the wires intersect

enumerate every single point that each wire passes through to see which ones they have in common

create n copies of each vector in stream, giving a single instruction for each movement

(toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput . folding (uncurry replicate))
uncurry will make replicate into the function: replicate :: (Int, V2 Int) -> [V2 Int], and folding will run that function, then flatten out the list into the focus of the fold. Ultimately this gives us just a huge list of unit vectors like this:

[V2 0 1, V2 1 0, V2 (-1) 0...]
-}
parsePaths :: IO (Set (V2 Int), Set (V2 Int))
parsePaths  = splitIntoWires <&> both %~
  (   -- make replicate into the function: replicate :: (Int, V2 Int) -> [V2 Int]
      -- folding run that function
      -- then flatten out the list into the focus of the fold
      -- gives a list of unit vectors, e.g., [V2 0 1, V2 1 0, V2 (-1) 0...]
      toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput
                  . folding (uncurry replicate))
      -- keep track of which positions will be walked
      -- need to accumulate our position across the whole list (using a scan)

      -- scan uses the V2 Num instance : adds the x and y components separately
      -- causes moving in the right direction after every step
      -- and keeps track of where we've been along the way
  >>> Prelude.scanl1 (+) -- >>> enables writing code top-to-bottom
  -- make it a set so it can be intersected
  >>> Set.fromList
  )

-- intersect the sets in the tuple
-- folds over each set using intersection
-- note: this can handle many wires (not just two) stored in a tuple or list
intersectPaths :: IO (Set (V2 Int))
intersectPaths  = parsePaths <&> foldl1Of each Set.intersection

-- find which intersection is closest to the origin (0, 0)
-- get the distance by summing the absolute value of the aspects of the Vector
-- (which is acting as a Point)

findClosestToOrigin :: IO (Maybe Int)
findClosestToOrigin  = intersectPaths <&> minimumOf (folded . to (sum . abs))

test_solvePart1 :: TestTree
test_solvePart1  = testCase "test_day03_solvePart1" $
  findClosestToOrigin >>= \r -> r @?= Just 248

{-
------------------------------------------------------------------------------
-- Part 2

find the intersection which is the fewest number of steps along the wire from the origin

sum together the steps along each wire and optimize for the smallest total

cannot use Set : need to know which step we were on when we reached each location
use Map via toMapOf
need the index of each element in the list
corresponds to it's distance from the origin along the wire
(could use zip [0..])
will use reindexex instead
traversed has a numerically increasing index by default
reindexed (+1) makes it start at 1 instead (since the first step counts)
toMapOf uses the index as the key, but this case need the vector as the key
(could use M.fromList)
need to swap index and value within the lens path
get the index using withIndex : adds the index to your value as a tuple, (Int, V2 Int)
swap places using the swapped iso
reflect the V2 Int into the index using ito: ito :: (s -> (i, a)) -> IndexedGetter i s a
Now toMapOf properly builds a Map (V2 Int) Int
-}

solvePart2 :: IO Int
solvePart2  = splitIntoWires
  <&> each %~
      (   toListOf ([regex|\w\d+|] . match . unpacked . _Cons . to parseInput
                      . folding (uncurry replicate))
      >>> Prelude.scanl1 (+)
      >>> toMapOf (reindexed (+1) traversed . withIndex . swapped . ito identity)
      )
      -- add distances when hitting an intersection
      -- resulting Map has sum of the two wires' distances at each intersection
  <&> foldl1Of each (Map.intersectionWith (+))
  <&> minimum -- minimum distance

test_solvePart2 :: TestTree
test_solvePart2  = testCase "test_day03_solvePart2" $
  solvePart2 >>= \r -> r @?= 28580
