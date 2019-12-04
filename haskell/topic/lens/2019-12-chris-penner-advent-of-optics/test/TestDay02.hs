{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module TestDay02 where

import           Control.Lens
import           Control.Lens.Regex.Text
import           Control.Monad.Loops     (untilM)
import qualified Data.Map                as Map
import           Data.Map.Lens           (toMapOf)
import qualified Data.Text.IO            as TIO
import           Data.Text.Lens          (unpacked)
import           Protolude               hiding (to)
import           Test.HUnit              ((@?=))
import           Test.Tasty              hiding (Timeout)
import           Test.Tasty.HUnit        (testCase)

-- https://chrispenner.ca/posts/advent-of-optics-02
-- https://adventofcode.com/2019/day/2

{-
computer with
- integer registers
- instruction counter
- operations : 1/addition, 2/multiplication
- operations
  - consume two integers following the instruction as addresses of args
  - 3rd integer representing address to store output
- increment instruction counter to next instruction and continue.
- program halts when 99 is in operation address
-}

readInputFile :: IO Text
readInputFile  = TIO.readFile "test/02.txt-sarahfossheim"

-- read input file as Text
-- there are commas in between each int
-- overkill : use regex to skip commas
solve1_0 :: IO [Text]
solve1_0  =  toListOf ([regex|\d+|] . match)
         <$> readInputFile

test_solve1_0 :: TestTree
test_solve1_0  = testCase "test_day02_solve1_0" $
  solve1_0 >>= \r -> take (length e) r @?= e
 where e = ["1","0","0","3","1","1","2","3"]

-- convert [Text] to [Int]
-- unpacked : convert from Text to String
-- _Show    : convert String to Int
solve1_1 :: IO [Int]
solve1_1  =  readInputFile
         <&> toListOf ([regex|\d+|] . match . unpacked . _Show @Int)

test_solve1_1 :: TestTree
test_solve1_1  = testCase "test_day02_solve1_1" $
  solve1_1 >>= \r -> take (length e) r @?= e
 where e = [1,0,0,3,1,1,2,3]

-- need random access to register values
-- switch from toListOf to toMapOf
-- toMapOf uses index of optic as key by default
-- indexing : wrap optic in 'indexing' (adds an increasing integer as an index to an optic)
--    to get a sequential Int count as the keys for the map
-- maps an increasing number to the next element in the list
solve1_2 :: IO (Map Int Int)
solve1_2  =  readInputFile
         <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))

test_solve1_2 :: TestTree
test_solve1_2  = testCase "test_day02_solve1_2" $
  solve1_2 >>= \r -> take (length e) (Map.toList r) @?= e
 where e = [(0,1),(1,0),(2,0),(3,3),(4,1),(5,1),(6,2),(7,3),(8,1),(9,3),(10,4),(11,3),(12,1)]

-- refactor
getRegisters :: IO (Map Int Int)
getRegisters =  readInputFile
            <&> toMapOf (indexing ([regex|\d+|] . match . unpacked . _Show @Int))

-- initialize 1st position in memory to 12 and 2nd position to 2

solve1_3 :: IO (Map Int Int)
solve1_3  =  getRegisters
         <&> ix 1 .~ 12
         <&> ix 2 .~  2

test_solve1_3 :: TestTree
test_solve1_3  = testCase "test_day02_solve1_3" $
  solve1_3 >>= \r -> take (length e) (Map.toList r) @?= e
 where e = [(0,1),(1,12),(2,2),(3,3),(4,1),(5,1),(6,2),(7,3),(8,1),(9,3),(10,4),(11,3),(12,1)]
 --                   ^     ^

-- now run one step of the program

-- refactor:

-- loadRegister
-- - takes a register 'address' and gets the value stored there
-- - 'use' is like get from MonadState
--     but enables getting a specific piece of state as focused by a lens
-- - 'ix' to get value at specific key from map
-- - 'ix r' is a traversal, not a lens
-- - either switch to 'preuse' that returns a Maybe-wrapped result, or
-- - use 'singular' to force the result (CRASHES if missing)
-- - since input was validated, use 'singular'
loadRegister
  :: (MonadState s m, Field2 s s b b, Ixed b)
  => Index b
  -> m (IxValue b)
loadRegister r = use (_2 . singular (ix r))

-- fst : current read-address
-- snd : register values
-- uses lensy MonadState helpers
oneStep :: State (Int, Map.Map Int Int) ()
oneStep  = do
  let -- loadNext
      -- - gets read-location from fst slot, then loads value at that register
      -- - load read-location via _1 <<+= 1;
      -- - performs += 1 action to the location, ('consumed' the current instruction)
      -- - the leading << says to return value there before altering it
      -- - get/increment read-location in one step
      -- - load value in current location using loadRegister
      loadNext       = _1 <<+= 1 >>= loadRegister

      -- getArg
      -- - gets value at current read-location, loads register at that address
      getArg         = loadNext >>= loadRegister

  -- loadNext to get opcode; converting to function via getOp, get args are run function
  out               <- getOp <$> loadNext <*> getArg <*> getArg
  -- load the output register (the next value at read-location)
  outputReg         <- loadNext
  -- put it in the right spot
  _2 . ix outputReg .= out

getOp :: Int -> (Int -> Int -> Int)
getOp 1 = (+)
getOp 2 = (*)
getOp n = panic $ "unknown op-code: " <> show n

-- refactor
getRegistersAndInit :: IO (Int, Map Int Int)
getRegistersAndInit  = solve1_3 >>= \theMap -> pure $ theMap & (,) 0

-- run oneStep
-- (&~) :: s -> State s a -> s
solve1_4 :: IO (Int, Map Int Int)
solve1_4  = getRegistersAndInit >>= \x -> pure $
  x &~ oneStep

test_solve1_4 :: TestTree
test_solve1_4  = testCase "test_day02_solve1_4" $
  solve1_3 >>= \s3 -> solve1_4 >>= \r -> r @?= (4, s3)

-- run program until completion using untilM
solve1_5 :: IO (Int, Map Int Int)
solve1_5  = getRegistersAndInit >>= \x -> pure $
  x &~ untilM oneStep ((==99) <$> (use _1 >>= loadRegister))

test_solve1_5 :: TestTree
test_solve1_5  = testCase "test_day02_solve1_5" $
  solve1_5 >>= \(i,m) -> (i,Map.lookup 0 m) @?= (132, Just 3267740)

-- explicit recursion instead of untilM
solve1_6 :: IO (Int, Map Int Int)
solve1_6  = getRegistersAndInit >>= \x -> pure $
  x &~ let loop = do
             oneStep
             use _1 >>= loadRegister >>= \case 99 -> return (); _ -> loop
        in loop

test_solve1_6 :: TestTree
test_solve1_6  = testCase "test_day02_solve1_6" $
  solve1_6 >>= \(i,m) -> (i,Map.lookup 0 m) @?= (132, Just 3267740)

-- fix :: (a -> a) -> a (instead of explicit recursion)

solve1_7 :: IO (Int, Map Int Int)
solve1_7  = getRegistersAndInit >>= \x -> pure $
  x &~ fix (\continue -> do
               oneStep
               use _1 >>= loadRegister >>= \case 99 -> return (); _ -> continue)

test_solve1_7 :: TestTree
test_solve1_7  = testCase "test_day02_solve1_7" $
  solve1_7 >>= \(i,m) -> (i,Map.lookup 0 m) @?= (132, Just 3267740)

-- view the first instruction location of registers to get the answer
solve1 :: IO Int
solve1  = solve1_7 >>= \x -> pure $
  x & view (_2 . singular (ix 0))

test_solve1 :: TestTree
test_solve1  = testCase "test_day02_solve1" $
  solve1 >>= \i -> i @?= 3267740

------------------------------------------------------------------------------
-- Part 2 : find the values to put into slots 1 and 2 which result in 19690720

-- parameterize above
solveSingle :: Map.Map Int Int -> Int -> Int -> Int
solveSingle registers noun verb =
  registers
    &  ix 1 .~ noun
    &  ix 2 .~ verb
    &  (,) 0
    &~ fix (\continue -> do
               oneStep
               use _1 >>= loadRegister >>= \case 99 -> return (); _ -> continue)
    &  view (_2 . singular (ix 0))

-- brute force run above with different 'noun' and 'verb' numbers until gets "right" answer
-- only 10,000 combinations
-- collect all possibilities using list comprehension
inputCombos :: [(Int,Int)]
inputCombos  = [(noun, verb) | noun <- [0..99], verb <- [0..99]]

solve2_all :: IO [Int]
solve2_all  = do
  registers <- getRegisters
  pure $   inputCombos
       ^.. traversed . to (uncurry (solveSingle registers))

test_solve2_all :: TestTree
test_solve2_all  = testCase "test_day02_solve2_all" $
  solve2_all >>= \r -> take (length e) r @?= e
 where e = [281754,281755,281756,281757,281758,281759,281760]

-- find a specific combination

solve2_variations :: IO (Maybe Int, Maybe (), Maybe Int)
solve2_variations  = do
  registers <- getRegisters
  pure
    ( inputCombos ^? traversed . to (uncurry (solveSingle registers)) . filtered (== 19690720)
      -- Returns (Just ()) if present, otherwise Nothing
    , inputCombos ^? traversed . to (uncurry (solveSingle registers)) . only 19690720
    , findOf (traversed . to (uncurry (solveSingle registers)))
             (== 19690720)
             inputCombos
    )

test_solve2_variations :: TestTree
test_solve2_variations  = testCase "test_day02_solve2_variations" $
  solve2_variations >>= \r -> r @?= (Just 19690720, Just (), Just 19690720)

-- but do not care about the answer -- want to know input that gave the answer
-- selfIndex   : stash inputs into an index, then carry them alongside the rest of computation
-- findIndexOf : find index of the first value which matches a predicate
solve2_index :: IO (Maybe (Int, Int))
solve2_index  = do
  registers <- getRegisters
  pure $ findIndexOf (traversed . selfIndex . to (uncurry (solveSingle registers)))
                     (== 19690720)
                     inputCombos

test_solve2_index :: TestTree
test_solve2_index  = testCase "test_day02_solve2_index" $
  solve2_index >>= \r -> r @?= Just (78,70)

{-
problem statement says to compute (100 * noun) + verb to get final result

could do it after running findIndexOf

alternative : do it inline, stashing answer in the index

reindexed : run a transformation over the index of an optic,
so alter selfIndex (which stashes the value into the index)
then we can map the index through the transformation
-}
solvePart2 :: IO (Maybe Int)
solvePart2  = do
  registers <- getRegisters
  pure $ findIndexOf ( traversed
                     . reindexed (\(noun, verb) -> (100 * noun) + verb) selfIndex
                     . to (uncurry (solveSingle registers)) )
                     (== 19690720)
                     inputCombos

test_solvePart2 :: TestTree
test_solvePart2  = testCase "test_day02_solvePart2" $
  solvePart2 >>= \r -> r @?= Just 7870

{-
summary
- regex       : extracting text
- toMapOf     : building maps from an indexed fold
- &~          : run state monads as part of a pipeline
- <&>         : pipeline data within a context
- <<+=        : simultaneous modification AND access using lenses in MonadState
- fix         : anonymous recursion
- selfIndex   : stash values till later
- reindexed   : edit indices
- findIndexOf : get index of a value matching a predicate
-}
