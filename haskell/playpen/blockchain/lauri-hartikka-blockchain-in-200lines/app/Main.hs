module Main where

import           Consensus             (consensusFollower, consensusLeader,
                                        runFollower, runLeader)
import           Http                  (site)
import           Util

import           Control.Concurrent    (MVar, newEmptyMVar)
import           Control.Concurrent    (forkIO)
import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.Monoid           ((<>))
import           System.Environment    (getArgs)
import           System.Log.Logger

mainProgram = "main"

host = "0.0.0.0"
port = 9160
{-
 [([9061],[9062,9063]),([9062],[9061,9063]),([9063],[9061,9062])]
 [([("0.0.0.0", 9061)],[("0.0.0.0", 9062),("0.0.0.0", 9063)])
 ,([("0.0.0.0", 9062)],[("0.0.0.0", 9061),("0.0.0.0", 9063)])
 ,([("0.0.0.0", 9063)],[("0.0.0.0", 9061),("0.0.0.0", 9062)])]
-}
main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [] -> doIt [([(host,port)], [(host,port)])]
    xs -> do
      if not (even (length xs))
        then error "Usage"
        else doIt (mkPartition (mkHostPortPairs xs))

doIt :: (Foldable t, Show a) => t ([(a, Int)], [(a, Int)]) -> IO ()
doIt lfs = do
  updateGlobalLogger mainProgram       (setLevel INFO)
  updateGlobalLogger consensusFollower (setLevel INFO)
  updateGlobalLogger consensusLeader   (setLevel INFO)
  mapM_ (\([leader], followers) -> doIt' leader followers) lfs
 where
  doIt' (s,leader) fs = do
    infoM mainProgram ("doIt': " <> show s <> " " <> show leader <> " " <> show fs)
    httpToConsensus <- newEmptyMVar
    forkIO $ forever (runFollower host leader)
    forkIO $ forever (runLeader httpToConsensus host leader)
    site httpToConsensus

{-
CMD <--> L <--> F1
                ^
                |
                v
           <--> F2
-}
