module Main where

import           Consensus             (runFollower, runLeader)
import           Http                  (site)

import           Control.Concurrent    (MVar, newEmptyMVar)
import           Control.Concurrent    (forkIO)
import           Control.Monad         (forever)
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 as BSC8 (pack)
import           Data.List             (partition)
import           System.Environment    (getArgs)

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
        else doIt (ss (sss xs))

doIt lfs =
  mapM_ (\([leader], followers) -> doIt' leader followers) lfs
 where
  doIt' (_,leader) _ = do
    httpToConsensus <- newEmptyMVar
    forkIO $ forever (runFollower host leader)
    forkIO $ forever (runLeader httpToConsensus host leader)
    site httpToConsensus

ss :: Eq a => [a] -> [([a], [a])]
ss xs =  map (\x -> partition ((==) x) xs) xs

-- assumes even check done in advance
sss []       = []
sss (h:p:xs) = (h, read p::Int) : sss xs

{-
sss ["0.0.0.0", "9061", "0.0.0.0", "9062", "0.0.0.0", "9063"]


CMD --> L --> F1
          --> F2
-}
