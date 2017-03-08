module Main where

import           Consensus             (runClient, runServer)
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

main :: IO ()
main = do
  xs <- getArgs
  case xs of
    [] -> doIt [([port], [port])]
    xs -> doIt (ss ((map read xs) :: [Int]))

doIt lfs =
  mapM_ (\([leader], followers) -> doIt' leader followers) lfs
 where
  doIt' leader peers = do
    httpToConsensus <- newEmptyMVar
    forkIO $ forever (runServer host leader)
    forkIO $ forever (runClient httpToConsensus host leader)
    site httpToConsensus

ss :: Eq a => [a] -> [([a], [a])]
ss xs =  map (\x -> partition ((==) x) xs) xs

{-
CMD --> L --> F1
          --> F2
-}
