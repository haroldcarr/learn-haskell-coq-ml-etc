module Main where

import           Control.Concurrent.MVar          (MVar, readMVar)
import qualified Control.Distributed.Process      as DP
import           Control.Monad.IO.Class           (liftIO)
import           System.Environment               (getArgs, getProgName)
------------------------------------------------------------------------------
import           StaticConfig
import           Messages

usage :: String -> IO ()
usage prog = putStrLn $ "usage: " ++ prog ++ " (master | remote) host port"

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  configMain prog args app

app :: MVar Config -> DP.Process ()
app config = do
  peers <- fmap remotePids (liftIO (readMVar config))
  DP.say $ "Peers: " ++ show (length peers) ++ " " ++ show peers
  messageHandler config
