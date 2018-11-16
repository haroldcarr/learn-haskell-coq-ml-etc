{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import qualified Control.Monad.RWS.Strict as RWS
------------------------------------------------------------------------------
import qualified LogData                  as L
import qualified RetrofitDebug            as L
------------------------------------------------------------------------------
import           Raft                     hiding (server)
import           Types

main :: IO ()
main =
  let (s, e) = mkSpecStateEnv
   in RWS.void $ RWS.evalRWST server e s

server :: Raft IO a ()
server = do
  r <- RWS.ask
  liftIO (putStrLn "")
  L.debugInfo L.exampleLogList'
  liftIO (putStrLn "")
  L.debugInfo $ L.debugUnexpectedS [L.TXT "internal error, there should be a next log entry"]
  let me = r^.cfg.nodeId
  liftIO (putStrLn "")
  L.debugRaftHandler AE [L.NID me, L.TXT "sandbagging"] -- , show ae
  return ()
