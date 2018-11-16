{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
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
  L.debugInfo L.exampleLogList
  L.debugInfo $ L.debugUnexpectedS [L.TXT "internal error, there should be a next log entry"]
  let me = r^.cfg.nodeId
  L.debugRaftHandler AE [L.NID me, L.TXT "sandbagging"] -- , show ae
  return ()
