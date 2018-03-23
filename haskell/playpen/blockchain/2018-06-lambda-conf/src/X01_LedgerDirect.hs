{-# LANGUAGE NoImplicitPrelude #-}

module X01_LedgerDirect where

import           RIO
import qualified System.Log.Logger as Log
------------------------------------------------------------------------------
import           Config
import           Ledger
import           Logging
import           X00_Base

runLedgerDirect :: IO ()
runLedgerDirect = do
  l <- createLedgerLocked (return Nothing) id
  let e = defaultConfig
      txHandler tx = do
        lCommit l e tx
        Log.infoM lDIRECT ("COMMITTED: " <> show tx)
  runServerAndClients e l txHandler
