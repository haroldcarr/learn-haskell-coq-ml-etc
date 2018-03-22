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
  l <- createLedgerLocked Nothing id
  let e = defaultConfig
      txHandler tx = do
        Log.infoM lDIRECT ("COMMITING: " <> show tx)
        lCommit l e tx
  runServerAndClients e l txHandler
