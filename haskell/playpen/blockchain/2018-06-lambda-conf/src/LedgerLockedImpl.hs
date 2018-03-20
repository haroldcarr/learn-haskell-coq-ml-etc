{-# OPTIONS_GHC -Wno-unused-do-bind    #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module LedgerLockedImpl where

import qualified Control.Concurrent      as CC
import qualified Control.Concurrent.MVar as MV
import qualified Control.Monad           as CM
import qualified Data.Sequence           as Seq
import           RIO
import qualified System.Random           as Random
------------------------------------------------------------------------------
import           Config
import           Ledger


createLedger
  :: IO (Ledger a env)
createLedger = do
  mv <- MV.newMVar Seq.empty
  return Ledger
    { lContents = MV.readMVar mv
    , lCommit = \e a -> do
        s <- MV.takeMVar mv
        CM.when (cDOSEnabled (getConfig e)) $ do
          d <- Random.randomRIO (1,10::Int)
          CM.when (d == 10) $ do
            runRIO e $ logInfo "BEGIN commitToLedger DOS"
            CC.threadDelay (1000000 * cDOSDelay (getConfig e))
            runRIO e $ logInfo "END commitToLedger DOS"
        MV.putMVar mv (s Seq.|> a)
    }
