module Logging where

import           Control.Monad     (forM_)
import qualified System.Log.Logger as Log

lBASE, lDIRECT, lLEDGER, lMINER :: String
lBASE   = "BASE"
lDIRECT = "DIRECT"
lLEDGER = "LEDGER"
lMINER  = "MINER"

setLogLevels :: IO ()
setLogLevels =
  forM_ [lBASE, lDIRECT, lLEDGER, lMINER] $ \x ->
    Log.updateGlobalLogger x (Log.setLevel Log.DEBUG)

