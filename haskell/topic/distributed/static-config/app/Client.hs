module Main where

import           Control.Concurrent.MVar           (MVar, readMVar)
import qualified Control.Distributed.Process       as DP
import qualified Control.Distributed.Process.Async as DPA
import           Control.Monad                     (forM_)
import           Control.Monad.IO.Class            (liftIO)
import           System.Environment                (getArgs, getProgName)
------------------------------------------------------------------------------
import           Messages
import           StaticConfig

main :: IO ()
main = do
  prog <- getProgName
  args <- getArgs
  configMain prog args app

app :: MVar Config -> DP.Process ()
app config = do
  peers <- fmap remotePids (liftIO (readMVar config))
  DP.say $ "Peers: " ++ show (length peers) ++ " " ++ show peers
  a1    <- DPA.async (DPA.task (messageHandler config))
  forM_ [ 1 .. 10 ] $ \rn -> do
    let peer = head peers
    DP.say $ "Sending to: " ++ show peer
    sendMessage rn [peer]
  _ <- DPA.wait a1 -- never exits
  return ()
