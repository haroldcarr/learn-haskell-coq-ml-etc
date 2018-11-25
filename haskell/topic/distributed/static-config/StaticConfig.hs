{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module StaticConfig where

import           Control.Concurrent               (threadDelay)
import           Control.Concurrent.MVar          (MVar, modifyMVar, newMVar,
                                                   readMVar)
import qualified Control.Distributed.Process      as DP
import qualified Control.Distributed.Process.Node as Node
import           Control.Exception                (throw)
import           Control.Monad                    (replicateM)
import           Control.Monad.Catch              (bracket, finally)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8            as BSC8
import           Data.Foldable                    (forM_)
import           Data.Maybe                       (catMaybes)
import           Data.Monoid                      ((<>))
import qualified Network.Socket                   as N
import qualified Network.Transport                as NT
import qualified Network.Transport.TCP            as NT

data Config = Config
  { transport     :: NT.Transport
  , rtable        :: DP.RemoteTable
  , localNodes    :: [Node.LocalNode]
  , remoteNodeIds :: [DP.NodeId]
  , remotePids    :: [DP.ProcessId]
  }

initialize
  :: N.HostName -> N.ServiceName -> DP.RemoteTable -> [String]
  -> IO (MVar Config)
initialize host port rtable0 nodeAddrs = do
  mTransport <- NT.createTransport host port (host,) NT.defaultTCPParameters
  case mTransport of
    Left err -> throw err
    Right transport0 ->
      let config = Config
            { transport     = transport0
            , rtable        = rtable0
            , localNodes    = []
            , remoteNodeIds = map go nodeAddrs
            , remotePids    = []
            }
      in newMVar config
 where
  go x = DP.NodeId (NT.EndPointAddress ("127.0.0.1:" <> BSC8.pack x <> ":0"))

getRemotePids
  :: MVar Config
  -> DP.Process [DP.ProcessId]
getRemotePids config = do
  nodes <- fmap remoteNodeIds (liftIO (readMVar config))
  bracket
   (mapM DP.monitorNode nodes)
   (mapM DP.unmonitor)
   $ \_ -> do
     forM_ nodes $ \nid -> DP.whereisRemoteAsync nid "WHERE_IS"
     pids <- catMaybes <$> replicateM (length nodes) (
       DP.receiveWait
         [ DP.match (\(DP.WhereIsReply "WHERE_IS" mPid) -> return mPid)
         , DP.match (\DP.NodeMonitorNotification {}     -> return Nothing)
         ])
     liftIO (modifyMVar config $ \c -> return ( c { remotePids = pids }
                                              , pids ))

newLocalNode
  :: MVar Config
  -> IO Node.LocalNode
newLocalNode config =
  modifyMVar config $ \c -> do
    localNode <- Node.newLocalNode (transport c) (rtable c)
    return ( c { localNodes = localNode : localNodes c }
           , localNode)

startProcess
  :: MVar Config
  -> (MVar Config -> DP.Process ())
  -> IO ()
startProcess config proc = do
  node <- newLocalNode config
  Node.runProcess node $ do
    pid <- DP.getSelfPid
    DP.register "WHERE_IS" pid
    liftIO (threadDelay 2000000)
    _ <- getRemotePids config
    proc config `finally` shutdownLogger

-- | Shut down the logger process.
-- Ensures that any pending messages are flushed before the process exits.
-- TODO: monitor the logger process to avoid deadlock if it has already died.
shutdownLogger
  :: DP.Process ()
shutdownLogger = do
  (sport,rport) <- DP.newChan
  DP.nsend "logger" sport
  DP.receiveChan rport

usage :: String -> IO ()
usage prog = putStrLn $ "usage: " ++ prog ++ " (master | remote) host port"

configMain :: String -> [String] -> (MVar Config -> DP.Process ()) -> IO ()
configMain programeName args app =
  case args of
    (host:port:peers) -> do
      config <- initialize host port Node.initRemoteTable peers
      startProcess config app
    _ -> usage programeName

