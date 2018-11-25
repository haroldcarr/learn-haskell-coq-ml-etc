module Messages where

import           Control.Concurrent          (threadDelay)
import           Control.Concurrent.MVar     (MVar, readMVar)
import qualified Control.Distributed.Process as DP
import           Control.Monad               (replicateM, replicateM_)
import           Control.Monad.Catch         (bracket)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Binary                 (Binary (get, put), getWord8,
                                              putWord8)
import           Data.Foldable               (forM_)
import           Data.Typeable               (Typeable)
------------------------------------------------------------------------------
import           StaticConfig

type RequestNum = Int
type From       = DP.ProcessId
type Via        = DP.ProcessId

data PeerMsg
  = RemoteTerminate
  | Message        RequestNum From
  | ForwardMessage RequestNum From Via
  deriving (Typeable, Show)

data MessageResponse = MessageResponse RequestNum [DP.ProcessId]
  deriving (Typeable, Show)

messageHandler
  :: MVar Config
  -> DP.Process ()
messageHandler config = do
  pid <- DP.getSelfPid
  DP.register "messageHandler" pid
  DP.say $ "messageHandler: " ++ show pid
  go
 where
  go = do
    msg <- DP.expect
    case msg of
      RemoteTerminate -> do
        DP.say "messageHandler: RemoteTerminate"
        return () -- no recursive call to 'go', so 'messageHandler' returns
      Message reqNum from -> do -- should only be sent by Client
        pid <- DP.getSelfPid
        DP.say $ "messageHandler: Message:" ++
                 " num: "  ++ show reqNum ++
                 " from: " ++ show from ++
                 " me "    ++ show pid
        responses <- forwardMessage config reqNum from pid
        DP.send from (MessageResponse reqNum (pid:responses))
        go
      ForwardMessage reqNum from via -> do -- should only be sent by Peer
        pid <- DP.getSelfPid
        DP.say $ "messageHandler: ForwardMessage:" ++
                 " num: "  ++ show reqNum ++
                 " from: " ++ show from ++
                 " via "   ++ show via  ++
                 " me "    ++ show pid
        DP.send via (MessageResponse reqNum [pid])
        go

sendMessage
  :: RequestNum
  -> [DP.ProcessId]
  -> DP.Process ()
sendMessage reqNum0 peers = do
  myPid <- DP.getSelfPid
  bracket
   (mapM DP.monitor peers)
   (mapM DP.unmonitor)
   $ \_ -> do
     forM_ peers $ \pid -> DP.send pid (Message reqNum0 myPid)
     replicateM_ (length peers) $
       DP.receiveWait
         [ DP.match $ \(MessageResponse reqNum from) -> do
             DP.say $ "sendMessage: MessageResponse " ++ show reqNum ++ " " ++ show from
             return ()
         , DP.match $ \DP.NodeMonitorNotification {} ->
             return ()
         ]

forwardMessage
  :: MVar Config
  -> RequestNum
  -> DP.ProcessId
  -> DP.ProcessId
  -> DP.Process [DP.ProcessId]
forwardMessage config reqNum0 from via = do
  DP.say "forwardMessage"
  peers <- fmap remotePids (liftIO (readMVar config))
  bracket
   (mapM DP.monitor peers)
   (mapM DP.unmonitor)
   $ \_ -> do
     forM_ peers $ \pid -> DP.send pid (ForwardMessage reqNum0 from via)
     concat <$> replicateM (length peers) (
       DP.receiveWait
         [ DP.match $ \(MessageResponse reqNum replyFrom) -> do
             DP.say $ "forwardMessage: MessageResponse " ++ show reqNum ++ " " ++ show replyFrom
             return replyFrom
         , DP.match $ \DP.NodeMonitorNotification {} ->
             return []
         ])

------------------------------------------------------------------------------

terminateAllPeers
  :: MVar Config
  -> DP.Process ()
terminateAllPeers config = do
  peers <- fmap remotePids (liftIO (readMVar config))
  forM_ peers $ \pid -> DP.send pid RemoteTerminate
  liftIO $ threadDelay 1000000

terminateRemote
  :: DP.NodeId
  -> DP.Process ()
terminateRemote nid = DP.nsendRemote nid "messageHandler" RemoteTerminate

------------------------------------------------------------------------------
instance Binary PeerMsg where
  put RemoteTerminate              = putWord8 0
  put (Message rn from)            = do putWord8 1; put (rn, from)
  put (ForwardMessage rn from via) = do putWord8 2; put (rn, from, via)
  get = do
    header <- getWord8
    case header of
      0 -> return RemoteTerminate
      1 -> uncurry Message <$> get
      2 -> do (rn, from, via) <- get; return (ForwardMessage rn from via)
      _ -> fail "PeerMsg.get: invalid"

instance Binary MessageResponse where
  put (MessageResponse rn from) = put (rn, from)
  get = uncurry MessageResponse <$> get

