{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

------------------------------------------------------------------------------
import           Control.Concurrent                       (forkIO, newMVar,
                                                           putMVar, takeMVar,
                                                           threadDelay, yield,
                                                           yield)
import qualified Control.Concurrent.Async                 as Async
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Control.Monad.State.Strict
import qualified Data.Map.Strict                          as Map
import qualified Data.Serialize                           as S
import           Data.Serialize.Text                      ()
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           GHC.Generics                             (Generic)
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
import           System.ZMQ4.Monadic

------------------------------------------------------------------------------

type Address = Text

newtype Addr           a   = Addr           { unAddr           :: a } deriving (Eq, Ord, Show)
newtype PushSock       a   = PushSock       { unPushSock       :: a }
newtype AddrToPushSock a s = AddrToPushSock { unAddrToPushSock :: Map.Map (Addr a) (PushSock s) }

-- | who to send a message to
data Recipients a
  = RAll
  | RSome !(Set.Set (Addr a))
  | ROne  !(Addr a)
  deriving (Eq, Generic, Show)

data OutBoundMsg addr msg = OutBoundMsg
  { obmTo   :: !(Recipients addr)
  , obmBody :: !msg
  } deriving (Eq, Generic)

data TransportEnv rpc = TransportEnv
  { inboxWrite :: !(UNB.InChan rpc)
  , outboxRead :: !(U.OutChan  (OutBoundMsg Address ByteString))
  , myAddr     :: !(Addr Address)
  , addrList   :: ![Addr Address] }

setup
  :: Addr Address
  -> IO ( TransportEnv rpc
        , MVar (UNB.Stream rpc)
        , U.InChan (OutBoundMsg Address ByteString) )
setup me = do
  (inboxWrite , inboxRead)  <- newNoBlockChan
  (outboxWrite, outboxRead) <- U.newChan
  pure ( TransportEnv inboxWrite outboxRead me []
       , inboxRead, outboxWrite )

runMsgServer :: (Show rpc, S.Serialize rpc) => TransportEnv rpc -> IO ()
runMsgServer te@TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    ztprint [show (unAddr myAddr), "launching ZMQ_THREAD"]
    -------------------------
    ztprint [show (unAddr myAddr), "launching ZMQ_RECEIVER"]
    zmqReceiver <- async $ do
      void (receiver te)
      ztprint [show (unAddr myAddr), "exiting ZMQ_RECEIVER"]
    -------------------------
    -- ensure the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    ztprint [show (unAddr myAddr), "launching ZMQ_SENDER"]
    zmqSender <- async $ do
      rolodex <- addNewAddrs (AddrToPushSock Map.empty) addrList
      void (sender myAddr outboxRead rolodex)
      ztprint [show (unAddr myAddr), "exiting ZMQ_SENDER"]
    -------------------------
    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> tprint [show (unAddr myAddr), "ZMQ_RECEIVER ()"]
      Right v -> tprint [show (unAddr myAddr), "ZMQ_SENDER", show v]
    -------------------------
    ztprint ["exiting ZMQ_THREAD"]

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> tprint [show (unAddr myAddr), "ZMQ_MSG_SERVER died Right ()"]
    Left err -> tprint [show (unAddr myAddr), "ZMQ_MSG_SERVER died Left", show err]

receiver :: (Show rpc, S.Serialize rpc) => TransportEnv rpc -> ZMQ z ()
receiver TransportEnv {..} = do
  sock <- socket Pull
  ztprint [show (unAddr myAddr), "bind"]
  _ <- bind sock (T.unpack (unAddr myAddr))
  forever $ do
    newMsg <- receive sock -- GET MESSAGE FROM ZMQ
    ztprint [show (unAddr myAddr), "recv", show newMsg]
    case S.decode newMsg of
      Left err -> do
        ztprint [show (unAddr myAddr), "failed S.decode", show newMsg, show err]
        liftIO yield
      Right r ->
        liftIO $ do
          UNB.writeChan inboxWrite r
          tprint [show (unAddr myAddr), "S.decode", show r]
          yield

sender
  :: Addr Address
  -> U.OutChan (OutBoundMsg Address ByteString)
  -> AddrToPushSock Address (Socket z Push)
  -> ZMQ z ()
sender me outboxRead !r = do
  rMvar <- liftIO (newMVar r)
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan outboxRead
    ztprint [show (unAddr me), "sending to", show addrs, "MSG", show msg]
    r'      <- liftIO (takeMVar rMvar)
    !newRol <- updateAddrToPushSock r' addrs
    !socks  <- recipList newRol addrs
    mapM_ (\s -> send s [] msg) socks -- GIVE MESSAGES TO ZMQ
    liftIO (putMVar rMvar newRol)
    ztprint [show (unAddr me), "sent msg"]

------------------------------------------------------------------------------

updateAddrToPushSock
  :: AddrToPushSock Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z (AddrToPushSock Address (Socket z Push))
updateAddrToPushSock r@(AddrToPushSock !_rol) RAll = pure $! r
updateAddrToPushSock r@(AddrToPushSock !rol) (RSome !addrs) =
  if Set.isSubsetOf addrs $! Map.keysSet rol
  then pure $! r
  else do
    !a <- addNewAddrs r $! Set.toList addrs
    pure $! a
updateAddrToPushSock r@(AddrToPushSock !rol) (ROne !addr) =
  if Set.member addr $! Map.keysSet rol
  then pure $! r
  else do
    !a <- addNewAddrs r [addr]
    pure $! a

addNewAddrs
  :: AddrToPushSock Address (Socket z Push)
  -> [Addr Address]
  -> ZMQ z (AddrToPushSock Address (Socket z Push))
addNewAddrs !r [] = pure r
addNewAddrs (AddrToPushSock !r) (x:xs) = do
  !r' <- if Map.member x r
         then pure $! AddrToPushSock r
         else do
           s <- socket Push
           _ <- connect s (T.unpack (unAddr x))
           pure $! AddrToPushSock $! Map.insert x (PushSock s) r
  r' `seq` addNewAddrs r' xs

recipList
  :: AddrToPushSock Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z [Socket z Push]
recipList (AddrToPushSock r) RAll          = pure $! unPushSock <$> Map.elems r
recipList (AddrToPushSock r) (RSome addrs) = pure $! unPushSock . (r Map.!) <$> Set.toList addrs
recipList (AddrToPushSock r) (ROne addr)   = pure $! unPushSock <$> [r Map.! addr]

------------------------------------------------------------------------------

newNoBlockChan :: IO (UNB.InChan a, MVar (UNB.Stream a))
newNoBlockChan = do
  (w, r) <- UNB.newChan
  r'     <- fmap head (UNB.streamChan 1 r) >>= \case
    Nothing -> panic "newNoBlockChan"
    Just x  -> newMVar x
  pure (w, r')

getMsgSync :: MVar (UNB.Stream msg) -> IO msg
getMsgSync m = do
  inboxRead <- takeMVar m
  t         <- UNB.tryReadNext inboxRead
  case t of
    UNB.Pending           -> putMVar m inboxRead  >> getMsgSync m
    UNB.Next v inboxRead' -> putMVar m inboxRead' >> pure v

tryGetMsgs :: Eq msg => MVar (UNB.Stream msg) -> Int -> IO [msg]
tryGetMsgs m i0 = do
  inboxRead <- takeMVar m
  msgs <- go inboxRead i0
  if msgs /= []
    then pure msgs
    else threadDelay 1000 >> pure []
 where
  go strm i =
    if i <= 0
    then putMVar m strm >> pure []
    else do
      s <- UNB.tryReadNext strm
      case s of
        UNB.Next a strm' -> fmap (a:) (go strm' (i - 1))
        UNB.Pending      -> putMVar m strm >> pure []

------------------------------------------------------------------------------

ztprint :: [Text] -> ZMQ z ()
ztprint _ = pure () -- liftIO . tprint

tprint :: [Text] -> IO ()
tprint _  = pure () -- print

