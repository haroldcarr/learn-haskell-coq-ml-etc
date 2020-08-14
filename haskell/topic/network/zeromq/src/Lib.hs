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

type Address      = Text
newtype SignedRPC = SignedRPC Text deriving (Eq, Generic, Show)
instance S.Serialize SignedRPC

newtype Addr         a   = Addr         { _unAddr         :: a } deriving (Eq, Ord, Read, Show)
newtype ListenOn     a   = ListenOn     { _unListenOn     :: a }
newtype AddrToListen a s = AddrToListen { _unAddrToListen :: Map.Map (Addr a) (ListenOn s) }

-- | who to send a message to
data Recipients a
  = RAll
  | RSome !(Set.Set (Addr a))
  | ROne  !(Addr a)
  deriving (Eq, Generic, Show)

data OutBoundMsg addr msg = OutBoundMsg
  { _obmTo   :: !(Recipients addr)
  , _obmBody :: !msg
  } deriving (Eq, Generic, Show)

data TransportEnv = TransportEnv
  { inboxWrite :: !(UNB.InChan SignedRPC)
  , outboxRead :: !(U.OutChan  (OutBoundMsg Address ByteString))
  , myAddr     :: !(Addr Address)
  , addrList   :: ![Addr Address]
  }

setup
  :: Addr Address
  -> IO ( TransportEnv
        , MVar (UNB.Stream SignedRPC)
        , U.InChan (OutBoundMsg Address ByteString) )
setup me = do
  (inboxWrite , inboxRead)  <- newNoBlockChan
  (outboxWrite, outboxRead) <- U.newChan
  pure ( TransportEnv inboxWrite outboxRead me []
       , inboxRead, outboxWrite )

runMsgServer :: TransportEnv -> IO ()
runMsgServer te@TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    ztprint [show (_unAddr myAddr), "launching ZMQ_THREAD"]
    -------------------------
    ztprint [show (_unAddr myAddr), "launching ZMQ_RECEIVER"]
    zmqReceiver <- async $ do
      void (recvProcess te)
      ztprint [show (_unAddr myAddr), "exiting ZMQ_RECEIVER"]
    -------------------------
    -- be sure that the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    ztprint [show (_unAddr myAddr), "launching ZMQ_SENDER"]
    zmqSender <- async $ do
      rolodex <- addNewAddrs (AddrToListen Map.empty) addrList
      void (sendProcess myAddr outboxRead rolodex)
      ztprint [show (_unAddr myAddr), "exiting ZMQ_SENDER"]
    -------------------------
    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> tprint [show (_unAddr myAddr), "ZMQ_RECEIVER ()"]
      Right v -> tprint [show (_unAddr myAddr), "ZMQ_SENDER", show v]
    -------------------------
    ztprint ["exiting ZMQ_THREAD"]

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> tprint [show (_unAddr myAddr), "ZMQ_MSG_SERVER died Right ()"]
    Left err -> tprint [show (_unAddr myAddr), "ZMQ_MSG_SERVER died Left", show err]

recvProcess :: TransportEnv -> ZMQ z ()
recvProcess TransportEnv {..} = do
  sock <- socket Pull
  ztprint [show (_unAddr myAddr), "about to bind"]
  _ <- bind sock (T.unpack (_unAddr myAddr))
  forever $ do
    newMsg <- receive sock -- get message from ZMQ4
    ztprint [show (_unAddr myAddr), "recv", show newMsg]
    case S.decode newMsg of
      Left err -> do
        ztprint [show (_unAddr myAddr), "failed S.decode", show newMsg, show err]
        liftIO yield
      Right s@(SignedRPC _) ->
        liftIO $ do
          UNB.writeChan inboxWrite s
          tprint [ show (_unAddr myAddr), "S.decode", show s]
          yield

sendProcess
  :: Addr Address
  -> U.OutChan (OutBoundMsg Address ByteString)
  -> AddrToListen Address (Socket z Push)
  -> ZMQ z ()
sendProcess me outboxRead !r = do
  rMvar <- liftIO (newMVar r)
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan outboxRead
    ztprint [show (_unAddr me), "sending to", show addrs, "MSG", show msg]
    r'      <- liftIO (takeMVar rMvar)
    !newRol <- updateAddrToListen r' addrs
    !toPoll <- recipList newRol addrs
    mapM_ (\s -> send s [] msg) toPoll -- Give messages to ZMQ4.
    liftIO (putMVar rMvar newRol)
    ztprint [show (_unAddr me), "sent msg"]

------------------------------------------------------------------------------

updateAddrToListen
  :: AddrToListen Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z (AddrToListen Address (Socket z Push))
updateAddrToListen r@(AddrToListen !_rol) RAll = pure $! r
updateAddrToListen r@(AddrToListen !rol) (RSome !addrs) =
  if Set.isSubsetOf addrs $! Map.keysSet rol
  then pure $! r
  else do
    !a <- addNewAddrs r $! Set.toList addrs
    pure $! a
updateAddrToListen r@(AddrToListen !rol) (ROne !addr) =
  if Set.member addr $! Map.keysSet rol
  then pure $! r
  else do
    !a <- addNewAddrs r [addr]
    pure $! a

addNewAddrs
  :: AddrToListen Address (Socket z Push)
  -> [Addr Address]
  -> ZMQ z (AddrToListen Address (Socket z Push))
addNewAddrs !r [] = pure r
addNewAddrs (AddrToListen !r) (x:xs) = do
  !r' <- if Map.member x r
         then pure $! AddrToListen r
         else do
           s <- socket Push
           _ <- connect s (T.unpack (_unAddr x))
           pure $! AddrToListen $! Map.insert x (ListenOn s) r
  r' `seq` addNewAddrs r' xs

recipList
  :: AddrToListen Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z [Socket z Push]
recipList (AddrToListen r) RAll          = pure $! _unListenOn <$> Map.elems r
recipList (AddrToListen r) (RSome addrs) = pure $! _unListenOn . (r Map.!) <$> Set.toList addrs
recipList (AddrToListen r) (ROne addr)   = pure $! _unListenOn <$> [r Map.! addr]

------------------------------------------------------------------------------

newNoBlockChan :: IO (UNB.InChan a, MVar (UNB.Stream a))
newNoBlockChan = do
  (w, r) <- UNB.newChan
  r'     <- fmap head (UNB.streamChan 1 r) >>= \case
    Nothing -> panic "newNoBlockChan"
    Just x  -> newMVar x
  pure (w, r')

getMsgSync :: MVar (UNB.Stream SignedRPC) -> IO SignedRPC
getMsgSync m = do
  inboxRead <- takeMVar m
  t         <- UNB.tryReadNext inboxRead
  case t of
    UNB.Pending           -> putMVar m inboxRead  >> getMsgSync m
    UNB.Next v inboxRead' -> putMVar m inboxRead' >> pure v

getBacklog :: MVar (UNB.Stream SignedRPC) -> Int -> IO [SignedRPC]
getBacklog m cnt = do
  inboxRead <- takeMVar m
  let go strm cnt' =
        if cnt' <= 0
        then putMVar m strm >> pure []
        else do
          s <- UNB.tryReadNext strm
          case s of
            UNB.Next a strm' -> fmap (a:) (go strm' (cnt'-1))
            UNB.Pending      -> putMVar m strm >> pure []
  blog <- go inboxRead cnt
  if blog /= []
    then pure blog
    else threadDelay 1000 >> pure []

------------------------------------------------------------------------------

ztprint :: [Text] -> ZMQ z ()
ztprint = liftIO . tprint

tprint :: [Text] -> IO ()
tprint  = print

