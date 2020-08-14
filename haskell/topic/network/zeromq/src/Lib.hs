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

newtype Addr     a   = Addr     { _unAddr    :: a } deriving (Read,Show,Eq,Ord)
newtype Rolodex  a s = Rolodex  {_unRolodex  :: Map.Map (Addr a) (ListenOn s)}
newtype ListenOn a   = ListenOn {_unListenOn :: a}

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

main :: IO ()
main  = do
  let a = Addr "tcp://127.0.0.1:10000"
      b = Addr "tcp://127.0.0.1:10001"
  (at, ainr, aobw) <- setup a
  (bt, binr, bobw) <- setup b
  runMsgServer at
  runMsgServer bt
  Async.concurrently_
    (sendMsgs b aobw)
    (Async.concurrently_
      (sendMsgs a bobw)
      (Async.concurrently_
        (recvMsgs ainr)
        (recvMsgs binr)))
 where
  sendMsgs to c =
    forM_ ["one", "two", "three"] $ \m ->
      U.writeChan c (OutBoundMsg (ROne to) (S.encode (SignedRPC m)))
  recvMsgs mvc = do
    {-
    m <- getMsgSync mvc
    print (["receive", "decoded", show m]::[Text])
    -}
    ms <- getBacklog mvc 3
    for_ ms $ \m -> print (["receive", "decoded", show m]::[Text])
    threadDelay 10000
    recvMsgs mvc

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

newNoBlockChan :: IO (UNB.InChan a, MVar (UNB.Stream a))
newNoBlockChan = do
  (w, r) <- UNB.newChan
  r'     <- fmap head (UNB.streamChan 1 r) >>= \case
    Nothing -> panic "newNoBlockChan"
    Just x  -> newMVar x
  pure (w, r')

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

runMsgServer
  :: TransportEnv
  -> IO ()
runMsgServer TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    liftIO (print ([show (_unAddr myAddr), "launching ZMQ_THREAD"] :: [Text]))

    zmqReceiver <- async $ do
      liftIO (print ([show (_unAddr myAddr), "launching ZMQ_RECEIVER"] :: [Text]))
      sock <- socket Pull
      liftIO (print ([show (_unAddr myAddr), "about to bind"] :: [Text]))
      _ <- bind sock (T.unpack (_unAddr myAddr))
      forever $ do
        newMsg <- receive sock -- get message from ZMQ4
        liftIO (print ([show (_unAddr myAddr), "recv", show newMsg] :: [Text]))
        case S.decode newMsg of
          Left err -> do
            liftIO (print ([ show (_unAddr myAddr)
                           , "failed S.decode", show newMsg, show err] :: [Text]))
            liftIO yield
          Right s@(SignedRPC _) ->
            liftIO $ do
              UNB.writeChan inboxWrite s
              liftIO (print ([ show (_unAddr myAddr), "S.decode", show s] :: [Text]))
              yield

    liftIO $ threadDelay 100000 -- to be sure that the receive side is up first

    liftIO (print ([show (_unAddr myAddr), "launching ZMQ_SENDER"] :: [Text]))
    zmqSender <- async $ do
      rolodex <- addNewAddrs (Rolodex Map.empty) addrList
      void $ sendProcess myAddr outboxRead rolodex
      liftIO (print ([show (_unAddr myAddr), "exiting ZMQ_SENDER"] :: [Text]))

    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> liftIO (print ([show (_unAddr myAddr), "ZMQ_RECEIVER ()"] :: [Text]))
      Right v -> liftIO (print ([show (_unAddr myAddr), "ZMQ_SENDER", show v] :: [Text]))

    liftIO (print (["exiting ZMQ_THREAD"] :: [Text]))

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> print ([ show (_unAddr myAddr), "ZMQ_MSG_SERVER died Right ()"] :: [Text])
    Left err -> print ([ show (_unAddr myAddr), "ZMQ_MSG_SERVER died Left", show err] :: [Text])

sendProcess
  :: Addr Address
  -> U.OutChan (OutBoundMsg Address ByteString)
  -> Rolodex Address (Socket z Push)
  -> ZMQ z ()
sendProcess me outboxRead !r = do
  rMvar <- liftIO $ newMVar r
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan outboxRead
    liftIO (print ([ show (_unAddr me), "sending to", show addrs, "MSG", show msg] :: [Text]))
    r'      <- liftIO $ takeMVar rMvar
    !newRol <- updateRolodex r' addrs
    !toPoll <- recipList newRol addrs
    mapM_ (\s -> send s [] msg) toPoll -- Give messages to ZMQ4.
    liftIO $ putMVar rMvar newRol
    liftIO (print ([show (_unAddr me), "sent msg"] :: [Text]))

updateRolodex
  :: Rolodex Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z (Rolodex Address (Socket z Push))
updateRolodex r@(Rolodex !_rol) RAll = pure $! r
updateRolodex r@(Rolodex !rol) (RSome !addrs) =
  if Set.isSubsetOf addrs $! Map.keysSet rol
  then pure $! r
  else do
    !a <- addNewAddrs r $! Set.toList addrs
    pure $! a
updateRolodex r@(Rolodex !rol) (ROne !addr) =
  if Set.member addr $! Map.keysSet rol
  then pure $! r
  else do
    !a <- addNewAddrs r [addr]
    pure $! a

addNewAddrs
  :: Rolodex Address (Socket z Push)
  -> [Addr Address]
  -> ZMQ z (Rolodex Address (Socket z Push))
addNewAddrs !r [] = pure r
addNewAddrs (Rolodex !r) (x:xs) = do
  !r' <- if Map.member x r
         then pure $! Rolodex r
         else do
           s <- socket Push
           _ <- connect s (T.unpack (_unAddr x))
           pure $! Rolodex $! Map.insert x (ListenOn s) r
  r' `seq` addNewAddrs r' xs

recipList
  :: Rolodex Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z [Socket z Push]
recipList (Rolodex r) RAll          = pure $! _unListenOn <$> Map.elems r
recipList (Rolodex r) (RSome addrs) = pure $! _unListenOn . (r Map.!) <$> Set.toList addrs
recipList (Rolodex r) (ROne addr)   = pure $! _unListenOn <$> [r Map.! addr]

