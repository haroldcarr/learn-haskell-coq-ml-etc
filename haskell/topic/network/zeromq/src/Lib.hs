{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

------------------------------------------------------------------------------
import           Types
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
import qualified Prelude
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
import           System.ZMQ4.Monadic

------------------------------------------------------------------------------

type Address = Prelude.String -- because that is what ZMQ4 uses

runMsgServer
  :: (Show rpc, S.Serialize rpc)
  => TransportEnv rpc Address
  -> IO ()
runMsgServer te@TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    ztprint myAddr ["launching ZMQ_THREAD"]
    -------------------------
    ztprint myAddr ["launching ZMQ_RECEIVER"]
    zmqReceiver <- async $ do
      void (receiver te)
      ztprint myAddr ["exiting ZMQ_RECEIVER"]
    -------------------------
    -- ensure the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    ztprint myAddr ["launching ZMQ_SENDER"]
    zmqSender <- async $ do
      rolodex <- addNewAddrs (ConnectionCache Map.empty) addrList
      void (sender myAddr outboxRead rolodex)
      ztprint myAddr ["exiting ZMQ_SENDER"]
    -------------------------
    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> tprint [show (unAddr myAddr), "ZMQ_RECEIVER ()"]
      Right v -> tprint [show (unAddr myAddr), "ZMQ_SENDER", show v]
    -------------------------
    ztprint myAddr ["exiting ZMQ_THREAD"]

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> tprint [show (unAddr myAddr), "ZMQ_MSG_SERVER died Right ()"]
    Left err -> tprint [show (unAddr myAddr), "ZMQ_MSG_SERVER died Left", show err]

receiver
  :: (Show rpc, S.Serialize rpc)
  => TransportEnv rpc Address
  -> ZMQ z ()
receiver TransportEnv {..} = do
  sock <- socket Pull
  ztprint myAddr ["bind"]
  _ <- bind sock (unAddr myAddr)
  forever $ do
    newMsg <- receive sock -- GET MESSAGE FROM ZMQ
    ztprint myAddr ["recv", show newMsg]
    case S.decode newMsg of
      Left err -> do
        ztprint myAddr ["failed S.decode", show newMsg, show err]
        liftIO yield
      Right r ->
        liftIO $ do
          UNB.writeChan inboxWrite r
          tprint [unAddr myAddr, "S.decode", show r]
          yield

sender
  :: Addr Address
  -> U.OutChan (OutBoundMsg Address ByteString)
  -> ConnectionCache Address (Socket z Push)
  -> ZMQ z ()
sender me outboxRead !r = do
  rMvar <- liftIO (newMVar r)
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan outboxRead
    ztprint me ["sending to", show addrs, "MSG", show msg]
    r'      <- liftIO (takeMVar rMvar)
    !newRol <- updateConnectionCache r' addrs
    !socks  <- recipList newRol addrs
    mapM_ (\s -> send s [] msg) socks -- GIVE MESSAGES TO ZMQ
    liftIO (putMVar rMvar newRol)
    ztprint me ["sent msg"]

------------------------------------------------------------------------------

updateConnectionCache
  :: ConnectionCache Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z (ConnectionCache Address (Socket z Push))
updateConnectionCache cc@(ConnectionCache !m) = \case
  RAll -> pure $! cc
  RSome !addrs ->
    if Set.isSubsetOf addrs $! Map.keysSet m
    then pure $! cc
    else do
      !cc' <- addNewAddrs cc $! Set.toList addrs
      pure $! cc'
  ROne !addr ->
    if Set.member addr $! Map.keysSet m
    then pure $! cc
    else do
      !cc' <- addNewAddrs cc [addr]
      pure $! cc'

addNewAddrs
  :: ConnectionCache Address (Socket z Push)
  -> [Addr Address]
  -> ZMQ z (ConnectionCache Address (Socket z Push))
addNewAddrs cc@(ConnectionCache !m) = \case
  [] -> pure cc
  (addr:addrs) -> do
    !cc' <- if Map.member addr m
            then pure $! ConnectionCache m
            else do
              s <- socket Push
              _ <- connect s (unAddr addr)
              pure $! ConnectionCache $! Map.insert addr (Connection s) m
    cc' `seq` addNewAddrs cc' addrs

recipList
  :: ConnectionCache Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z [Socket z Push]
recipList (ConnectionCache r) = \case
  RAll        -> pure $! unConnection <$> Map.elems r
  RSome addrs -> pure $! unConnection . (r Map.!) <$> Set.toList addrs
  ROne  addr  -> pure $! unConnection <$> [r Map.! addr]

------------------------------------------------------------------------------

ztprint :: Addr Address -> [Text] -> ZMQ z ()
ztprint _ _ = pure () -- liftIO . tprint

tprint :: [Prelude.String] -> IO ()
tprint _  = pure () -- print

