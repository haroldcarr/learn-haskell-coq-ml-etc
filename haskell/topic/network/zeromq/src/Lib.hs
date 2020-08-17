{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

------------------------------------------------------------------------------
import           ConnectionCache
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
    l logInfo ["launching ZMQ_THREAD"]
    -------------------------
    l logInfo ["launching ZMQ_RECEIVER"]
    zmqReceiver <- async $ do
      void (receiver te)
      l logErr ["exiting ZMQ_RECEIVER"]
    -------------------------
    -- ensure the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    l logInfo ["launching ZMQ_SENDER"]
    zmqSender <- async $ do
      cc <- addNewAddrs (ConnectionCache Map.empty) (Set.fromList addrList)
      void (sender te cc)
      l logErr ["exiting ZMQ_SENDER"]
    -------------------------
    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> logErr [show myAddr, "ZMQ_RECEIVER ()"]
      Right v -> logErr [show myAddr, "ZMQ_SENDER", show v]
    -------------------------
    l logErr ["exiting ZMQ_THREAD"]

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> logErr [show myAddr, "ZMQ_MSG_SERVER died Right ()"]
    Left err -> logErr [show myAddr, "ZMQ_MSG_SERVER died Left", show err]

receiver
  :: (Show rpc, S.Serialize rpc)
  => TransportEnv rpc Address
  -> ZMQ z ()
receiver TransportEnv {..} = do
  sock <- socket Pull
  l logInfo ["bind"]
  _ <- bind sock myAddr
  forever $ do
    newMsg <- receive sock -- GET MSG FROM ZMQ
    l logInfo ["recv", show newMsg]
    case S.decode newMsg of
      Left err -> do
        l logErr ["failed S.decode", show newMsg, show err]
        liftIO yield
      Right r ->
        liftIO $ do
          UNB.writeChan inboxWrite r -- GIVE MSG TO SYSTEM
          logInfo ["S.decode", show r]
          yield

sender
  :: TransportEnv rpc Address
  -> ConnectionCache Address (Socket z Push)
  -> ZMQ z ()
sender TransportEnv{..} !cc0 = do
  ccMvar <- liftIO (newMVar cc0)
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan outboxRead -- GET MSGS FROM SYSTEM
    l logInfo ["sending to", show addrs, "MSG", show msg]
    cc     <- liftIO (takeMVar ccMvar)
    !cc'   <- updateConnectionCache cc addrs
    !socks <- getConnections cc' addrs
    mapM_ (\s -> send s [] msg) socks -- GIVE MSGS TO ZMQ
    liftIO (putMVar ccMvar cc')
    l logInfo ["sent msg"]

l :: ([Text] -> IO    ())
  -> ([Text] -> ZMQ z ())
l f x = liftIO (f x)

------------------------------------------------------------------------------

updateConnectionCache
  :: ConnectionCache Address (Socket z Push)
  -> Recipients Address
  -> ZMQ z (ConnectionCache Address (Socket z Push))
updateConnectionCache !cc !rs =
  maybe (pure cc) (addNewAddrs cc) (checkExistingConnections cc rs)

addNewAddrs
  :: ConnectionCache Address (Socket z Push)
  -> Set.Set Address
  -> ZMQ z (ConnectionCache Address (Socket z Push))
addNewAddrs (ConnectionCache !m0) !addrs = ConnectionCache <$> foldM go m0 addrs
 where
  go !m !addr = do
    !s <- socket Push
    void (connect s addr)
    pure $! Map.insert addr s m
