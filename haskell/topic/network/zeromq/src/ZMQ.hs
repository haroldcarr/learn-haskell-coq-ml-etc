{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZMQ where

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
import qualified Prelude
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
import           System.ZMQ4.Monadic

------------------------------------------------------------------------------

type Address = Prelude.String -- because that is what ZMQ4 uses

runMsgServer
  :: TransportEnv Address
  -> IO ()
runMsgServer te@TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    l logInfo ["starting zmqThread"]
    -------------------------
    l logInfo ["starting zmqReceiver"]
    zmqReceiver <- async $ do
      void (receiver te)
      l logErr ["exiting zmqReceiver"]
    -------------------------
    -- ensure the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    l logInfo ["starting zmqSender"]
    zmqSender <- async $ do
      (_, _, cc) <- mkNewConnections (ConnectionCache Map.empty) addrList
      void (sender te cc)
      l logErr ["exiting zmqSender"]
    -------------------------
    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> logErr ["waitEitherCancel", "zmqReceiver ()"]
      Right v -> logErr ["waitEitherCancel", "zmqSender", show v]
    -------------------------
    l logErr ["exiting zmqThread"]

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> logErr ["ZMQ runMsgServer died Right ()"]
    Left err -> logErr ["ZMQ runMsgServer died Left", show err]

receiver
  :: TransportEnv Address
  -> ZMQ z ()
receiver TransportEnv {..} = do
  sock <- socket Pull
  l logInfo ["bind"]
  _ <- bind sock myAddr
  forever $ do
    !newMsg <- receive sock -- GET MSG FROM ZMQ
    l logInfo ["recv", show newMsg]
    liftIO $ do
      UNB.writeChan inboxWrite newMsg -- GIVE MSG TO SYSTEM
      yield

sender
  :: TransportEnv Address
  -> ConnectionCache Address (Socket z Push)
  -> ZMQ z ()
sender TransportEnv{..} !cc0 = do
  ccMvar <- liftIO (newMVar cc0)
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan outboxRead -- GET MSGS FROM SYSTEM
    l logInfo ["sending to", show addrs, "MSG", show msg]
    !cc            <- liftIO (takeMVar ccMvar)
    (_, !cs, !cc') <- getOrMakeConnection cc addrs
    mapM_ (\(_,!s) -> send s [] msg) cs -- GIVE MSGS TO ZMQ
    liftIO (putMVar ccMvar cc')
    l logInfo ["sent msg"]

l :: ([Text] -> IO    ())
  -> ([Text] -> ZMQ z ())
l f x = liftIO (f x)

------------------------------------------------------------------------------

-- returns ([could not connect], [connections], cache)
-- TODO: ZMQ viz could not connect
-- TODO: connections are (address, socket) - address for debugging
getOrMakeConnection
  :: ConnectionCache Address (Socket z Push)
  -> [Address]
  -> ZMQ z ([Address], [(Address, Socket z Push)], ConnectionCache Address (Socket z Push))
getOrMakeConnection !cc !peers  =
  case getConnections cc peers of
    ([],          connections) -> pure ([], connections, cc)
    (needConnect, connections) -> do
      (cannotConnect, newConnections, cc') <- mkNewConnections cc needConnect
      pure (cannotConnect, connections ++ newConnections, cc')

mkNewConnections
  :: ConnectionCache Address (Socket z Push)
  -> [Address]
  -> ZMQ z ([Address], [(Address, Socket z Push)], ConnectionCache Address (Socket z Push))
mkNewConnections (ConnectionCache !m0) !addrs = do
  (!absent, !present, !m) <- foldM go ([], [], m0) addrs
  pure $! (absent, present, ConnectionCache m)
 where
  go (!ab, !p, !m) !address = do
    !s <- socket Push
    void (connect s address)
    pure $! (ab, (address, s):p, Map.insert address s m)

