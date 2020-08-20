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
import           NoBlockChan
import           Types
------------------------------------------------------------------------------
import           Control.Concurrent                       (forkIO, newMVar,
                                                           putMVar, takeMVar,
                                                           threadDelay, yield,
                                                           yield)
import qualified Control.Concurrent.Async                 as Async
import qualified Control.Concurrent.Chan.Unagi            as U
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
  :: Channel chanType
  => Proxy chanType
  -> TransportEnv Address chanType
  -> IO ()
runMsgServer chanType te@TransportEnv{..} = void $ forkIO $ forever $ do
  zmqThread <- Async.async $ runZMQ $ do
    l teLogInfo ["starting zmqThread"]
    -------------------------
    l teLogInfo ["starting zmqReceiver"]
    zmqReceiver <- async $ do
      void (receiver chanType te)
      l teLogErr ["exiting zmqReceiver"]
    -------------------------
    -- ensure the receive side is up
    liftIO (threadDelay 100000)
    -------------------------
    l teLogInfo ["starting zmqSender"]
    zmqSender <- async $ do
      -- The connection cache MUST be created inside the ZMQ monad and cannot be passed outside.
      (_, _, cc) <- mkNewConnections (ConnectionCache Map.empty) teAddrList
      void (sender te cc)
      l teLogErr ["exiting zmqSender"]
    -------------------------
    liftIO $ Async.waitEitherCancel zmqReceiver zmqSender >>= \case
      Left () -> teLogErr ["waitEitherCancel", "zmqReceiver ()"]
      Right v -> teLogErr ["waitEitherCancel", "zmqSender", show v]
    -------------------------
    l teLogErr ["exiting zmqThread"]

  res <- Async.waitCatch zmqThread
  Async.cancel zmqThread >> case res of
    Right () -> teLogErr ["ZMQ runMsgServer died Right ()"]
    Left err -> teLogErr ["ZMQ runMsgServer died Left", show err]

receiver
  :: Channel chanType
  => Proxy chanType
  -> TransportEnv Address chanType
  -> ZMQ z ()
receiver chanType TransportEnv {..} = do
  sock <- socket Pull
  l teLogInfo ["bind"]
  _ <- bind sock teMyAddr
  forever $ do
    !newMsg <- receive sock -- GET MSG FROM ZMQ
    l teLogInfo ["recv", show newMsg]
    liftIO $ do
      writeC chanType teInboxWrite newMsg -- GIVE MSG TO SYSTEM
      yield

-- The app layer MUST communicate with ZMQ via the channel
-- because the connection cache must live inside the ZMQ monad.
sender
  :: TransportEnv Address chanType
  -> ConnectionCache Address (Socket z Push)
  -> ZMQ z ()
sender TransportEnv{..} !cc0 = do
  ccMvar <- liftIO (newMVar cc0)
  forever $ do
    (OutBoundMsg !addrs !msg) <- liftIO $! U.readChan teOutboxRead -- GET MSGS FROM SYSTEM
    l teLogInfo ["sending to", show addrs, "MSG", show msg]
    !cc             <- liftIO (takeMVar ccMvar)
    (es, !cs, !cc') <- getOrMakeConnection cc addrs
    forM_ es (\e -> l teLogErr ["could not connect to", show e])
    forM_ cs (\(_,!s) -> send s [] msg) -- GIVE MSGS TO ZMQ
    liftIO (putMVar ccMvar cc')
    l teLogInfo ["sent msg"]

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

