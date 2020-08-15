{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NoBlockChan where

------------------------------------------------------------------------------
import           Control.Concurrent                       (newMVar,
                                                           putMVar, takeMVar,
                                                           threadDelay
                                                           )
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Control.Monad.State.Strict
import           Data.Serialize.Text                      ()
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
------------------------------------------------------------------------------

newNoBlockChan :: IO (UNB.InChan a, MVar (UNB.Stream a))
newNoBlockChan = do
  (w, r) <- UNB.newChan
  r'     <- fmap head (UNB.streamChan 1 r) >>= \case
    Nothing -> panic "newNoBlockChan"
    Just x  -> newMVar x
  pure (w, r')

getMsgSync :: MVar (UNB.Stream msg) -> IO msg
getMsgSync !m = do
  inboxRead <- takeMVar m
  t         <- UNB.tryReadNext inboxRead
  case t of
    UNB.Pending           -> putMVar m inboxRead  >> getMsgSync m
    UNB.Next v inboxRead' -> putMVar m inboxRead' >> (pure $! v)

tryGetMsgs :: Eq msg => MVar (UNB.Stream msg) -> Int -> IO [msg]
tryGetMsgs !m !i0 = do
  inboxRead <- takeMVar m
  msgs      <- go inboxRead i0
  if msgs /= []
    then pure $! msgs
    else threadDelay 1000 >> pure []
 where
  go !strm !i =
    if i <= 0
    then putMVar m strm >> pure []
    else do
      s <- UNB.tryReadNext strm
      case s of
        UNB.Next a strm' -> a `seq` strm' `seq` fmap (a:) (go strm' (i - 1))
        UNB.Pending      -> putMVar m strm >> pure []

