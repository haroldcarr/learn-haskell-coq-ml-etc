{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Init where

------------------------------------------------------------------------------
import           NoBlockChan
import           Types
------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
------------------------------------------------------------------------------

initialize
  :: addr
  -> (addr -> [Text] -> IO ())
  -> (addr -> [Text] -> IO ())
  -> IO ( TransportEnv addr
        , MVar (UNB.Stream ByteString)
        , U.InChan (OutBoundMsg addr) )
initialize me le li = do
  (inboxW , inboxR)  <- newNoBlockChan
  (outboxW, outboxR) <- U.newChan
  pure ( TransportEnv inboxW outboxR me [] (le me) (li me)
       , inboxR, outboxW )

