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
  :: Addr addr
  -> (Addr addr -> [Text] -> IO ())
  -> (Addr addr -> [Text] -> IO ())
  -> IO ( TransportEnv rpc addr
        , MVar (UNB.Stream rpc)
        , U.InChan (OutBoundMsg addr ByteString) )
initialize me le li = do
  (inboxW , inboxR)  <- newNoBlockChan
  (outboxW, outboxR) <- U.newChan
  pure ( TransportEnv inboxW outboxR me [] (le me) (li me)
       , inboxR, outboxW )

