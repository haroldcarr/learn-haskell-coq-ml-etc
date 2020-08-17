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
  :: addr -- listen address
  -> (addr -> [Text] -> IO ()) -- logErr
  -> (addr -> [Text] -> IO ()) -- logInfo
  -> Bool
  -> IO ( TransportEnv addr
        , MVar (UNB.Stream ByteString)
        , U.OutChan ByteString
        , U.InChan (OutBoundMsg addr) )
initialize me le li b = do
  (inboxWNB, inboxRNB) <- newNoBlockChan
  (inboxW  , inboxR)   <- U.newChan
  (outboxW , outboxR)  <- U.newChan
  pure ( TransportEnv inboxWNB inboxW outboxR me [] (le me) (li me) b
       , inboxRNB
       , inboxR
       , outboxW )

