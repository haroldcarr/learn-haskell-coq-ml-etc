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
  -> Bool
  -> IO ( TransportEnv addr
        , MVar (UNB.Stream ByteString)
        , U.OutChan ByteString
        , U.InChan (OutBoundMsg addr) )
initialize me le li unb uobc = do
  (inboxWNB, inboxRNB) <- newNoBlockChan
  (inboxW  , inboxR)   <- U.newChan
  (outboxW , outboxR)  <- U.newChan
  pure ( TransportEnv  me [] unb inboxWNB inboxW uobc outboxR (le me) (li me)
       , inboxRNB
       , inboxR
       , outboxW )

