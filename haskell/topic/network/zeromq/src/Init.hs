{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}

module Init where

------------------------------------------------------------------------------
import           NoBlockChan
import           Types
------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi as U
import           Protolude                     hiding (async, newChan, readChan,
                                                to)
------------------------------------------------------------------------------

initialize
  :: forall chanType addr
   . Channel chanType
  => Proxy chanType
  -> addr -- listen address
  -> (addr -> [Text] -> IO ()) -- logErr
  -> (addr -> [Text] -> IO ()) -- logInfo
  -> IO ( TransportEnv addr chanType
        , ReadChanType chanType
        , U.InChan (OutBoundMsg addr) )
initialize chanType me le li = do
  (inboxW , inboxR)  <- mkC chanType
  (outboxW, outboxR) <- U.newChan
  pure ( TransportEnv me [] inboxW outboxR (le me) (li me)
       , inboxR
       , outboxW )
