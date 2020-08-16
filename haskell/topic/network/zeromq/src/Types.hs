{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

------------------------------------------------------------------------------
import           ConnectionCache
import           NoBlockChan
------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Control.Monad.State.Strict
import qualified Data.Map.Strict                          as Map
import           Data.Serialize.Text                      ()
import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
------------------------------------------------------------------------------

data OutBoundMsg addr msg = OutBoundMsg
  { obmTo   :: !(Recipients addr)
  , obmBody :: !msg
  } deriving (Eq, Generic)

data TransportEnv rpc addr = TransportEnv
  { inboxWrite :: !(UNB.InChan rpc)
  , outboxRead :: !(U.OutChan  (OutBoundMsg addr ByteString))
  , myAddr     :: !(Addr addr)
  , addrList   :: ![Addr addr]
  , logErr     :: !([Text] -> IO ())
  , logInfo    :: !([Text] -> IO ()) }

setup
  :: Addr addr
  -> (Addr addr -> [Text] -> IO ())
  -> (Addr addr -> [Text] -> IO ())
  -> IO ( TransportEnv rpc addr
        , MVar (UNB.Stream rpc)
        , U.InChan (OutBoundMsg addr ByteString) )
setup me le li = do
  (inboxW , inboxR)  <- newNoBlockChan
  (outboxW, outboxR) <- U.newChan
  pure ( TransportEnv inboxW outboxR me [] (le me) (li me)
       , inboxR, outboxW )
