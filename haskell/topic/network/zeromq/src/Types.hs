{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Data.Serialize.Text                      ()
import           GHC.Generics                             (Generic)
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
------------------------------------------------------------------------------

data OutBoundMsg addr = OutBoundMsg
  { obmTo   :: ![addr]
  , obmBody :: !ByteString
  } deriving (Eq, Generic)

data TransportEnv addr = TransportEnv
  { teMyAddr       :: !addr                    -- listen address
  , teAddrList     :: ![addr]                  -- peers known at initialization
  , teUseNoBlock   :: Bool                     -- use noblock or blocking channels for inbound messages (not both)
  , teInboxWriteNB :: !(UNB.InChan ByteString) -- noblocking
  , teInboxWrite   :: !(U.InChan   ByteString) -- blocking
  , teUseOBChan    :: Bool                     -- use a channel to give outbound messages to ZMQ
  , teOutboxRead   :: !(U.OutChan  (OutBoundMsg addr)) -- outbound channel
  , teLogErr       :: !([Text] -> IO ())
  , teLogInfo      :: !([Text] -> IO ())
  }

