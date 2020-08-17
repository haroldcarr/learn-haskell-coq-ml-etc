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

data OutBoundMsg addr msg = OutBoundMsg
  { obmTo   :: ![addr]
  , obmBody :: !msg
  } deriving (Eq, Generic)

data TransportEnv rpc addr = TransportEnv
  { inboxWrite :: !(UNB.InChan rpc)
  , outboxRead :: !(U.OutChan  (OutBoundMsg addr ByteString))
  , myAddr     :: !addr
  , addrList   :: ![addr]
  , logErr     :: !([Text] -> IO ())
  , logInfo    :: !([Text] -> IO ()) }
