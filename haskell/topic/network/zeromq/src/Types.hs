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
  { teInboxWriteNB :: !(UNB.InChan ByteString)     -- noblocking - just use this, or the next, not both
  , teInboxWrite   :: !(U.InChan   ByteString)     -- blocking
  , teOutboxRead   :: !(U.OutChan  (OutBoundMsg addr))
  , teMyAddr       :: !addr
  , teAddrList     :: ![addr]
  , teLogErr       :: !([Text] -> IO ())
  , teLogInfo      :: !([Text] -> IO ())
  , teUseNoBlock   :: Bool }
