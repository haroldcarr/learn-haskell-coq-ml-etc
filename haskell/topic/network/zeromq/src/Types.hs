{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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

data BlockOrNonBlock = BlockingChannel | NonBlockingChannel

type family WriteChanType a where
  WriteChanType 'BlockingChannel    =   U.InChan ByteString
  WriteChanType 'NonBlockingChannel = UNB.InChan ByteString

type family ReadChanType a where
  ReadChanType 'BlockingChannel     =        U.OutChan ByteString
  ReadChanType 'NonBlockingChannel  = MVar (UNB.Stream ByteString)

type family WriteReadChanType a where
  WriteReadChanType a = (WriteChanType a, ReadChanType a)

data TransportEnv addr bornb = TransportEnv
  { teMyAddr     :: !addr                    -- listen address
  , teAddrList   :: ![addr]                  -- peers known at initialization
  , teInboxWrite :: !(WriteChanType bornb)
  , teOutboxRead :: !(U.OutChan  (OutBoundMsg addr)) -- outbound blocking channel
  , teLogErr     :: !([Text] -> IO ())
  , teLogInfo    :: !([Text] -> IO ())
  }

