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
  { inboxWrite :: !(UNB.InChan ByteString)
  , outboxRead :: !(U.OutChan  (OutBoundMsg addr))
  , myAddr     :: !addr
  , addrList   :: ![addr]
  , logErr     :: !([Text] -> IO ())
  , logInfo    :: !([Text] -> IO ()) }
