{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Data.Serialize.Text                      ()
import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
------------------------------------------------------------------------------

-- | who to send a message to
data Recipients a
  = RAll
  | RSome !(Set.Set a)
  | ROne  !a
  deriving (Eq, Generic, Show)

data OutBoundMsg addr msg = OutBoundMsg
  { obmTo   :: !(Recipients addr)
  , obmBody :: !msg
  } deriving (Eq, Generic)

data TransportEnv rpc addr = TransportEnv
  { inboxWrite :: !(UNB.InChan rpc)
  , outboxRead :: !(U.OutChan  (OutBoundMsg addr ByteString))
  , myAddr     :: !addr
  , addrList   :: ![addr]
  , logErr     :: !([Text] -> IO ())
  , logInfo    :: !([Text] -> IO ()) }
