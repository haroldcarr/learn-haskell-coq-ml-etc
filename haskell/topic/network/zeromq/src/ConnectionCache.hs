{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConnectionCache where

------------------------------------------------------------------------------
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Serialize.Text        ()
import qualified Data.Set                   as Set
import           GHC.Generics               (Generic)
import           Protolude                  hiding (async, newChan, readChan,
                                             to)
------------------------------------------------------------------------------

newtype Addr            a   = Addr            {unAddr            :: a} deriving (Eq, Ord, Show)
newtype Connection      c   = Connection      {unConnection      :: c}
newtype ConnectionCache a c = ConnectionCache {unConnectionCache :: Map.Map (Addr a)(Connection c)}

-- | who to send a message to
data Recipients a
  = RAll
  | RSome !(Set.Set (Addr a))
  | ROne  !(Addr a)
  deriving (Eq, Generic, Show)

-- | Returns Nothing if all addresses in cache.
-- Returns Just set of addresses NOT in cache.
checkExistingConnections
  :: Ord addr
  => ConnectionCache addr conn
  -> Recipients addr
  -> Maybe (Set.Set (Addr addr))
checkExistingConnections (ConnectionCache !m) = \case
  RAll -> Nothing
  RSome !addrs ->
    if Set.isSubsetOf addrs $! Map.keysSet m
    then Nothing
    else Just $! Set.difference addrs (Map.keysSet m)
  ROne !addr ->
    if Set.member addr $! Map.keysSet m
    then Nothing
    else Just $! Set.singleton addr

-- | Assumes that connections exist for all recipients.
getConnections
  :: (MonadIO m, Ord addr)
  => ConnectionCache addr conn
  -> Recipients addr
  -> m [conn]
getConnections (ConnectionCache m) = \case
  RAll        -> pure $! unConnection <$> Map.elems m
  RSome addrs -> pure $! unConnection . (m Map.!) <$> Set.toList addrs
  ROne  addr  -> pure $! unConnection <$> [m Map.! addr]

