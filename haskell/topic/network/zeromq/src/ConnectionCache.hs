{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConnectionCache where

------------------------------------------------------------------------------
import           Types
------------------------------------------------------------------------------
import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as Map
import           Data.Serialize.Text        ()
import qualified Data.Set                   as Set
import           Protolude                  hiding (async, newChan, readChan,
                                             to)
------------------------------------------------------------------------------

newtype Connection      c   = Connection      { unConnection      :: c }
newtype ConnectionCache a c = ConnectionCache { unConnectionCache :: Map.Map a c }

-- | Returns Nothing if all addresses in cache.
-- Returns Just set of addresses NOT in cache.
checkExistingConnections
  :: Ord addr
  => ConnectionCache addr conn
  -> Recipients addr
  -> Maybe (Set.Set addr)
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
  RAll        -> pure $! Map.elems m
  RSome addrs -> pure $! (m Map.!) <$> Set.toList addrs
  ROne  addr  -> pure    [m Map.! addr]

