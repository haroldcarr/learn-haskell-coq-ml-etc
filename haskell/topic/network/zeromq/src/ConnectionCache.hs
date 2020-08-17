{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConnectionCache where

------------------------------------------------------------------------------
import qualified Data.Map.Strict as Map
import           Protolude       hiding (async, newChan, readChan, to)
------------------------------------------------------------------------------

newtype ConnectionCache a c = ConnectionCache { unConnectionCache :: Map.Map a c }

-- | returns ([not in cache], [in cache])
getConnections
  :: Ord addr
  => ConnectionCache addr conn
  -> [addr]
  -> ([addr], [(addr, conn)])
getConnections (ConnectionCache !m) = foldl' go ([],[])
 where
  go (!nic, !ic) !a = case Map.lookup a m of
    Nothing -> (a:nic,       ic)
    Just c  -> (  nic, (a,c):ic)
