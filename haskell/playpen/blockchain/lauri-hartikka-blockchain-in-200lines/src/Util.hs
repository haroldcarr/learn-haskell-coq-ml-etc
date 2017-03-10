module Util where

import           Data.List (partition)

-- splits the list so
-- fst contains the initiator host/port
-- snd contains target host/ports
mkPartition :: Eq a => [a] -> [([a], [a])]
mkPartition xs =  map (\x -> partition ((==) x) xs) xs

-- assumes list is [ host, port, ... ]
-- assumes even check done in advance
mkHostPortPairs []       = []
mkHostPortPairs (h:p:xs) = (h, read p::Int) : mkHostPortPairs xs

