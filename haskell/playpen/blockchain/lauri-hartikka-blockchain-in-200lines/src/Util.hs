module Util where

type Host = String
type Port = Int

-- assumes list is [ host, port, ... ]
-- assumes even check done in advance
mkHostPortPairs :: [String] -> [(String,Int)]
mkHostPortPairs      []  = []
mkHostPortPairs (h:p:xs) = (h, read p::Int) : mkHostPortPairs xs

mkInitiatorList      []  = []
mkInitiatorList   (x:xs) = (x, xs) : mkInitiatorList xs
