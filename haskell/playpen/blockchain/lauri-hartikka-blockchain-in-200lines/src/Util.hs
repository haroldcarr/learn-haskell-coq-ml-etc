module Util where

import           Control.Applicative
import           Data.List           (partition)
import           Data.List           (tails)
import           Data.Traversable    (sequenceA)

type Host = String
type Port = Int

-- splits the list so
-- fst contains the initiator host/port
-- snd contains target host/ports
mkPartition :: Eq a => a -> [a] -> ([a], [a])
mkPartition x = partition ((==) x)

-- assumes list is [ host, port, ... ]
-- assumes even check done in advance
mkHostPortPairs :: [String] -> [(String,Int)]
mkHostPortPairs []       = []
mkHostPortPairs (h:p:xs) = (h, read p::Int) : mkHostPortPairs xs

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

windows :: Int -> [a] -> [[a]]
windows n = transpose' . take n . tails

foo = init . foo'
 where
  foo'    []  = []
  foo' (x:xs) = (x, xs) : foo' xs
