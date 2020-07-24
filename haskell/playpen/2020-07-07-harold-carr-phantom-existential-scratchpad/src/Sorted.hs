{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeOperators         #-}

module Sorted where

import           Data.Coerce (coerce)
import qualified Data.List   as L
import           GDP
import           Protolude   hiding (sort, sortBy)
import           Test.Hspec

newtype SortedBy o a = SortedBy a
instance The (SortedBy o a) a

sortBy :: ((a -> a -> Ordering) ~~ comp)
       -> [a]
       -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

mergeBy :: ((a -> a -> Ordering) ~~ comp)
        -> SortedBy comp [a]
        -> SortedBy comp [a]
        -> SortedBy comp [a]
mergeBy comp0 xs0 ys0 =
  coerce (mergeBy' (the comp0) (the xs0) (the ys0))
 where
  mergeBy' :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
  mergeBy' comp = go
   where
    go []         ys         = ys
    go xs         []         = xs
    go xs@(x:xs') ys@(y:ys') = case comp x y of
      GT -> y : go xs  ys'
      _  -> x : go xs' ys

------------------------------------------------------------------------------

minimum_O1 :: SortedBy comp [a] -> Maybe a
minimum_O1 xs = case the xs of
  []    -> Nothing
  (x:_) -> Just x

sort :: Ord a => [a] -> [a] -> [a]
sort xs ys =
  name (comparing Down) $ \gt ->
    let xs' = sortBy gt xs
        ys' = sortBy gt ys
     in (the $ mergeBy gt xs' ys')

s0t :: Spec
s0t  = it "Sorted0" $
  sort [99, -99, 0::Int] [3, 100, -50]
  `shouldBe` [100,99,3,0,-50,-99]
