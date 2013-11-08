{-
Created       : 2013 Nov 06 (Wed) 18:33:56 by carr.
Last Modified : 2013 Nov 07 (Thu) 18:22:49 by carr.
-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad (liftM)
import Data.MeldableHeap -- http://hackage.haskell.org/package/meldable-heap-2.0.3/docs/Data-MeldableHeap.html
import Data.List (sort)
import Data.Maybe (fromJust, isNothing)

{-
-- https://gitorious.org/aocapq/aocapq/source/7f24bb1571b3bd89ada668ea81c37ccdeb825498:src/PriorityQueue/QuickCheck.hs
    genericArbitrary :: (PriorityQueue pq, Arbitrary a, Ord a) => Gen (pq a)
    genericArbitrary = fromList `fmap` listOf arbitrary

genericArbitrary :: (PQ pq, Arbitrary a, Ord a) => Gen (pq a)
genericArbitrary = liftM (foldr (\x acc -> insert x acc) empty) (listOf arbitrary)

bug gets:
    `pq' is applied to too many type arguments
    In the type signature for `genericArbitrary':
      genericArbitrary :: (PQ pq, Arbitrary a, Ord a) => Gen (pq a)
-}

-- http://stackoverflow.com/questions/19840751/how-to-write-a-haskell-quickcheck-generator-for-a-data-meldableheap-priority-que
genericArbitrary' :: (Arbitrary a, Ord a) => Gen (PQ a)
genericArbitrary' = liftM fromList (listOf arbitrary)

{-
-- https://gitorious.org/aocapq/aocapq/source/8e27d47d4579516b93bd0532e4b25d1e8ad08df3:src/PriorityQueue/PQueue/QuickCheck.hs
instance (Arbitrary a, Ord a) => Arbitrary (MQ.MinQueue a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
-}

instance (Arbitrary a, Ord a) => Arbitrary (PQ a) where
    arbitrary = genericArbitrary'

instance (Ord a, Show a) => (Show (PQ a)) where
   show pq = "PQ " ++ show (mkList pq)

isEmpty :: Ord a => PQ a -> Bool
isEmpty h = isNothing (findMin h)

deleteMin :: Ord a => PQ a -> PQ a
deleteMin h = case extractMin h of
                  Nothing      -> error "bad news"
                  Just (_, pq) -> pq

prop_empty_meld :: Bool
prop_empty_meld = isEmpty (meld empty empty :: PQ Int)

prop_insert_delete_x_2_empty :: Ord a => a -> Bool
prop_insert_delete_x_2_empty a = isEmpty $ deleteMin (deleteMin h)
    where h = insert a $ insert a empty

prop_min_insert_empty :: Ord a => a -> Bool
prop_min_insert_empty a = findMin h == Just a
    where h = insert a empty

prop_min_meld_insert_empty :: Ord a => a -> a -> a -> Bool
prop_min_meld_insert_empty a b c = findMin (meld h h) == Just (minimum [a, b, c])
    where h = insert a (insert b (insert c empty))

prop_order :: Ord a => a -> a -> Bool
prop_order a b = findMin h == Just min
    where min = minimum [a, b]
          max = maximum [a, b]
          h   = insert max (insert min empty)

prop_transitive :: Ord a => a -> a -> a -> Bool
prop_transitive a b c = small <= medium && medium <= large
    where h      = insert a (insert b (insert c empty))
          small  = findMin h
          medium = findMin (deleteMin h)
          large  = findMin (deleteMin (deleteMin h))

prop_gen1 :: (Num a, Ord a) => PQ a -> Bool
prop_gen1 h = findMin (insert m h) == Just m
    where (Just m) = case findMin h of
                         Nothing -> Just 0
                         Just x  -> Just x

prop_gen1_delete :: (Num a, Ord a) => PQ a -> Property
prop_gen1_delete h = not (isEmpty h) ==> findMin (insert m (deleteMin h)) == Just m
    where m = fromJust $ findMin h

prop_sorted :: Ord a => PQ a -> Bool
prop_sorted h = xs == sort xs
    where xs = mkList h

prop_melded_min :: Ord a => PQ a -> PQ a -> Bool
prop_melded_min h1 h2 = meldMin == h1min || meldMin == h2min
    where h1min   = findMin h1
          h2min   = findMin h2
          meldMin = findMin (meld h1 h2)

prop_length_meld_dups :: Ord a => PQ a -> Bool
prop_length_meld_dups h = 2 * length xs == length xsxs
    where xs    = mkList h
          hDup  = fromList xs
          hMeld = meld h hDup
          xsxs  = mkList hMeld

prop_length_meld :: Ord a => PQ a -> PQ a -> Bool
prop_length_meld h1 h2 = length xs1 + length xs2 == length xsxs
    where xs1  = mkList h1
          xs2  = mkList h2
          xsxs = mkList (meld h1 h2)

prop_thread_id_97 :: Ord a => PQ a -> PQ a -> Property
prop_thread_id_97 h1 h2 = not (isEmpty h1) && not (isEmpty h2) ==> mkList hm  == mkList hmp
    where hm  = meld h1 h2
          m   = fromJust (findMin h1)
          h1p = deleteMin h1
          h2p = insert m h2
          hmp = meld h1p h2p

fromList :: Ord a => [a] -> PQ a
fromList = foldr insert empty

-- PQ toList returns arbitrary order
-- this return sorted : min first
mkList :: Ord a => PQ a -> [a]
mkList h = case extractMin h of
               Nothing      -> []
               Just (a, h') -> a : mkList h'

runTests :: IO Bool
runTests = $quickCheckAll

main = runTests

-- End of file.
