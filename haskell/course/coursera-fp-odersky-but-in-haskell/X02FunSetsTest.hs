{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 03 (Thu) 15:48:16 by carr.
-}

module X02FunSetsTest where

import Test.HUnit
import AssertError
import X02FunSets

s1     = singletonSet 1
s2     = singletonSet 2
s3     = singletonSet 3
sl0    = Set (< 0)
sg0    = Set (> 0)
s1to9  = Set (\x -> x > 0 && x < 10)
s6to14 = Set (\x -> x > 5 && x < 15)
{-# ANN isEven "HLint: ignore Redundant lambda" #-}
isEven =      \x -> x `mod` 2 == 0
sEven  = Set isEven

us1s2        = s1  `union` s2
usl0sg0      = sl0 `union` sg0

is1s2        = s1    `intersect` s2
is1to9s6to14 = s1to9 `intersect` s6to14

ds1s2        = diff s1    s2
ds1to9s6to14 = diff s1to9 s6to14

fs1to9sEven  = filter' s1to9 isEven

tests = TestList
    [
    -- UNION
     TestCase $ assertEqual "singleton"   True    (contains s1        1)
    ,TestCase $ assertEqual "union  1"    True    (contains us1s2     1)
    ,TestCase $ assertEqual "union  2"    True    (contains us1s2     2)
    ,TestCase $ assertEqual "union  3"    False   (contains us1s2     3)
    ,TestCase $ assertEqual "union s1 s2" "{1,2}" (show us1s2)
    ,TestCase $ assertEqual "union -6"    True    (contains usl0sg0 (-6))
    ,TestCase $ assertEqual "union  0"    False   (contains usl0sg0   0)
    ,TestCase $ assertEqual "union  9"    True    (contains usl0sg0   9)

    -- INTERSECT
    ,TestCase $ assertEqual "intersect 1"     False    (contains is1s2 1)
    ,TestCase $ assertEqual "intersect 2"     False    (contains is1s2 2)
    ,TestCase $ assertEqual "intersect 3"     False    (contains is1s2 3)
    ,TestCase $ assertEqual "intersect s1 s2" "{}"     (show is1s2)
    ,TestCase $ assertEqual "intersect s1 s1" True     (contains (s1 `intersect` s1) 1)
    ,TestCase $ assertEqual "intersect -10"   False    (contains is1to9s6to14 (-10))
    ,TestCase $ assertEqual "intersect   5"   False    (contains is1to9s6to14    5 )
    ,TestCase $ assertEqual "intersect   9"   True     (contains is1to9s6to14    9 )
    ,TestCase $ assertEqual "intersect  10"   False    (contains is1to9s6to14   10 )
    ,TestCase $ assertEqual "intersect 100"   False    (contains is1to9s6to14  100 )
    ,TestCase $ assertEqual "intersect s1to9 s6to14" "{6,7,8,9}" (show is1to9s6to14)

    -- DIFF
    ,TestCase $ assertEqual "diff 1x" True  (contains ds1s2          1)
    ,TestCase $ assertEqual "diff 2x" False (contains ds1s2          2)
    ,TestCase $ assertEqual "diff 3x" False (contains ds1s2          3)
    ,TestCase $ assertEqual "diff s1 s2" "{1}" (show ds1s2)
    ,TestCase $ assertEqual "diff  4" False (contains (diff s1 s1)   1)
    ,TestCase $ assertEqual "diff  0" False (contains ds1to9s6to14   0)
    ,TestCase $ assertEqual "diff  1" True  (contains ds1to9s6to14   1)
    ,TestCase $ assertEqual "diff  2" True  (contains ds1to9s6to14   2)
    ,TestCase $ assertEqual "diff  3" True  (contains ds1to9s6to14   3)
    ,TestCase $ assertEqual "diff  4" True  (contains ds1to9s6to14   4)
    ,TestCase $ assertEqual "diff  5" True  (contains ds1to9s6to14   5)
    ,TestCase $ assertEqual "diff  6" False (contains ds1to9s6to14   6)
    ,TestCase $ assertEqual "diff  7" False (contains ds1to9s6to14   7)
    ,TestCase $ assertEqual "diff 15" False (contains ds1to9s6to14  15)
    ,TestCase $ assertEqual "diff s1to9 s6to14" "{1,2,3,4,5}" (show ds1to9s6to14)

    -- FILTER

    ,TestCase $ assertEqual "filter  0" False (contains fs1to9sEven  0)
    ,TestCase $ assertEqual "filter  1" False (contains fs1to9sEven  1)
    ,TestCase $ assertEqual "filter  2" True  (contains fs1to9sEven  2)
    ,TestCase $ assertEqual "filter  3" False (contains fs1to9sEven  3)
    ,TestCase $ assertEqual "filter  4" True  (contains fs1to9sEven  4)
    ,TestCase $ assertEqual "filter  5" False (contains fs1to9sEven  5)
    ,TestCase $ assertEqual "filter  6" True  (contains fs1to9sEven  6)
    ,TestCase $ assertEqual "filter  7" False (contains fs1to9sEven  7)
    ,TestCase $ assertEqual "filter  8" True  (contains fs1to9sEven  8)
    ,TestCase $ assertEqual "filter  9" False (contains fs1to9sEven  9)
    ,TestCase $ assertEqual "filter 10" False (contains fs1to9sEven 10)
    ,TestCase $ assertEqual "filter 11" False (contains fs1to9sEven 11)
    ,TestCase $ assertEqual "filter s1to9 sEven" "{2,4,6,8}" (show fs1to9sEven)

    -- FORALL
    ,TestCase $ assertEqual "forall 1"     True  (forall s1                 (== 1) )
    ,TestCase $ assertEqual "forall 2"     False (forall s2                 (== 1) )
    ,TestCase $ assertEqual "forall 3"     False (forall s3                 (== 1) )
    ,TestCase $ assertEqual "forall True"  True  (forall (Set (const True)) (const True))
    ,TestCase $ assertEqual "forall False" False (forall (Set (const True)) (const False))
    ,TestCase $ assertEqual "forall >0"    True  (forall s1to9              (> 0))

    -- EXISTS
    ,TestCase $ assertEqual "exists 1"     True  (exists s1                 (== 1))
    ,TestCase $ assertEqual "exists 2"     False (exists s2                 (== 1))
    ,TestCase $ assertEqual "exists 3"     False (exists s3                 (== 1))
    ,TestCase $ assertEqual "exists True"  True  (exists (Set (const True)) (const True))
    ,TestCase $ assertEqual "exists False" False (exists (Set (const True)) (const False))
    ,TestCase $ assertEqual "exists sEven" True  (exists s1to9              isEven)

    -- MAP
    ,TestCase $ assertEqual "map +1" "{2}"                               (show (map' s1     (+ 1)))
    ,TestCase $ assertEqual "map +2" "{8,9,10,11,12,13,14,15,16}"        (show (map' s6to14 (+ 2)))
    ,TestCase $ assertEqual "map *x" "{36,49,64,81,100,121,144,169,196}" (show (map' s6to14 (\x->x*x)))

    ]

main = runTestTT tests

-- End of file.
