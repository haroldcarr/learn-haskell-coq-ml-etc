{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 07 (Mon) 19:16:18 by carr.
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
     teq "singleton"   (contains s1        1)  True
    ,teq "union  1"    (contains us1s2     1)  True
    ,teq "union  2"    (contains us1s2     2)  True
    ,teq "union  3"    (contains us1s2     3)  False
    ,teq "union s1 s2" (show us1s2)            "{1,2}"
    ,teq "union -6"    (contains usl0sg0 (-6)) True
    ,teq "union  0"    (contains usl0sg0   0)  False
    ,teq "union  9"    (contains usl0sg0   9)  True

    -- INTERSECT
    ,teq "intersect 1"            (contains is1s2 1)               False
    ,teq "intersect 2"            (contains is1s2 2)               False
    ,teq "intersect 3"            (contains is1s2 3)               False
    ,teq "intersect s1 s2"        (show is1s2)                     "{}"
    ,teq "intersect s1 s1"        (contains (s1 `intersect` s1) 1) True
    ,teq "intersect -10"          (contains is1to9s6to14 (-10))    False
    ,teq "intersect   5"          (contains is1to9s6to14    5 )    False
    ,teq "intersect   9"          (contains is1to9s6to14    9 )    True
    ,teq "intersect  10"          (contains is1to9s6to14   10 )    False
    ,teq "intersect 100"          (contains is1to9s6to14  100 )    False
    ,teq "intersect s1to9 s6to14" (show is1to9s6to14)              "{6,7,8,9}"

    -- DIFF
    ,teq "diff 1x"           (contains ds1s2          1) True
    ,teq "diff 2x"           (contains ds1s2          2) False
    ,teq "diff 3x"           (contains ds1s2          3) False
    ,teq "diff s1 s2"        (show ds1s2) "{1}"
    ,teq "diff  4"           (contains (diff s1 s1)   1) False
    ,teq "diff  0"           (contains ds1to9s6to14   0) False
    ,teq "diff  1"           (contains ds1to9s6to14   1) True
    ,teq "diff  2"           (contains ds1to9s6to14   2) True
    ,teq "diff  3"           (contains ds1to9s6to14   3) True
    ,teq "diff  4"           (contains ds1to9s6to14   4) True
    ,teq "diff  5"           (contains ds1to9s6to14   5) True
    ,teq "diff  6"           (contains ds1to9s6to14   6) False
    ,teq "diff  7"           (contains ds1to9s6to14   7) False
    ,teq "diff 15"           (contains ds1to9s6to14  15) False
    ,teq "diff s1to9 s6to14" (show ds1to9s6to14)         "{1,2,3,4,5}"

    -- FILTER

    ,teq "filter  0"          (contains fs1to9sEven  0)  False
    ,teq "filter  1"          (contains fs1to9sEven  1)  False
    ,teq "filter  2"          (contains fs1to9sEven  2)  True
    ,teq "filter  3"          (contains fs1to9sEven  3)  False
    ,teq "filter  4"          (contains fs1to9sEven  4)  True
    ,teq "filter  5"          (contains fs1to9sEven  5)  False
    ,teq "filter  6"          (contains fs1to9sEven  6)  True
    ,teq "filter  7"          (contains fs1to9sEven  7)  False
    ,teq "filter  8"          (contains fs1to9sEven  8)  True
    ,teq "filter  9"          (contains fs1to9sEven  9)  False
    ,teq "filter 10"          (contains fs1to9sEven 10)  False
    ,teq "filter 11"          (contains fs1to9sEven 11)  False
    ,teq "filter s1to9 sEven" (show fs1to9sEven)         "{2,4,6,8}"

    -- FORALL
    ,teq "forall 1"     (forall s1                 (== 1) )       True
    ,teq "forall 2"     (forall s2                 (== 1) )       False
    ,teq "forall 3"     (forall s3                 (== 1) )       False
    ,teq "forall True"  (forall (Set (const True)) (const True))  True
    ,teq "forall False" (forall (Set (const True)) (const False)) False
    ,teq "forall >0"    (forall s1to9              (> 0))         True

    -- EXISTS
    ,teq "exists 1"     (exists s1                 (== 1))        True
    ,teq "exists 2"     (exists s2                 (== 1))        False
    ,teq "exists 3"     (exists s3                 (== 1))        False
    ,teq "exists True"  (exists (Set (const True)) (const True))  True
    ,teq "exists False" (exists (Set (const True)) (const False)) False
    ,teq "exists sEven" (exists s1to9              isEven)        True

    -- MAP
    ,teq "map +1" (show (map' s1     (+ 1)))     "{2}"
    ,teq "map +2" (show (map' s6to14 (+ 2)))     "{8,9,10,11,12,13,14,15,16}"
    ,teq "map *x" (show (map' s6to14 (\x->x*x))) "{36,49,64,81,100,121,144,169,196}"

    ]

main = runTestTT tests

-- End of file.
