> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE KindSignatures       #-}
> {-# LANGUAGE PolyKinds            #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE TypeOperators        #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module TLMS where
>
> import Data.Proxy
> import GHC.TypeLits
> import Labels

https://www.athiemann.net/2017/08/31/mergesort.html

Alexander Thiemann

Type Level Merge Sort (Haskell)

Aug 31, 2017

Motivation : records in superrecord are represented as lists of type level pairs
- one holding the key
- one the type of the value

Goal : type family (i.e,, function)
- given type level list : e.g., : '["int" := Int, "foo" := String]
- returns '[ "foo" := String, "int" := Int]
  sort by key field ("int" > "foo")

merge sort recursively divides input list into smaller sublists until
sublists are trivially sorted, then merges the sublists while returning up the call chain.

For the above division: type level take and drop.

> type family ListTake (xs :: [k]) (n :: Nat) :: [k] where
>   ListTake      '[]  n = '[]
>   ListTake       xs  0 = '[]
>   ListTake (x ': xs) n = (x ': ListTake xs (n - 1))

> type family ListDrop (xs :: [k]) (n :: Nat) :: [k] where
>   ListDrop      '[]  n = '[]
>   ListDrop       xs  0 = xs
>   ListDrop (x ': xs) n = ListDrop xs (n - 1)

> _testDrop2 :: ( ListDrop '[1, 2, 3, 4] 2 ~ x, x ~ '[3, 4] ) => Proxy x
> _testDrop2 = Proxy

If implemented correctly, it compiles.
Otherwise “overlapping patterns” warning.

need to know the list length in order to break lists into halves.

> type family LengthOf (xs :: [k]) :: Nat where
>   LengthOf '[] = 0
>   LengthOf (x ': xs) = 1 + LengthOf xs

> _testLengthOf :: ( LengthOf '[1, 2, 3, 4] ~ x, x ~ 4 ) => Proxy x
> _testLengthOf = Proxy

Need to divide a type level number (the list length) by two.
Not provided by GHC.TypeLits.

Attempted utility:

type family If (cond :: Bool) (ifTrue :: k) (ifFalse :: k) :: k where
    If 'True  x y = x
    If 'False x y = y

problem (especially when using it with recursion) : not "lazy"
- so both branches get fully reduced
- can not use to check for a terminating condition (it recurses forever)000000000000000000

Writing type families is kind-of stating reduction rules.
So check must be implemented inline in a type family:

> type family HalfOfHelper (n :: Nat) (i :: Nat) (dist :: Nat) (o :: Ordering) :: Nat where
>   HalfOfHelper n m dist 'GT = m - 1
>   HalfOfHelper n m dist 'EQ = m
>   HalfOfHelper n m    1 'LT = m
>   HalfOfHelper n m dist 'LT =
>     HalfOfHelper  n
>                           (m + 2)
>                  (n -    ((m + 2) * 2))
>                  (CmpNat ((m + 2) * 2) n)
>
> type family HalfOf (n :: Nat) :: Nat where
>   -- optimizations for faster compilation
>   HalfOf 0 = 0
>   HalfOf 1 = 1
>   HalfOf 2 = 1
>   HalfOf 3 = 1
>   HalfOf 4 = 2
>   HalfOf 5 = 2
>   HalfOf 6 = 3
>   HalfOf 7 = 3
>   HalfOf 8 = 4
>   HalfOf 9 = 4
>   HalfOf 10 = 5
>   -- general case
>   HalfOf n = HalfOfHelper n 0 n 'LT -- usually (CmpNat 0 n), but 0 ist already handled!

> _testHalfOf99 :: ( HalfOf 99 ~ x, x ~ 49 ) => Proxy x
> _testHalfOf99 = Proxy
>
> _testHalfOf100 :: ( HalfOf 100 ~ x, x ~ 50 ) => Proxy x
> _testHalfOf100 = Proxy

Note:
- Type checker has reduction limitation of 201 (default).
- Therefore can only divide numbers by two up to n = 793.

The specific use case uses labels operator (:=)

> type family FieldListMergeHelper (xs :: [*]) (ys :: [*]) (o :: Ordering) :: [*] where
>   FieldListMergeHelper (x := xv ': xs) (y := yv ': ys) 'GT =
>     (y := yv) ': FieldListMerge (x := xv ': xs) ys
>   FieldListMergeHelper (x := xv ': xs) (y := yv ': ys) leq =
>     (x := xv) ': FieldListMerge xs (y := yv ': ys)
>
> type family FieldListMerge (xs :: [*]) (ys :: [*]) :: [*] where
>   FieldListMerge             xs             '[] = xs
>   FieldListMerge            '[]              ys = ys
>   FieldListMerge (x := xv ': xs) (y := yv ': ys) =
>     FieldListMergeHelper (x := xv ': xs) (y := yv ': ys) (CmpSymbol x y)

- used helper to work around if-then-else
- depending on key of (key := type) tuple, merge head of either left or right list.

Above supports split/merge of lists.

> type family ListSortStep (xs :: [*]) (halfLen :: Nat) :: [*] where
>   ListSortStep xs halfLen =
>     FieldListMerge
>       (FieldListSort (ListTake xs halfLen))
>       (FieldListSort (ListDrop xs halfLen))
>
> type family FieldListSort (xs :: [*]) :: [*] where
>   FieldListSort     '[] = '[]
>   FieldListSort    '[x] = '[x]
>   FieldListSort '[x, y] = FieldListMerge '[x] '[y] -- optimization
>   FieldListSort     xs  = ListSortStep xs (HalfOf (LengthOf xs))

Helper to prevent the duplicate reduction of HalfOf (LengthOf xs).

> _testSort2 ::
>   ( FieldListSort '["test" := Int, "abc" := String] ~ x
>   , x ~ '["abc" := String, "test" := Int]
>   ) => Proxy x
> _testSort2 = Proxy
>
> _testSort3 ::
>   ( FieldListSort '["test" := Int, "abc" := String, "def" := String] ~ x
>   , x ~ '["abc" := String, "def" := String, "test" := Int]
>   ) => Proxy x
> _testSort3 = Proxy

TODO
- generalize FieldListSort allows sorting lists of [k], given
  - a comparator function f -> f -> Ordering, and
  - a mapper k -> f to extract the sorting criterion.

Alternative to drop/take + length
- type function that takes a list taking the two first elements at a time
  and putting them into different components of a tuple.

Use singletons?

