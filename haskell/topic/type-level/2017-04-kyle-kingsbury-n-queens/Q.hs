{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module Q where

{-
https://aphyr.com/posts/342-typing-the-technical-interview
Typing the technical interview 2017/04/10
-}

data Nil
data Cons x xs

class First list x | list -> x
instance First Nil Nil
instance First (Cons x more) x

class ListConcat a b c | a b -> c
instance ListConcat Nil x x
instance (ListConcat         as  bs         cs)
      =>  ListConcat (Cons a as) bs (Cons a cs)

-- Concatenate all lists in a list
class ListConcatAll ls l | ls -> l
instance ListConcatAll Nil Nil
instance (ListConcat chunk acc result, ListConcatAll             rest acc)
 =>                                    ListConcatAll (Cons chunk rest) result

data True
data False

-- Is any element of this list True?
class AnyTrue list t | list -> t
instance AnyTrue Nil              False
instance AnyTrue (Cons True more) True
instance (AnyTrue list t)
  => AnyTrue (Cons False list) t

class Not b1 b | b1 -> b
instance Not False True
instance Not True  False

class Or b1 b2 b | b1 b2 -> b
instance Or True  True  True
instance Or True  False True
instance Or False True  True
instance Or False False False

data Z
data S n

type N0 = Z
type N1 = S N0
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7

-- Equality
class PeanoEqual a b t | a b -> t
instance  PeanoEqual  Z     Z    True
instance  PeanoEqual (S a)  Z    False
instance  PeanoEqual  Z    (S b) False
instance (PeanoEqual    a     b  t)
       => PeanoEqual (S a) (S b) t

-- Comparison (<)
class PeanoLT a b t | a b -> t
instance  PeanoLT     Z     Z    False
instance  PeanoLT    (S a)  Z    False
instance  PeanoLT     Z    (S b) True
instance (PeanoLT       a     b  t)
       => PeanoLT    (S a) (S b) t

-- Absolute difference
class PeanoAbsDiff a b c | a b -> c
instance PeanoAbsDiff   Z     Z     Z
instance PeanoAbsDiff   Z    (S b) (S b)
instance PeanoAbsDiff  (S a)    Z  (S a)
instance (PeanoAbsDiff    a     b     c)
       => PeanoAbsDiff (S a) (S b)    c

-- Integers from n to 0
class Range n xs | n -> xs
instance  Range Z Nil
instance (Range    n          xs)
       => Range (S n) (Cons n xs)

class LegalCompare t | -> t where
  legalCompare :: t
instance (PeanoEqual (S Z) (S Z) t)
       => LegalCompare t
{-
:t legalCompare
=>              :: True
-}
class IllegalCompare t | -> t where
  illegalCompare :: t
instance (PeanoEqual True (Cons Z False) t)
       => IllegalCompare t
{-
:t illegalCompare
=>                :: PeanoEqual True (Cons Z False) t => t
-}
class Apply f a r | f a -> r

data Conj1 list
instance Apply (Conj1 list) x (Cons x list)

-- Map f over a list
class Map f xs ys | f xs -> ys
instance Map f Nil Nil
instance (Apply f x y, Map f xs ys)
  => Map f (Cons x xs) (Cons y ys)

-- Map f over list and concatenate results together
class MapCat f xs zs | f xs -> zs
instance MapCat f Nil Nil
instance (Map f xs chunks, ListConcatAll chunks ys)
  => MapCat f xs ys

-- Filter a list with an Apply-able predicate function
class AppendIf pred x ys zs | pred x ys -> zs
instance AppendIf True x ys (Cons x ys)
instance AppendIf False x ys ys

class Filter f xs ys | f xs -> ys
instance Filter f Nil Nil
instance (Apply f x t,
          Filter f xs ys,
          AppendIf t x ys zs)
  => Filter f (Cons x xs) zs

data Queen x y
data Queen1 x
instance Apply (Queen1 x) y (Queen x y)

-- A list of queens in row x with y from 0 to n.
class QueensInRow n x queens | n x -> queens
instance (Range n ys, Map (Queen1 x) ys queens)
  => QueensInRow n x queens

-- Does queen a threaten queen b?
class Threatens a b t | a b -> t
instance (PeanoEqual ax bx xeq,
          PeanoEqual ay by yeq,
          Or xeq yeq xyeq,
          PeanoAbsDiff ax bx dx,
          PeanoAbsDiff ay by dy,
          PeanoEqual dx dy deq,
          Or xyeq deq res)
  => Threatens (Queen ax ay) (Queen bx by) res

-- Partial application of Threatens
data Threatens1 a
instance (Threatens a b t)
  => Apply (Threatens1 a) b t

-- Is queen b compatible with all queen as?
class Safe config queen t | config queen -> t
instance (Map (Threatens1 queen) config m1,
          AnyTrue m1 t1,
          Not     t1 t2)
  => Safe config queen t2

data Safe1 config
instance (Safe config queen t)
  => Apply (Safe1 config) queen t

-- Add a queen with the given x coordinate to a legal configuration, returning
-- a set of legal configurations.
class AddQueen n x c cs | n x c -> cs
instance (QueensInRow n x candidates,
          Filter (Safe1 c) candidates filtered,
          Map (Conj1 c) filtered cs)
  => AddQueen n x c cs

data AddQueen2 n x
instance (AddQueen n x c cs)
  => Apply (AddQueen2 n x) c cs

-- Add a queen at x to every configuration, returning a set of legal
-- configurations.
class AddQueenToAll n x cs cs' | n x cs -> cs'
instance (MapCat (AddQueen2 n x) cs cs')
  => AddQueenToAll n x cs cs'

-- Add queens recursively
class AddQueensIf pred n x cs cs' | pred n x cs -> cs'
instance AddQueensIf False n x cs cs
instance (AddQueenToAll n x cs cs2,
          AddQueens n (S x) cs2 cs')
  => AddQueensIf True n x cs cs'

class AddQueens n x cs cs' | n x cs -> cs'
instance (PeanoLT x n pred,
          AddQueensIf pred n x cs cs')
  => AddQueens n x cs cs'

-- Solve
class Solution n c | n -> c where
  solution :: n -> c
instance (AddQueens n Z (Cons Nil Nil) cs, First cs c)
  => Solution n c where solution = undefined
{-
:t solution (undefined :: N6)
=>:: Cons (Queen (S (S (S (S (S Z))))) (S Z))
       (Cons (Queen (S (S (S (S Z)))) (S (S (S Z))))
          (Cons (Queen (S (S (S Z))) (S (S (S (S (S Z))))))
             (Cons (Queen (S (S Z)) Z)
                (Cons (Queen (S Z) (S (S Z)))
                   (Cons (Queen Z (S (S (S (S Z)))))
                      Nil)))))

:t solution (undefined :: N7)
  :: Cons
       (Queen (S (S (S (S (S (S Z)))))) (S N0))
       (Cons
          (Queen (S (S (S (S (S Z))))) (S N2))
          (Cons
             (Queen (S (S (S (S Z)))) (S N4))
             (Cons
                (Queen (S (S (S Z))) Z)
                (Cons
                   (Queen (S (S Z)) (S N1))
                   (Cons (Queen (S Z) (S N3)) (Cons (Queen Z (S N5)) Nil))))))
-}

