module Lec2-HC where

open import Lec1-HC

-- ==============================================================================
-- Lecture 4 : Sigma, Difference, Vector Take
-- https://www.youtube.com/watch?v=OZeDRtRmgkw

-- 43:14

data Vec (X : Set) : Nat -> Set where
  []   :                              Vec X  zero
  _::_ : {n : Nat} -> X -> Vec X n -> Vec X (suc n)
infixr 4 _::_

vTake : (m n : Nat) -> m >= n -> {X : Set} -> Vec X m -> Vec X n
vTake      m   zero   m>=n       xs  = []
vTake (suc m) (suc n) m>=n (x :: xs) = x :: vTake m n m>=n xs

vTake32 : Set
vTake32 = vTake 3 2 <> (1 :: 2 :: 3 :: []) == (1 :: 2 :: [])

_ : vTake 3 2 <> (1 :: 2 :: 3 :: []) == (1 :: 2 :: [])
_ = refl (1 :: 2 :: [])

-- 47:35 : taking ALL elements is an identity
vTakeIdFact : (n : Nat) {X : Set} (xs : Vec X n)
           -> vTake n n (refl->= n) xs == xs
vTakeIdFact .0             [] = refl []
vTakeIdFact (suc n) (x :: xs) = cong (x ::_) (vTakeIdFact n xs)

-- 48:17 : taking p elements from Vm is same as taking n elements from Vm, forming Vn
--         then taking p elments from Vn
vTakeCpFact : (m n p : Nat) (m>=n : m >= n) (n>=p : n >= p) {X : Set} (xs : Vec X m)
           -> vTake m p (trans->= m n p m>=n n>=p)                 xs
           == vTake n p                      n>=p  (vTake m n m>=n xs)
vTakeCpFact      m       n   zero   m>=n n>=p _ = refl []
vTakeCpFact (suc m) (suc n) (suc p) m>=n n>=p (x :: xs) =
  cong (x ::_) (vTakeCpFact  m n p m>=n n>=p xs)

-- 48:54
-- vTakeIdFact : reflexivity  turns into identity
-- vTakeCpFact : transitivity turns into composition

-- ==============================================================================
-- Lecture 5 : How Rewrite Works
-- https://www.youtube.com/watch?v=b5salYMZoyM

-- 3:34
vTake53 : Vec Nat 3
vTake53 = vTake 5 3 <> (1 :: 2 :: 3 :: 4 :: 5 :: [])

-- 5:30
vTake' : ∀ {m : Nat} {X : Set}
      -> (n : Nat) -> m >= n -> Vec X m
      -> Vec X n
vTake'     {m}  zero   m>=n       xs  = []
vTake' {suc m} (suc n) m>=n (x :: xs) = x :: vTake' {m} n m>=n xs

{-
-- 7:45
rigid symbols : constructors (value and type)
- if you are trying to tell if two usages of constructors match each other,
  you can compare them component-wise
- no defined computational meaning - they are what they are
- e.g., Vec Nat 3
non-rigid
- e.g., >=
- can compute


-- 15:48
vTake : think of as turning a '>=' proof into a vector operation.
- In comes a proof on numbers, out comes an operation on vectors.
vTakeIdFact : identity proof on numbers turns into identity operation on vectors
vTakeCpFact : proof of transitivity on numbers turns into composition operation on vectors

-- 24:08 : best practice : use record (instead of data) when possible : agda is more aggressive

-- 24:28 : HOW REWRITE WORKS
Rewrite is shorthand for
- abstract the LHS   of the equation using 'with'
- abstract the proof of the equation using 'with'
- then pattern matching on the proof
-}

------------------------------------------------------------------------------
-- Splittings (which bear some relationship to <= from ex1)
-- 39:00

data _<[_]>_ : Nat -> Nat -> Nat -> Set where
  zzz : zero <[ zero ]> zero

  lll : {l m r : Nat} ->     l <[     m ]>     r
                      -> suc l <[ suc m ]>     r

  rrr : {l m r : Nat} ->     l <[     m ]>     r
                      ->     l <[ suc m ]> suc r

xzzz : Set
xzzz = zero <[ zero ]> zero

x1z1 : Set
x1z1 = 1 <[ zero ]> 1

x159 : Set
x159 = 1 <[ 5 ]> 9

x951 : Set
x951 = 9 <[ 5 ]> 1

-- 41:08
-- combine two vectors into one according to given instructions

-- this is kind of a "double-sided" '<='
{-        collection
              * <--- *
       * ---> *
       * ---> *
              * <--- *
       * ---> *
 3 from left     2 from right
-}
_>[_]<_ : {X : Set} {l m r : Nat}
       ->     Vec X l
       ->           l <[ m ]>       r -- instructions
             ->               Vec X r
       ->          Vec X m
-- why is rrr the first line?
-- 29:49 https://www.youtube.com/watch?v=RW4aC_6n0yQ
-- do NOT pattern match on vector first (LHS of first clause)
-- pattern match on instructions first
-- i.e., no pattern match on xl, so goes to try to match instruction : >[ rrr mmm ]<
xl        >[ rrr mmm ]< (x :: xr) = x :: (xl >[ mmm ]< xr)
(x :: xl) >[ lll mmm ]<       xr  = x :: (xl >[ mmm ]< xr)
[]        >[ zzz     ]<       []  = []

v6rl : Vec Nat 6
v6rl = (0 :: 2 :: 3 :: []) >[ rrr (rrr (rrr (lll (lll (lll zzz))))) ]< (7 :: 8 :: 9 :: [])
_ : v6rl == (7 :: 8 :: 9 :: 0 :: 2 :: 3 :: [])
_ = refl    (7 :: 8 :: 9 :: 0 :: 2 :: 3 :: [])

v6   : Vec Nat 6
v6   = (0 :: 2 :: 3 :: []) >[ lll (rrr (lll (rrr (lll (rrr zzz))))) ]< (7 :: 8 :: 9 :: [])
_ : v6 == (0 :: 7 :: 2 :: 8 :: 3 :: 9 :: [])
_ = refl  (0 :: 7 :: 2 :: 8 :: 3 :: 9 :: [])

-- 44:27

-- split a vector into two vectors according to given instructions
-- reverses '<[ m ]>' - instructions for taking things apart
data FindSplit {X : Set} {l m r : Nat}
     (nnn : l <[ m ]> r) : (xs : Vec X m) -> Set where
  splitBits : (xl : Vec X l) (xr : Vec X r) -> FindSplit nnn (xl >[ nnn ]< xr)

-- proves FindSplit
findSplit : {X : Set} {l m r : Nat} (nnn : l <[ m ]> r) (xs : Vec X m) -> FindSplit nnn xs

findSplit  zzz      [] = splitBits [] []

findSplit (lll nnn) (x :: xs) with findSplit nnn xs
                                           -- FindSplit (lll nnn) (x :: (xl >[ nnn ]< xr))
findSplit (lll nnn) (x :: .(xl >[ nnn ]< xr)) | splitBits xl xr = splitBits (x :: xl) xr

findSplit (rrr nnn) (x :: xs) with findSplit nnn xs
                                           -- FindSplit (rrr nnn) (x :: (xl >[ nnn ]< xr))
findSplit (rrr nnn) (x :: .(xl >[ nnn ]< xr)) | splitBits xl xr = splitBits xl (x :: xr)

-- ==============================================================================
-- https://www.youtube.com/watch?v=RW4aC_6n0yQ

-- 5:08
{-
The o' <= constructor : you can keep increasing the RHS

  oz -- stop
  os -- take this one and keep going
  o' -- skip this one and keep going

Tabulating the ways of choosing N things from M things
A path is a way to get a specific embedding.
The numbers in parenthesis shows the number of paths to a specific embedding (Pascal's triangle).

                                     oz
                                    0<=0
                                    (1)
                                   /    \
                                 /        \
                               /            \
                             o'              os
                          0<=1                1<=1
                          (1)                 (1)
                         /    \              /    \
                       /        \          /        \
                     /            \      /            \
                   o'              os   o'             os
                  0<=2              1<=2              2<=2
                  (1)               (2)               (2)
                 /    \            /    \            /    \
               /        \        /        \        /        \
             /            \     /           \    /            \
           o'              os  o'           os  o'             os
          0<=3              1<=3             2<=3             3<=3
          (1)               (3)              (3)              (1)

witnesses to M choose N
read a value in a set of this type as instructions for choosing
-}

-- 14:58
-- 'findSplit' (above)
-- 23:20
-- last clause of 'findSplit'
-- 29:49 : the same ERROR I had before changing order of clauses in _>[_]<_
-- 34:22 : rule of thumb
-- if there is one column of args, where on every line is a constructor then no change
-- otherwise, choose ordering carefully
-- 35:52 : final order of clauses of of  _>[_]<_ -- tidy to have nil case at top or bottom
-- 40:15 shows how agda/haskell does case splitting on _>[_]<_




