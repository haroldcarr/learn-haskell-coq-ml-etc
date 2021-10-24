module HC-Lec1 where

-- ==============================================================================
-- Lecture 1 : Programs and Proofs
-- https://www.youtube.com/watch?v=O4oczQry9Jw&t=1412s

------------------------------------------------------------------------------
-- some basic logical types

-- 13:22 -- like Haskell Void
data Zero : Set where
  -- No constructors, so no inhabitants
  -- Represents logical impossibility.

-- 16:45 -- like Haskell ()
record One : Set where
  constructor <> -- added in video Lecture 2 4:50
  -- This record has no fields.
  -- This record has exactly one inhabitant.

-- 18:42 -- like Haskell Either
data _+_ (S : Set) (T : Set) : Set where
  inl : S -> S + T -- left  constructor
  inr : T -> S + T -- right constructor

-- 24:50 -- like Haskell pair ( , )
record _*_ (S : Set) (T : Set) : Set where
  constructor _,_ -- added in video Lecture 2 4:58
  field
    fst : S
    snd : T

------------------------------------------------------------------------------
-- examples

-- 28:36
comm-* : {A : Set} {B : Set} -> A * B -> B * A
comm-* record { fst = a ; snd = b } = record { fst = b ; snd = a }

-- 43:00
assocLR-+ : {A B C : Set} -> (A + B) + C -> A + (B + C)
assocLR-+ (inl (inl a)) =      inl a
assocLR-+ (inl (inr b)) = inr (inl b)
assocLR-+      (inr c)  = inr (inr c)

-- 47:34
-- If you can arrive at a contradiction, then you can derive any conclusion.
-- Given a Zero, must produce an X for ANY X.
-- But there are no inhabitants of Zero.
-- So use 'absurd' pattern that indicates something that is IMPOSSIBLE.
naughtE : {X : Set} -> Zero -> X
naughtE ()

-- standard composition: f << g is "f after g"
_<<_ : {X Y Z : Set} -> (Y -> Z) -> (X -> Y) -> (X -> Z)
(f << g) x = f (g x)

-- diagrammatic composition: f >> g is "f then g"
_>>_ : {X Y Z : Set} -> (X -> Y) -> (Y -> Z) -> (X -> Z)
                     --       ^^^^^^^^          dominoes!
(f >> g) x = g (f x)

-- infix application
_$_ : {S : Set}{T : S -> Set}(f : (x : S) -> T x)(s : S) -> T s
f $ s = f s
infixl 2 _$_

-- ==============================================================================
-- Lecture 2 : more Programs and Proof, Introducing "with"
-- https://www.youtube.com/watch?v=qcVZxQTouDk

-- 2:25
_$*_ : {A A' B B' : Set} -> (A -> A') -> (B -> B') -> A * B -> A' * B'
(a→a' $* b→b') (a , b) = (a→a' a) , (b→b' b)

-- 6:53
_$+_ : {A A' B B' : Set} -> (A -> A') -> (B -> B') -> A + B -> A' + B'
(a→a' $+ b→b') (inl a) = inl (a→a' a)
(a→a' $+ b→b') (inr b) = inr (b→b' b)

-- 8:52 -- like Haskell const ; this is pure for applicative functor needing an E
combinatorK : {A E : Set} -> A -> E -> A
combinatorK a _ = a

-- 11:25 -- this is application
combinatorS : {S T E : Set} -> (E -> S -> T) -> (E -> S) -> E -> T
combinatorS e→s→t e→s e = e→s→t e (e→s e)

-- 14:50
idK : {X : Set} -> X -> X
idK x = combinatorK x x

-- 19:00
idSKK : {X : Set} -> X -> X
idSKK {X} = combinatorS combinatorK (combinatorK {E = X}) -- 'Zero' and 'One' also work

id : {X : Set} -> X -> X
-- id x = x -- is the easy way; let's do it a funny way to make a point
id = combinatorS combinatorK (combinatorK {_} {Zero})
--                          no choice for -^   ^^^^- could be anything

-- 30:25
-- naughtE

------------------------------------------------------------------------------
-- from logic to data

-- 32:05

data Nat : Set where
  zero : Nat
  suc  : Nat -> Nat -- recursive data type
{-# BUILTIN NATURAL Nat #-} -- enables decimal notation

-- 32:58
_+N_ : Nat -> Nat -> Nat
zero  +N y = y
suc x +N y = suc (x +N y)

four : Nat
four = 2 +N 2

------------------------------------------------------------------------------
-- and back to logic

-- 36:46
data _==_ {X : Set} : X -> X -> Set where
  refl : (x : X) -> x == x -- the relation that is "only reflexive"
{-# BUILTIN EQUALITY _==_ #-}

see4 : (2 +N 2) == 4
see4 = refl 4

-- 42:35
-- application of equalities
_=$=_ : {X Y : Set} {f f' : X -> Y} {x x' : X}
     -> f   == f'
     ->   x ==    x'
     -> f x == f' x
refl f =$= refl x = refl (f x)

------------------------------------------------------------------------------
-- computing types

-- 45:00
_>=_ : Nat -> Nat -> Set
x     >= zero  = One     -- i.e., true
zero  >= suc y = Zero    -- i.e., false
suc x >= suc y = x >= y

--a0 : 2 >= 4
--a0 = {!!}

a1 : 4 >= 2
a1 = <>

refl->= : (n : Nat) -> n >= n
refl->=  zero   = <>
refl->= (suc n) = refl->= n

trans->= : (x y z : Nat) -> x >= y -> y >= z  -> x >= z
trans->= zero     zero        z  x>=y y>=z = y>=z
trans->= (suc x)  zero    zero   x>=y y>=z = <>
trans->= (suc x) (suc y)  zero   x>=y y>=z = <>
trans->= (suc x) (suc y) (suc z) x>=y y>=z = trans->= x y z x>=y y>=z

------------------------------------------------------------------------------
-- construction by proof

-- 47:38

record Sg (S : Set) (T : S -> Set) : Set where -- Sg is short for "Sigma"
  constructor _,_
  field
    fst : S       -- a value
    snd : T fst   -- some evidence about that value

difference : (m n : Nat) -> m >= n -> Sg Nat λ d -> m == (n +N d)
difference      m   zero   m>=n = m , refl m
difference  zero   (suc n) ()
difference (suc m) (suc n) m>=n
  with difference m n m>=n
...| d , e
   = d , xxx m n d e
 where
  xxx : ∀ (m n d : Nat) -> m == (n +N d) -> suc m == suc (n +N d)
  xxx m n d p rewrite p = refl (suc (n +N d))

-- ==============================================================================
-- Lecture 3 : Proof by induction
-- https://www.youtube.com/watch?v=8xFT9FPlm18

-- 6:20
-- Nat with zero, +N and assocLR-+ form a monoid.

-- 8:48
zero-+N : (n : Nat) -> (zero +N n) == n
zero-+N n = refl n -- by definition

-- 9:36
-- version in video
-- +N-zero' : (n : Nat) -> (n +N zero) == n
-- +N-zero'  zero   = refl zero -- by definition
-- +N-zero' (suc n) = refl suc =$= +N-zero' n -- TODO : does not compile

-- HC alternate version
+N-zero : (n : Nat) -> (n +N zero) == n
+N-zero  zero   = refl zero -- by definition
+N-zero (suc n)
  with +N-zero n
...| n+N0==n
  rewrite n+N0==n
   = refl (suc n)

-- 26:00
-- version in video
-- assocLR-+N' : (x y z : Nat) -> ((x +N y) +N z) == (x +N (y +N z))
-- assocLR-+N'  zero   y z = refl (y +N z)
-- assocLR-+N' (suc x) y z = refl suc =$= assocLR-+N' x y z -- TODO : does not compile

-- 30:21
assocLR-+N : (x y z : Nat) -> ((x +N y) +N z) == (x +N (y +N z))
assocLR-+N  zero   y z = refl (y +N z)
assocLR-+N (suc x) y z
  rewrite assocLR-+N x y z
  = refl (suc (x +N (y +N z)))

------------------------------------------------------------------------------
-- computing types

-- 34:46
refl->=' : (n : Nat) -> n >= n
refl->='  zero   = <>
refl->=' (suc n) = refl->=' n

-- 41:00
trans->=' : (x y z : Nat) -> x >= y -> y >= z  -> x >= z
-- start with 'z'
trans->='      x       y   zero   x>=y y>=z = <>
trans->=' (suc x) (suc y) (suc z) x>=y y>=z = trans->=' x y z x>=y y>=z

-- ==============================================================================
-- Lecture 4 : Sigma, Difference, Vector Take
-- https://www.youtube.com/watch?v=OZeDRtRmgkw

-- 0:46 Sigma type (see above)

-- 5:10 -- make _*_ from Sg
_*'_ : (S : Set) -> (T : Set) -> Set
s *' t = Sg s λ _ -> t

-- 10:55 difference (see above)
difference' : (m n : Nat) -> m >= n -> Sg Nat λ d -> m == (n +N d)
-- 1st clause of >= matches on right, so start with 'n'
difference'      m   zero   m>=n = m , refl m
difference' (suc m) (suc n) m>=n
  with difference' m n m>=n
...| d , m==n+Nd
  rewrite m==n+Nd
  = d , refl (suc (n +N d)) -- video also uses refl suc =$= m==n+Nd

-- 27:24 -- pattern match on proof of equation ; here 'm==n+Nd'
difference'' : (m n : Nat) -> m >= n -> Sg Nat λ d -> m == (n +N d)
-- 1st clause of >= matches on right, so start with 'n'
difference''      m   zero   m>=n = m , refl m
difference'' (suc m) (suc n) m>=n
  with difference'' m n m>=n
...| d , m==n+Nd
  with m==n+Nd
... | refl .(n +N d) = d , (refl (suc (n +N d)))

tryMe     = difference 42 37
-- dontTryMe = difference 37 42 {!!}

------------------------------------------------------------------------------
-- things to remember to say

{-
-- 34:22 : = VIZ ==
=  : makes a definition (part of Agda programming language)
== : makes a type (by user built definition)

function type is both IMPLICATION and UNIVERSAL QUANTIFICATION : why call Pi?

why is Sigma called Sigma?

-- 38:50 : B or not B
must be able to return a
- inl B      for any B : not possible because nothing known about B
- inR B→Zero for any B : not possible because Zero has no elements
exMiddle : {B : Set} -> B + (B -> Zero)
exMiddle = ?

-- 40:45
-- not possible to "look inside" 'notAandB' to determine 'inl' or 'inr'
deMorgan : {A B : Set} -> ((A * B) -> Zero) -> (A -> Zero) + (B -> Zero)
deMorgan notAandB = {!!}
-}





