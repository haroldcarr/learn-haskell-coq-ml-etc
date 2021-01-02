module z03-natural-numbers where

open import bool
open import bool-thms using (𝔹-contra)
open import eq
open import level

-- p 50

data ℕ : Set where
  zero :     ℕ
  suc  : ℕ → ℕ

-- p 51

{-# BUILTIN NATURAL ℕ #-}

-------------------------
_+_ : ℕ → ℕ → ℕ
zero  + n =          n
suc m + n = suc (m + n) -- recursive call on structurally smaller left side 'm' -- termination

-- p 53

-------------------------
0+ : ∀ (x : ℕ)
   → 0 + x ≡ x
0+ _ = refl -- via definitional equality

{- fails because _+_ defined in terms of pattern matching on left arg
+0 : ∀ (x : ℕ)
    → x + 0 ≡ x
+0 _ = refl

-- p 55

solution is for proof to call itself recursively:
-------------------------
-}
+0 : ∀ (x : ℕ)
   → x + 0 ≡ x

-- BASE
+0  zero                = refl -- via definitional equality

-- INDUCTIVE
-- note: name of 'y' variable, if renamed to 'x', is not same as 'x' in signature
+0 (suc y) rewrite +0 y = refl
{-    ^
      |
'x' in sig instantiated to 'suc y'

so trying to prove (the goal) : suc y + 0 ≡ suc y

goal can be simplified by inductive case of def of _+_

so goal definitionally equal to : suc (y + 0) ≡ suc y

       rewrite recursive call proves   y + 0  ≡     y

  type of proof is x + 0 ≡ x
  so Agda with replace occurrences of x + 0 with x

                       leaving  : suc  y      ≡ suc y

-- p 57

PROOF BY INDUCTION: recursive proofs

------------------------------------------------------------------------------
+ ASSOCIATIVITY

When theorem has multiple variables, must decide which one to use for induction.
Generally: var used where a recursive fun will patten match on it.
If several vars are pattern matched, prefer the one that is matched the most.

Since _+_ matches on 1st arg, then do induction here on 1st arg.
Here 'x' is matched twice but 'y' only once -- so use 'x'.
-}

-------------------------
+assoc : ∀ (x y z : ℕ)
       →  x + (y  + z)
       ≡ (x +  y) + z
{-
BASE case
  goal: zero + (y + z) ≡ (zero + y) + z
  via def/eq
-}
+assoc  zero   y z                      = refl

{-
INDUCTIVE case
  goal          : suc (x + (y + z)) ≡ suc ((x + y) + z)
  need proof of :     (x + (y + z)) ≡     ((x + y) + z)
    that is exactly the inductive hypotheses
    so use rewrite to recursively apply the proof
-}
+assoc (suc x) y z rewrite +assoc x y z = refl

------------------------------------------------------------------------------
-- p 60 + COMMUTATIVITY (rewrite with multiple equations)

-------------------------
+suc : ∀ (x y : ℕ) → x + (suc y) ≡ suc (x + y)
+suc  zero   y                  = refl
+suc (suc x) y rewrite +suc x y = refl

-------------------------
+comm : ∀ (x y : ℕ) → x + y ≡ y + x
{-
BASE
 Goal:         : y ≡ (y + zero)
 so rewrite right hand side using +0 proof
 simplifies to : y ≡  y
-}
+comm  zero   y rewrite +0 y = refl
{-
INDUCTIVE
 Goal : suc (x + y) ≡ (y + suc x)
-}
+comm (suc x) y rewrite +suc y x | +comm x y = refl

------------------------------------------------------------------------------
-- p 62 MULTIPLICATION

-------------------------
_*_ : ℕ → ℕ → ℕ
zero  * n = zero
suc m * n = n + (m * n)

-------------------------
-- p 63 - right distributivity of * over +

-- variable counts x : 2; y : 1; z : 0 : so use x

*distribr : ∀ (x y z : ℕ)
          → (x + y) * z ≡ (x * z) + (y * z)

-- BASE
*distribr  zero   y z = refl

{-
INDUCTIVE
Goal:  ((suc x + y) * z)   ≡ ((suc x * z)  + (y * z))
Goal: (z + ((x + y) * z)) ≡ ((z + (x * z)) + (y * z))

*distribr (suc x) y z rewrite *distribr x y z = {!!}
Goal: (z + ((x * z) + (y * z))) ≡ ((z + (x * z)) + (y * z))
      right-associated                left-associated
so reassociate: to prove
A + (B + C) ≡ (A + B) + C
^    ^   ^
|    |   |
z    |   |
   x * z |
       y * z
-}
*distribr (suc x) y z rewrite *distribr x y z =
  +assoc z (x * z) (y * z)

------------------------------------------------------------------------------
-- p 65 * COMMUTATIVITY

-------------------------
*0 : ∀ (x : ℕ)
   → x * 0 ≡ 0

-- BASE
*0 zero = refl -- def/eq : (zero * 0) ≡ 0
{-
INDUCTIVE
(suc x * 0) ≡ 0
    (x * 0) ≡ 0 -- IH
rewrite
         0  ≡ 0
-}
*0 (suc y) rewrite *0 y = refl

-------------------------

*suc : ∀ (x y : ℕ) → x * (suc y) ≡ x + (x * y)
{-
BASE
         (zero * suc y) ≡ (zero + (zero * y))
def/eq    zero          ≡  zero
-}
*suc zero y = refl
{-
INDUCTIVE
                (suc x *   suc y)   ≡ (suc   x +   (suc x * y))
def/eq    suc  (y + (x *   suc y))  ≡  suc  (x + (y  + (x * y)))
rw *suc   suc  (y + (x  + (x * y))) ≡  suc  (x + (y  + (x * y)))
rw +assoc suc ((y +  x) + (x * y))  ≡  suc  (x + (y  + (x * y)))
rw +assoc suc ((y +  x) + (x * y))  ≡  suc ((x +  y) + (x * y))
rw +comm  suc ((x +  y) + (x * y))  ≡  suc ((x +  y) + (x * y))
-}
*suc (suc x) y
  rewrite
    *suc   x y
  | +assoc y x (x * y)
  | +assoc x y (x * y)
  | +comm  y x = refl

-------------------------

*comm : ∀ (x y : ℕ) → x * y ≡ y * x
{-
BASE
             (zero * y) ≡ (y * zero)
def/eq        zero      ≡ (y * zero)
rs *0         zero      ≡      zero
-}
*comm zero y rewrite *0 y = refl
{-
INDUCTIVE
           (suc x * y)  ≡ (y *  suc x)
def/eq    (y + (x * y)) ≡ (y *  suc x)
rw *suc   (y + (x * y)) ≡ (y + (y * x))
rw *comm  (y + (y * x)) ≡ (y + (y * x))
-}
*comm (suc x) y
  rewrite
    *suc  y x
  | *comm x y = refl

------------------------------------------------------------------------------
-- p 66 * ASSOCIATIVITY

*assoc : ∀ (x y z : ℕ) → x * (y * z) ≡ (x * y) * z
{-
BASE
        (zero * (y * z)) ≡ ((zero * y) * z)
def/eq   zero            ≡   zero
-}
*assoc  zero   y z = refl
{-
INDUCTIVE

                      (suc x *  (y  * z))  ≡        ((suc x * y)  * z)
def/eq        ((y * z) +  (x *  (y  * z))) ≡ ((y      +  (x * y)) * z)
rw *distribr  ((y * z) +  (x *  (y  * z))) ≡ ((y * z) + ((x * y)  * z))
rw *assoc     ((y * z) + ((x *   y) * z))  ≡ ((y * z) + ((x * y)  * z))
-}
*assoc (suc x) y z
  rewrite
    *distribr y (x * y) z
  | *assoc x y z = refl

------------------------------------------------------------------------------
-- p 67 LESS-THEN <

_<_ : ℕ → ℕ → 𝔹
0       <      0  = ff
0       < (suc y) = tt
(suc x) < (suc y) = x < y
(suc x) <      0  = ff

-------------------------
-- < is TRANSITIVE

{-
xx : ∀ {x y z : ℕ}
   → x < y
   → y < z
   → x < z

Above will not type check because _<_ returns 𝔹 VALUE, not a type
Types have type Set in Agda -- 𝔹 is not the same as Set.
Could define _<_ as a relation: ℕ → ℕ → Set.
But then _<_ could not be used computationally.
Expressions with type Set describe code -- they are NOT code themselves
- e.g., cannot pattern-match on expressions of type Set
        cannot compute with them
Transitivity theorem is statement about behavior of the PROGRAM _<_.
-}

-- p 69

-------------------------
<-0 : ∀ (x : ℕ) → x < 0 ≡ ff
<-0      0  = refl --  (zero < 0) ≡ ff; ff ≡ ff
<-0 (suc y) = refl -- (suc y < 0) ≡ ff; ff ≡ ff

-------------------------
<-trans : ∀ {x y z : ℕ}
        → x < y ≡ tt
        → y < z ≡ tt
        → x < z ≡ tt
{-
       p1 : (x < zero) ≡ tt    ; p2 : (zero < z) ≡ tt      = Goal: (x < z) ≡ tt
rw <-0 p1 : ff         ≡ tt    ; p2 : (0    < z) ≡ tt      = Goal: (x < z) ≡ tt
-}
<-trans     {x}     {0}         p1 p2 rewrite <-0 x = 𝔹-contra p1

-- can't be called like this so no proof needed
<-trans     {0} {suc y}     {0} p1 ()

--     p1 : (zero < suc y) ≡ tt ; p2 : (suc y < suc z) ≡ tt = Goal: (zero < suc z) ≡ tt
<-trans     {0} {suc y} {suc z} p1 p2 = refl

-- can't be called like this so no proof needed
<-trans {suc x} {suc y}     {0} p1 ()

--     p1 : (suc x < suc y) ≡ tt; p2 : (suc y < suc z) ≡ tt = Goal: (suc x < suc z) ≡ tt
<-trans {suc x} {suc y} {suc z} p1 p2 = <-trans {x} {y} {z} p1 p2 -- uses IH via recursive call

------------------------------------------------------------------------------
-- p 71 EQUALITY TEST for ℕ
{-
so far, rely on
- def/eq : done automatically during type checking
- ≡      : express, as a TYPE, a proposition that two VALUES are equal

there are other kinds of equality, e,g.,

COMPUTATIONAL EQUALITY : tests VALUE equality
-}

_=ℕ_ : ℕ → ℕ → 𝔹
0     =ℕ     0 = tt
suc x =ℕ suc y = x =ℕ y
_     =ℕ     _ = ff

_≤_  : ℕ → ℕ → 𝔹
x ≤ y = (x < y) || x =ℕ y

-------------------------

=ℕ-refl : ∀ (x : ℕ) → (x =ℕ x) ≡ tt
=ℕ-refl 0 = refl
=ℕ-refl (suc x) = (=ℕ-refl x)

-------------------------

-- SOUNDNESS property: things indicated to be true really are true
=ℕ-to-≡ : ∀ {x y : ℕ} → x =ℕ y ≡ tt → x ≡ y
=ℕ-to-≡     {0}     {0} _ = refl
=ℕ-to-≡ {suc x}     {0} ()
=ℕ-to-≡     {0} {suc y} ()
=ℕ-to-≡ {suc x} {suc y} p
  rewrite
    =ℕ-to-≡ {x} {y} p = refl

-------------------------

=ℕ-from-≡ : ∀ {x y : ℕ}
          → x ≡ y
          → x =ℕ y ≡ tt
=ℕ-from-≡ {x} refl = =ℕ-refl x

------------------------------------------------------------------------------
-- p 73 EXERCISES

-- 1 nat-thms

--------------------------------------------------
-- properties of addition
--------------------------------------------------

+1 : ∀ (x : ℕ) → x + 1 ≡ suc x
+1 zero = refl
+1 (suc n) rewrite +1 n = refl

+perm : ∀ (x y z : ℕ) → x + (y + z) ≡ y + (x + z)
+perm zero y z = refl
+perm (suc x) y z -- (suc  x + (y + z)) ≡ (y + (suc  x + z))
                  --  suc (x + (y + z)) ≡ (y +  suc (x + z))
  rewrite
    +suc y (x + z) -- suc (x + (y + z)) ≡ suc (y + (x + z))
  | +assoc x y z   -- suc ((x + y) + z) ≡ suc (y + (x + z))
  | +comm x y      -- suc ((y + x) + z) ≡ suc (y + (x + z))
  | +assoc y x z   -- suc ((y + x) + z) ≡ suc ((y + x) + z)
  = refl

--------------------------------------------------
-- properties of multiplication
--------------------------------------------------

*1 : ∀ {n : ℕ} → n * 1 ≡ n
*1 {zero} = refl
*1 {suc n}    -- (suc n * 1) ≡ suc n
              -- suc (n * 1) ≡ suc n
  rewrite
    *comm n 1 -- suc (n + 0) ≡ suc n
  | +0 n      -- suc  n      ≡ suc n
  = refl

--------------------------------------------------
-- properties of <, ≤, and =ℕ, iszero
--------------------------------------------------

0-≤ : ∀ (x : ℕ) → 0 ≤ x ≡ tt
0-≤ zero = refl
0-≤ (suc n)      -- (0 ≤ suc n) ≡ tt
  rewrite 0-≤ n  --          tt ≡ tt
  = refl

=ℕ-sym : ∀ (x y : ℕ) → (x =ℕ y) ≡ (y =ℕ x)
=ℕ-sym zero zero  = refl
=ℕ-sym (suc x) zero = refl -- (suc x =ℕ zero) ≡ (zero =ℕ suc x);  ff ≡ ff
=ℕ-sym zero (suc y) = refl -- (zero =ℕ suc y) ≡ (suc y =ℕ zero);  ff ≡ ff
=ℕ-sym (suc x) (suc y) rewrite =ℕ-sym x y = refl

=ℕ-suc : ∀ (x : ℕ) → suc x =ℕ x ≡ ff
=ℕ-suc zero = refl
=ℕ-suc (suc n) rewrite =ℕ-suc n = refl

<-suc : ∀ (n : ℕ) → n < suc n ≡ tt
<-suc zero = refl
<-suc (suc n) rewrite <-suc n = refl

-- TODO: understand
suc-inj : ∀ {n m : ℕ} → suc n ≡ suc m → n ≡ m
suc-inj         {0}  {0} _ = refl
suc-inj {m = suc m} refl   = refl

<=ℕff : ∀ (x : ℕ) → 0 < x ≡ tt → x =ℕ 0 ≡ ff
<=ℕff (suc x) _ = refl

<≤ : ∀ {n m : ℕ} → n < m ≡ tt → n ≤ m ≡ tt
<≤     {0}     {0} _ = refl
<≤ {suc _}     {0} ()
<≤     {0} {suc _} _ = refl
<≤ {suc n} {suc m} p rewrite <≤ {n} {m} p = refl

--------------------------------------------------
-- ordering properties of < and ≤ℕ
--------------------------------------------------

<-irrefl : ∀ (n : ℕ) → n < n ≡ ff
<-irrefl zero = refl
<-irrefl (suc n) rewrite <-irrefl n = refl

<-asym : ∀ {x y : ℕ}
       → x < y ≡ tt
       → y < x ≡ ff
<-asym {0}         {0} _ = refl
<-asym {0}     {suc _} _ = refl
<-asym {suc _}     {0}     ()
<-asym {suc x} {suc y} p = <-asym {x} {y} p

ℕ-trichotomy𝔹 : ∀ (n m : ℕ) → n < m || n =ℕ m || m < n ≡ tt
ℕ-trichotomy𝔹      0       0  = refl
ℕ-trichotomy𝔹      0  (suc m) = refl
ℕ-trichotomy𝔹 (suc n)      0  = refl
ℕ-trichotomy𝔹 (suc n) (suc m) = ℕ-trichotomy𝔹 n m

<≤-trans : ∀ {x y z : ℕ}
         → x < y ≡ tt
         → y ≤ z ≡ tt
         → x < z ≡ tt
<≤-trans     {x}     {0}         p1 _  rewrite <-0 x = 𝔹-contra p1
<≤-trans     {0} {suc y}     {0} _  ()
<≤-trans     {0} {suc y} {suc z} _  _  = refl
<≤-trans {suc x} {suc y}     {0} _  ()
<≤-trans {suc x} {suc y} {suc z} p1 p2 = <≤-trans {x} {y} {z} p1 p2

≤-refl : ∀ (x : ℕ) → x ≤ x ≡ tt
≤-refl zero = refl
≤-refl (suc x) rewrite ≤-refl x = refl

--------------------------------------------------
-- injectivity properties of addition
--------------------------------------------------

+inj1 : ∀ {x y z : ℕ}
      → x + y ≡ x + z
      →     y ≡     z
+inj1 {0} {y} {z} p = p
+inj1 {suc x} {y} {z} p = +inj1 {x} {y} {z} (suc-inj p)

+inj2 : ∀ {x y z : ℕ}
      → x + z ≡ y + z
      → x     ≡ y
+inj2 {x} {y} {z} p        -- p : (x + z) ≡ (y + z); Goal: x ≡ y
  rewrite
    +comm x z              -- p : (z + x) ≡ (y + z); Goal: x ≡ y
  | +comm y z              -- p : (z + x) ≡ (z + y); Goal: x ≡ y
  = +inj1 {z} {x} {y} p    -- NOTE: '+inj1'

------------------------------------------------------------------------------

-- 2

{- TODO
nat.agda

>, >= : defined in terms of < and <=

Prove versions of theorems like <-trans and <+
- modified to use _>_ instead of _<_

for practice writing out formulas in Agda
since the proofs can be written just to invoke the theorems dealing with _<_
-}

_>_ : ℕ → ℕ → 𝔹
a > b = b < a

_≥_ : ℕ → ℕ → 𝔹
a ≥ b = b ≤ a

<-trans2 : ∀ {x y z : ℕ}
         → y > x ≡ tt
         → z > y ≡ tt
         → z > x ≡ tt
<-trans2 {x} {y} {z} = <-trans {x} {y} {z}

------------------------------------------------------------------------------

-- 3a

f : (n : ℕ) → ℕ
f      0  = 1
f (suc x) = (suc x) * (f x)

f-is-factorial : f 5 ≡ 120
f-is-factorial = refl

-- 3b

f' : ℕ → 𝔹
fb : ℕ → 𝔹

-- is-odd
f'      0  = ff
f' (suc x) = fb x

-- is-even
fb      0  = tt
fb (suc x) = f' x

f'-is-odd : f' (suc 0) ≡ tt
f'-is-odd = refl

fb-is-even : fb (suc (suc 0)) ≡ tt
fb-is-even = refl
