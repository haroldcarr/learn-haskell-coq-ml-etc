module x02-842Induction-hc-2 where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

-- PLFA coverage of identity, associativity, commutativity, distributivity.

-- + associative
_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
  (3 + 4) + 5 ≡⟨⟩
       7  + 5 ≡⟨⟩
           12 ≡⟨⟩
   3 +  9     ≡⟨⟩
   3 + (4 + 5)
  ∎

+-identityᴸ : ∀ (m : ℕ) → zero + m ≡ m
+-identityᴸ m = refl

0+ : ∀ (m : ℕ) → zero + m ≡ m
0+ = +-identityᴸ

+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ  zero   = refl
+-identityʳ (suc m) = cong suc (+-identityʳ m)

+0 : ∀ (m : ℕ) → m + zero ≡ m
+0 = +-identityʳ

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc  zero   n p = refl
+-assoc (suc m) n p = cong suc (+-assoc m n p)

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc  zero   n = refl
+-suc (suc m) n = cong suc (+-suc m n)

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm  zero   n          --   zero + n  ≡ n + zero
                          --          n  ≡ n + zero
  rewrite +-identityʳ n   --          n  ≡ n
  = refl
+-comm (suc m) n          -- suc  m + n  ≡ n + suc m
                          -- suc (m + n) ≡ n + suc m
  rewrite +-suc n m       -- suc (m + n) ≡ suc (n + m)
  | cong suc (+-comm m n) -- suc (n + m) ≡ suc (n + m)
  = refl

-- without using induction/recursion
+-swap : ∀ (m n p : ℕ) → (m + n) + p ≡ n + (m + p)
+-swap  zero   n p = refl
+-swap (suc m) n p      -- suc  m + n + p  ≡ n + (suc m + p)
                        -- suc (m + n + p) ≡ n + suc (m + p)
  rewrite
    +-suc n (m + p)     -- suc (m + n + p) ≡ suc (n + (m + p))
  | sym (+-assoc n m p) -- suc (m + n + p) ≡ suc (n +  m + p)
  | +-comm m n          -- suc (n + m + p) ≡ suc (n +  m + p)
  = refl

*-+-rdistrib : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
*-+-rdistrib  zero   n p = refl
*-+-rdistrib (suc m) n p       -- (suc m + n)    * p  ≡ suc  m * p + n * p
                               -- p + (m + n)    * p  ≡ p +  m * p + n * p
  rewrite
    *-+-rdistrib m n p         -- p + (m * p + n * p) ≡ p +  m * p + n * p
  | +-assoc p (m * p) (n * p)  -- p + (m * p + n * p) ≡ p + (m * p + n * p)
  = refl

*-assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc  zero   n p = refl
*-assoc (suc m) n p            -- suc  m * n    * p ≡     suc m * (n * p)
                               -- (n + m * n)   * p ≡ n * p + m * (n * p)
  rewrite
    *-+-rdistrib n (m * n) p   -- n * p + m * n * p ≡ n * p + m * (n * p)
  | sym (*-assoc m n p)        -- n * p + m * n * p ≡ n * p + m *  n * p
  = refl

*0 : ∀ (m : ℕ) → m * zero ≡ zero
*0  zero   = refl
*0 (suc m) = *0 m

*1 : ∀ (m : ℕ) → m * suc zero ≡ m
*1  zero   = refl
*1 (suc m) = cong suc (*1 m)

-- like addition, helper lemmas needed
*-comm : ∀ (m n : ℕ) → m * n ≡ n * m
*-comm  zero   n rewrite *0 n = refl
*-comm  m   zero rewrite *0 m = refl
*-comm (suc m) (suc n)        -- suc       m * suc n   ≡ suc      n * suc m
                              -- suc (n +  m * suc n)  ≡ suc (m + n * suc m)
  rewrite
    *-comm m (suc n)          -- suc (n + (m + n * m)) ≡ suc (m +  n * suc m)
  | *-comm n (suc m)          -- suc (n + (m + n * m)) ≡ suc (m + (n + m * n))
  | sym (+-assoc n m (n * m)) -- suc (n +  m + n * m)  ≡ suc (m + (n + m * n))
  | sym (+-assoc m n (m * n)) -- suc (n +  m + n * m)  ≡ suc (m +  n + m * n)
  |      +-swap  n m (n * m)  -- suc (m + (n + n * m)) ≡ suc (m +  n + m * n)
  | sym (+-assoc m n (n * m)) -- suc (m +  n + n * m)  ≡ suc (m +  n + m * n)
  | *-comm m n                -- suc (m +  n + n * m)  ≡ suc (m +  n + n * m)
  = refl

0∸n≡0 : ∀ (m : ℕ) → zero ∸ m ≡ zero
0∸n≡0  zero   = refl
0∸n≡0 (suc m) rewrite 0∸n≡0 m = refl

m∸0≡m : ∀ (m : ℕ) → m ∸ zero ≡ m
m∸0≡m m = refl

∸-+-assoc : ∀ (m n p : ℕ) → m ∸ n ∸ p ≡ m ∸ (n + p)
∸-+-assoc  zero        n       p      -- zero ∸ n ∸ p  ≡ zero ∸ (n + p)
  rewrite
    0∸n≡0 (n + p)                     -- 0 ∸ n ∸ p     ≡ 0
  | 0∸n≡0 n                           -- 0     ∸ p     ≡ 0
  | 0∸n≡0 p                           -- 0             ≡ 0
  = refl
∸-+-assoc      m    zero       p
  = refl
∸-+-assoc      m       n    zero      -- m ∸ n ∸ zero  ≡ m ∸ (n + zero)
  rewrite
    +0 n                              -- m ∸ n         ≡ m ∸  n
  = refl
∸-+-assoc (suc m) (suc n) (suc p)     -- m ∸ n ∸ suc p ≡ m ∸ (n + suc p)
  rewrite sym (∸-+-assoc m n (suc p)) -- m ∸ n ∸ suc p ≡ m ∸  n ∸ suc p
  = refl

------------------------------------------------------------------------------
-- Properties of binary representation.

-----
-- BEGIN : copied from x01-842Naturals-hc-2.agda

data Bin-ℕ : Set where
  bits : Bin-ℕ
  _x0 : Bin-ℕ → Bin-ℕ
  _x1 : Bin-ℕ → Bin-ℕ
bin-zero : Bin-ℕ
bin-zero = bits

bin-one : Bin-ℕ
bin-one = bits x1     -- 1 in binary

bin-two : Bin-ℕ
bin-two = bits x1 x0  -- 10 in binary

inc : Bin-ℕ → Bin-ℕ
inc  bits  = bits x1
inc (m x0) = m    x1
inc (m x1) = (inc m) x0

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc m) = suc (suc (dbl m))

tob : ℕ → Bin-ℕ
tob  zero   = bits
tob (suc m) = inc (tob m)

fromb : Bin-ℕ → ℕ
fromb  bits  = zero
fromb (n x0) =      dbl (fromb n)
fromb (n x1) = suc (dbl (fromb n))

_bin-+_ : Bin-ℕ → Bin-ℕ → Bin-ℕ
bits   bin-+     n  =                    n
m      bin-+  bits  =            m
(m x0) bin-+ (n x0) =           (m bin-+ n) x0
(m x0) bin-+ (n x1) =      inc ((m bin-+ n) x0)
(m x1) bin-+ (n x0) =      inc ((m bin-+ n) x0)
(m x1) bin-+ (n x1) = inc (inc ((m bin-+ n) x0))

-- END : copied from x01-842Naturals-hc-2.agda
-----

-- Bin-ℕ version of dbl. Do not use tob/fromb.

dblb : Bin-ℕ → Bin-ℕ
dblb  bits  = bits
dblb (m x0) =           (dblb m) x0
dblb (m x1) = inc (inc ((dblb m) x0))

_ : dblb  bits ≡ bits
_ = refl
_ : dblb (bits       x0) ≡ bits          x0
_ = refl
_ : dblb (bits       x1) ≡ bits       x1 x0
_ = refl
_ : dblb (bits    x1 x0) ≡ bits    x1 x0 x0
_ = refl
_ : dblb (bits    x1 x1) ≡ bits    x1 x1 x0
_ = refl
_ : dblb (bits x1 x0 x0) ≡ bits x1 x0 x0 x0
_ = refl
_ : dblb (bits x1 x0 x1) ≡ bits x1 x0 x1 x0
_ = refl

from∘inc : ∀ (m : Bin-ℕ) → fromb (inc m) ≡ suc (fromb m)
from∘inc  bits  = refl
from∘inc (m x0) = refl
from∘inc (m x1) --       fromb (inc (m x1))  ≡ suc (fromb (m x1))
                --      dbl (fromb (inc m))  ≡ suc (suc (dbl (fromb m)))
  rewrite
    from∘inc m  --      dbl (suc (fromb m))  ≡ suc (suc (dbl (fromb m)))
                -- suc (suc (dbl (fromb m))) ≡ suc (suc (dbl (fromb m)))
  = refl

from∘tob : ∀ (m : ℕ) → fromb (tob m) ≡ m
from∘tob  zero   = refl
from∘tob (suc m)     -- fromb (tob (suc m)) ≡ suc m
                     -- fromb (inc (tob m)) ≡ suc m
  rewrite
    from∘inc (tob m) -- suc (fromb (tob m)) ≡ suc m
  | from∘tob m       -- suc             m   ≡ suc m
  = refl

-- ∀ (m : Bin-ℕ) → tob (fromb m) ≡ m cannot be proved.
-- Because there are multiple representations of ZERO.
-- This restriction can be proved.

to/from-corr : ∀ (b : Bin-ℕ) (n : ℕ) → b ≡ tob n → fromb b ≡ n
to/from-corr .bits  zero   refl = refl
to/from-corr .(inc (tob n)) (suc n) refl -- fromb (tob (suc n)) ≡ suc n
                                         -- fromb (inc (tob n)) ≡ suc n
  rewrite
    from∘inc (tob n)                     -- suc (fromb (tob n)) ≡ suc n
  | to/from-corr (tob n) n refl          -- suc             n   ≡ suc n
  = refl

dblb∘inc : ∀ (b : Bin-ℕ) → dblb (inc b) ≡ inc (inc (dblb b))
dblb∘inc  bits  = refl
dblb∘inc (b x0) = refl
dblb∘inc (b x1)  --       dblb (inc (b   x1)) ≡  inc (inc (dblb (b x1)))
                 --      (dblb (inc  b)  x0)  ≡ (inc (inc (dblb  b)) x0)
  rewrite
    dblb∘inc b   --  (inc (inc (dblb b)) x0)  ≡ (inc (inc (dblb  b)) x0)
  = refl

to∘dbl : ∀ (m : ℕ) → tob (dbl m) ≡ dblb (tob m)
to∘dbl  zero   = refl
to∘dbl (suc m)       --      tob (dbl  (suc m))  ≡      dblb (tob (suc m))
                     -- inc (inc (tob  (dbl m))) ≡      dblb (inc (tob m))
  rewrite
    to∘dbl m         -- inc (inc (dblb (tob m))) ≡      dblb (inc (tob m))
  | dblb∘inc (tob m) -- inc (inc (dblb (tob m))) ≡ inc (inc (dblb (tob m)))
  = refl

from∘dblb : ∀ (b : Bin-ℕ) → fromb (dblb b) ≡ dbl (fromb b)
from∘dblb  bits  = refl
from∘dblb (b x0)      --             fromb (dblb (b x0)) ≡ dbl (fromb (b x0))
                      --            dbl (fromb (dblb b)) ≡ dbl (dbl (fromb b))
  rewrite
    from∘dblb b       --            dbl  (dbl (fromb b)) ≡ dbl (dbl (fromb b))
  = refl
from∘dblb (b x1)      --             fromb (dblb (b x1)) ≡ dbl (fromb (b x1))
                      --     dbl (fromb (inc (dblb b)))  ≡ suc (suc (dbl (dbl (fromb b))))
  rewrite
    from∘inc (dblb b) --     dbl (suc (fromb (dblb b)))  ≡ suc (suc (dbl (dbl (fromb b))))
  | from∘dblb b       -- suc (suc (dbl (dbl (fromb b)))) ≡ suc (suc (dbl (dbl (fromb b))))
  = refl

-- lemma
-- second case for unary addition
--     suc m + n = suc (m + n)
-- into binary setting.
-- Induction on both m and n is needed.

bin-+-linc : ∀ (m n : Bin-ℕ) → (inc m) bin-+ n ≡ inc (m bin-+ n)
bin-+-linc bits    bits  = refl
bin-+-linc bits   (n x0) = refl
bin-+-linc bits   (n x1) = refl
bin-+-linc (m x0)  bits  = refl
bin-+-linc (m x0) (n x0) = refl
bin-+-linc (m x0) (n x1) = refl
bin-+-linc (m x1)  bits  = refl
bin-+-linc (m x1) (n x0) rewrite bin-+-linc m n = refl
bin-+-linc (m x1) (n x1) rewrite bin-+-linc m n = refl

-- theorem relates unary and binary addition
to∘+ : ∀ (m n : ℕ) → tob (m + n) ≡ tob m bin-+ tob n
to∘+  zero   n = refl
to∘+ (suc m) n                 --  tob (suc m + n)        ≡ (tob (suc m) bin-+ tob n)
                               -- inc (tob (m + n))       ≡ (inc (tob m) bin-+ tob n)
  rewrite
    bin-+-linc (tob m) (tob n) -- inc (tob (m + n))       ≡ inc (tob m bin-+ tob n)
  | to∘+ m n                   -- inc (tob m bin-+ tob n) ≡ inc (tob m bin-+ tob n)
  = refl
