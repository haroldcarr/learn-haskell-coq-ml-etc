module x02-842Induction-hc where

import Relation.Binary.PropositionalEquality as Eq
open   Eq using (_≡_; refl; cong; sym)
open   Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

-- associativity

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
    (3 + 4) + 5  ≡⟨⟩
         7  + 5  ≡⟨⟩
    12           ≡⟨⟩
    3 +  9       ≡⟨⟩
    3 + (4 + 5)
  ∎

+-identityᴸ : ∀ (m : ℕ) → zero + m ≡ m
+-identityᴸ m = refl -- via def/eq

+-identityʳ : ∀ (m : ℕ) → m + zero ≡ m
+-identityʳ   zero  = refl
+-identityʳ (suc m)         -- suc  m + zero  ≡ suc m
                            -- suc (m + zero) ≡ suc m
  rewrite +-identityʳ m     -- suc  m         ≡ suc m
  = refl

+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc   zero  n p = refl
+-assoc (suc m) n p         -- suc  m +  n + p   ≡ suc  m + (n + p)
                            -- suc (m +  n + p)  ≡ suc (m + (n + p))
  rewrite +-assoc m n p     -- suc (m + (n + p)) ≡ suc (m + (n + p))
  = refl

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc   zero  n = refl
+-suc (suc m) n             -- suc  (m + suc n)  ≡ suc (suc (m + n))
  rewrite +-suc m n         -- suc (suc (m + n)) ≡ suc (suc (m + n))
  = refl

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm zero n               -- zero + n          ≡ n + zero
                            --        n          ≡ n + zero
  rewrite +-identityʳ n     --        n          ≡ n
  = refl
+-comm (suc m)   zero       -- suc  m + zero     ≡ zero + suc m
                            -- suc (m + zero)    ≡ suc m
  rewrite +-identityʳ m     -- suc  m            ≡ suc m
  = refl
+-comm (suc m) (suc n)      -- suc  m + suc n    ≡ suc  n + suc m
                            -- suc (m + suc n)   ≡ suc (n + suc m)
  rewrite
    +-suc m n               -- suc (suc (m + n)) ≡ suc (n + suc m)
  | +-suc n m               -- suc (suc (m + n)) ≡ suc (suc (n + m))
  | +-comm m n              -- suc (suc (n + m)) ≡ suc (suc (n + m))
  = refl

-- without using induction/recursion
+-swap : ∀ (m n p : ℕ) → (m + n) + p ≡ n + (m + p)
+-swap   zero  n p = refl
+-swap (suc m) n p          -- suc  m +  n + p   ≡ n + (suc  m + p)
                            -- suc (m +  n + p)  ≡ n + suc  (m + p)
  rewrite
    +-suc n (m + p)         -- suc (m +  n + p)  ≡ suc (n + (m + p))
  | +-comm m n              -- suc (n +  m + p)  ≡ suc (n + (m + p))
  | +-assoc n m p           -- suc (n + (m + p)) ≡ suc (n + (m + p))
  = refl

-- addition distributes over multiplication
*-+-rdistrib : ∀ (m n p : ℕ) → (m + n) * p ≡ m * p + n * p
*-+-rdistrib   zero  n p = refl
*-+-rdistrib (suc m) n p       --     (suc m + n) * p ≡ suc m * p + n * p
                               --     p + (m + n) * p ≡ p + m * p + n * p
  rewrite
    *-+-rdistrib m n p         -- p + (m * p + n * p) ≡ p + m * p + n * p
  | +-assoc p (m * p) (n * p)  -- p + (m * p + n * p) ≡ p + (m * p + n * p)
  = refl

*-assoc : ∀ (m n p : ℕ) → (m * n) * p ≡ m * (n * p)
*-assoc   zero  n p = refl
*-assoc (suc m) n p            --       suc m * n * p ≡ suc m * (n * p)
                               --     (n + m * n) * p ≡ n * p + m * (n * p)
  rewrite
    *-+-rdistrib n (m * n) p   -- n * p + m *  n * p  ≡ n * p + m * (n * p)
  | *-assoc m n p              -- n * p + m * (n * p) ≡ n * p + m * (n * p)
  = refl

*0 : ∀ (n : ℕ) → n * zero ≡ zero
*0    zero = refl
*0 (suc n) = *0 n

*-suc : ∀ (m n : ℕ) → m * suc n ≡ m + (m * n)
*-suc   zero  n = refl
*-suc (suc m) n                --         suc m * suc n ≡ suc  m +  suc m * n
                               -- suc (n + m  * suc n)  ≡ suc (m + (n + m * n))
  rewrite
    *-suc m n                  -- suc (n + (m + m * n)) ≡ suc (m + (n + m * n))
  | sym (+-assoc n m (m * n))  -- suc (n +  m + m * n)  ≡ suc (m + (n + m * n))
  | +-comm n m                 -- suc (m +  n + m * n)  ≡ suc (m + (n + m * n))
  | +-assoc m n (m * n)        -- suc (m + (n + m * n)) ≡ suc (m + (n + m * n))
  = refl

*-comm : ∀ (m n : ℕ) → m * n ≡ n * m
*-comm   zero  n rewrite *0 n = refl
*-comm (suc m) n               -- suc m * n ≡ n * suc m
                               -- n + m * n ≡ n * suc m
  rewrite
    *-suc n m                  -- n + m * n ≡ n + n * m
  | *-comm m n                 -- n + n * m ≡ n + n * m
  = refl

0∸n≡0 : ∀ (m : ℕ) → zero ∸ m ≡ zero
0∸n≡0   zero  = refl
0∸n≡0 (suc m) rewrite 0∸n≡0 m = refl

-- form of associativity for monus
∸-+-assoc : ∀ (m n p : ℕ) → m ∸ n ∸ p ≡ m ∸ (n + p)
∸-+-assoc   zero     n    p    --  zero ∸ n ∸ p ≡ zero ∸ (n + p)
  rewrite
    0∸n≡0 n                    --         0 ∸ p ≡ 0    ∸ (n + p)
  | 0∸n≡0 p                    --             0 ≡ 0    ∸ (n + p)
  | 0∸n≡0 (n + p)              --             0 ≡ 0
  = refl
∸-+-assoc      m  zero    p    --   m      ∸ p  ≡ m ∸      p
  = refl
∸-+-assoc      m     n zero    --   m ∸  n      ≡ m ∸ (n + zero)
  rewrite +-identityʳ n        --   m ∸  n      ≡ m ∸  n
  = refl
∸-+-assoc (suc m) (suc n) p    --   m ∸  n ∸ p  ≡ m ∸ (n + p)
  rewrite ∸-+-assoc m n p      --   m ∸ (n + p) ≡ m ∸ (n + p)
  = refl

------------------------------------------------------------------------------
-- properties of binary representation

-- BEGIN copied from 842Naturals

data Bin-ℕ : Set where
  bits : Bin-ℕ
  _x0 : Bin-ℕ → Bin-ℕ
  _x1 : Bin-ℕ → Bin-ℕ

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc n) = suc (suc (dbl n))

inc : Bin-ℕ → Bin-ℕ
inc  bits  =  bits x1
inc (m x0) =     m x1
inc (m x1) = inc m x0

tob : ℕ → Bin-ℕ
tob   zero   = bits
tob (suc m)  = inc (tob m)

fromb : Bin-ℕ → ℕ
fromb  bits  = 0
fromb (n x0) =      dbl (fromb n)
fromb (n x1) = suc (dbl (fromb n))

_bin-+_ : Bin-ℕ → Bin-ℕ → Bin-ℕ
bl      bin-+   bits  =       bl
bits    bin-+  br     =                br
(bl x0) bin-+ (br x0) =      (bl bin-+ br) x0
(bl x0) bin-+ (br x1) =      (bl bin-+ br) x1
(bl x1) bin-+ (br x0) =      (bl bin-+ br) x1
(bl x1) bin-+ (br x1) = inc ((bl bin-+ br) x1)

-- END copied from 842Naturals

dblb : Bin-ℕ → Bin-ℕ
dblb b = b x0

_ : dblb (tob 3) ≡ tob  6
_ = refl
_ : dblb (tob 7) ≡ tob 14
_ = refl
_ : dblb (tob 9) ≡ tob 18
_ = refl

from∘inc : ∀ (b : Bin-ℕ) → fromb (inc b) ≡ suc (fromb b)
from∘inc  bits  = refl
from∘inc (b x0) = refl
from∘inc (b x1)            --          fromb (inc (b x1)) ≡      suc      (fromb (b x1))
                           --     dbl (fromb (inc  b))    ≡ suc (suc (dbl (fromb  b)))
  rewrite from∘inc b       -- suc (suc (dbl (fromb b)))   ≡ suc (suc (dbl (fromb  b)))
  = refl

from∘tob : ∀ (m : ℕ) → fromb (tob m) ≡ m
from∘tob   zero  = refl
from∘tob (suc m)           -- fromb (tob (suc m)) ≡ suc m
                           -- fromb (inc (tob m)) ≡ suc m
  rewrite
    from∘inc (tob m)       -- suc (fromb (tob m)) ≡ suc m
  | from∘tob      m        -- suc             m   ≡ suc m
  = refl

-- The property ∀ (m : Bin-ℕ) → tob (fromb m) ≡ m cannot be proved.
-- Because there are two representations of zero.
-- The following restriction can be proved:

to/from-corr : ∀ (b : Bin-ℕ) (n : ℕ) → b ≡ tob n → fromb b ≡ n --  TODO
to/from-corr b n b≡tn = {!!}

dblb∘inc : ∀ (b : Bin-ℕ) → dblb (inc b) ≡ inc (inc (dblb b))
dblb∘inc  bits  = refl
dblb∘inc (b x0)             -- dblb (inc (b x0)) ≡ inc (inc (dblb (b     x0)))
                            --      dblb (b x1)  ≡               ((b x1) x0)
  rewrite dblb∘inc b        --       ((b x1) x0) ≡               ((b x1) x0)
  = refl
dblb∘inc (b x1)             -- dblb (inc (b x1)) ≡ inc (inc (dblb (b x1)))
                            -- dblb (inc  b x0)  ≡    ((inc b x0) x0)
  rewrite dblb∘inc b        --   ((inc b x0) x0) ≡ ((inc b x0) x0)
  = refl
{-
-- more BIN properties to prove

-- not sure this can be proved because of multiple representations of zero
to∘dbl : ∀ (m : ℕ) → tob (dbl m) ≡ dblb (tob m) -- TODO -- not sure if provable because of zero
to∘dbl   zero               -- tob (dbl zero) ≡ dblb (tob zero)
                            --           bits ≡ dblb bits
  = {!!}
to∘dbl (suc m)              --       tob (dbl (suc m)) ≡ dblb (tob (suc m))
                            -- inc (inc (tob (dbl m))) ≡ dblb (inc (tob m))
  rewrite to∘dbl m          --     (inc (tob m) x0)    ≡ (inc (tob m) x0)
  = refl

from∘dblb : ∀ (b : Bin-ℕ) → fromb (dblb b) ≡ dbl (fromb b)
from∘dblb b = refl

-- bit version of second case for unary addition: suc m + n = suc (m + n)
-- useful in the next proof
-- hint: use induction on both parameters
bin-+-linc : ∀ (b1 b2 : Bin-ℕ) → (inc b1) bin-+ b2 ≡ inc (b1  bin-+ b2) -- TODO
bin-+-linc   b1   bits = refl
bin-+-linc bits (b2 x0) = {!!}
bin-+-linc bits (b2 x1) = {!!}
bin-+-linc (b1 x0) b2  = {!!}
bin-+-linc (b1 x1) b2  = {!!}


-- 842 exercise: PlusUnaryBinary (2 points)
-- This theorem relates unary and binary addition.

to∘+ : ∀ (m n : ℕ) → tob (m + n) ≡ tob m bin-+ tob n --  TODO
to∘+ m n = {!!}
-}

{-
The follwing theorems can be found in:
import Data.Nat.Properties using (+-assoc; +-identityʳ; +-suc; +-comm)

-- Unicode

    ∀  U+2200  FOR ALL (\forall, \all)
    ʳ  U+02B3  MODIFIER LETTER SMALL R (\^r)
    ′  U+2032  PRIME (\')
    ″  U+2033  DOUBLE PRIME (\')
    ‴  U+2034  TRIPLE PRIME (\')
    ⁗  U+2057  QUADRUPLE PRIME (\')
-}
