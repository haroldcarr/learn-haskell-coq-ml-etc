module x01-842Naturals-hc-2 where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}

one : ℕ
one = suc zero

two : ℕ
two = suc (suc zero)

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))

_ : seven ≡ 7
_ = refl

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

_ : 2 + 3 ≡ 5
_ = refl

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩    -- is shorthand for
    (suc (suc zero)) + (suc (suc (suc zero)))
  ≡⟨⟩    -- many steps condensed
    5
  ∎

_*_ : ℕ → ℕ → ℕ
zero  * n = zero
suc m * n = n + (m * n)

_ =
  begin
    2 * 3
  ≡⟨⟩    -- many steps condensed
    6
  ∎

_ : 3 * 4 ≡ 12
_ = refl

_^_ : ℕ → ℕ → ℕ
m ^ zero  = suc zero
m ^ suc n = m * (m ^ n)

_ : 2 ^ 0 ≡ 1
_ = refl

_ : 2 ^ 1 ≡ 2
_ = refl

_ : 2 ^ 2 ≡ 4
_ = refl

_ : 2 ^ 3 ≡ 8
_ = refl

_ : 3 ^ 3 ≡ 27
_ = refl

_∸_ : ℕ → ℕ → ℕ
zero  ∸     n = zero
m     ∸  zero = m
suc m ∸ suc n = m ∸ n

_ : 3 ∸ 2 ≡ 1
_ = refl

_ : 2 ∸ 3 ≡ 0
_ = refl

infixl 6  _+_  _∸_
infixl 7  _*_

{-# BUILTIN NATPLUS  _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _∸_ #-}

-- Binary representation.

data Bin-ℕ : Set where
  bits : Bin-ℕ
  _x0 : Bin-ℕ → Bin-ℕ
  _x1 : Bin-ℕ → Bin-ℕ

-- Our representation of zero is different from PLFA.
-- We use the empty sequence of bits (more consistent).

bin-zero : Bin-ℕ
bin-zero = bits

bin-one : Bin-ℕ
bin-one = bits x1     -- 1 in binary

bin-two : Bin-ℕ
bin-two = bits x1 x0  -- 10 in binary

-- 842 exercise: Increment (1 point)
-- Define increment (add one).

inc : Bin-ℕ → Bin-ℕ
inc  bits  = bits x1
inc (m x0) = m    x1
inc (m x1) = (inc m) x0

_ : inc (bits) ≡ bits x1
_ = refl
_ : inc (bits          x1) ≡ bits       x1 x0
_ = refl
_ : inc (bits       x1 x0) ≡ bits       x1 x1
_ = refl
_ : inc (bits       x1 x1) ≡ bits    x1 x0 x0
_ = refl
_ : inc (bits    x1 x0 x0) ≡ bits    x1 x0 x1
_ = refl
_ : inc (bits    x1 x0 x1) ≡ bits    x1 x1 x0
_ = refl
_ : inc (bits    x1 x1 x0) ≡ bits    x1 x1 x1
_ = refl
_ : inc (bits    x1 x1 x1) ≡ bits x1 x0 x0 x0
_ = refl
_ : inc (bits x1 x0 x1 x1) ≡ bits x1 x1 x0 x0
_ = refl

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

_ : tob 6 ≡ bits x1 x1 x0
_ = refl

_ : fromb  bits           ≡ 0
_ = refl
_ : fromb (bits       x0) ≡ 0
_ = refl
_ : fromb (bits       x1) ≡ 1
_ = refl
_ : fromb (bits    x1 x0) ≡ 2
_ = refl
_ : fromb (bits    x1 x1) ≡ 3
_ = refl
_ : fromb (bits x1 x1 x0) ≡ 6
_ = refl
_ : fromb (bits x1 x1 x0) ≡ 6
_ = refl

-- Do NOT use 'to' and 'from'. Work with Bin-ℕ as if ℕ did not exist.
_bin-+_ : Bin-ℕ → Bin-ℕ → Bin-ℕ
bits   bin-+     n  =                    n
m      bin-+  bits  =            m
(m x0) bin-+ (n x0) =           (m bin-+ n) x0
(m x0) bin-+ (n x1) =      inc ((m bin-+ n) x0)
(m x1) bin-+ (n x0) =      inc ((m bin-+ n) x0)
(m x1) bin-+ (n x1) = inc (inc ((m bin-+ n) x0))

_ : (bits x1 x0 x0 x0) bin-+ (bits x1) ≡ (bits x1 x0 x0 x1)
_ = refl
_ : (bits x1 x0 x0 x1) bin-+ (bits x1) ≡ (bits x1 x0 x1 x0)
_ = refl
_ : (bits x1 x0 x0 x0) bin-+ (bits x1 x1 x1) ≡ (bits x1 x1 x1 x1)
_ = refl
_ : (tob 6) bin-+ (tob 2) ≡ (tob 8)
_ = refl
_ : (bits x1 x0) bin-+ (bits x1 x1) ≡ (bits x1 x0 x1)
_ = refl
