module x01-842Naturals-hc where

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

one : ℕ
one = suc zero

two : ℕ
two = suc (suc zero)

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))

{-# BUILTIN NATURAL ℕ #-}

import Relation.Binary.PropositionalEquality as Eq
open Eq             using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

_ : 2 + 3 ≡ 5
_ = refl

_ : 2 + 3 ≡ 5
_ =
  begin
    2                + 3                      ≡⟨⟩    -- is shorthand for
    (suc (suc zero)) + (suc (suc (suc zero))) ≡⟨⟩    -- many steps condensed
    5
  ∎

_*_ : ℕ → ℕ → ℕ
zero  * n = 0
suc m * n = n + (m * n)

_ =
  begin
    2 * 3 ≡⟨⟩    -- many steps condensed
    6
  ∎

_^_ : ℕ → ℕ → ℕ
m ^  zero = 1
m ^ suc n = m * (m ^ n)

_ : 2 ^ 3 ≡ 8
_ = refl
_ : 2 ^ 4 ≡ 16
_ = refl
_ : 3 ^ 3 ≡ 27
_ = refl
_ : 3 ^ 10 ≡ 59049
_ = refl

-- Monus (subtraction for naturals, bottoms out at zero).

_∸_ : ℕ → ℕ → ℕ
zero  ∸     n = zero
m     ∸  zero = m
suc m ∸ suc n = m ∸ n

_ =
  begin
     3 ∸ 2
  ≡⟨⟩ -- many steps condensed
     1
  ∎

_ =
  begin
     2 ∸ 3
  ≡⟨⟩ -- many steps condensed
     0
  ∎

_ = begin 5 ∸ 3 ≡⟨⟩ 2 ∎
_ = begin 3 ∸ 5 ≡⟨⟩ 0 ∎

infixl 6  _+_  _∸_
infixl 7  _*_

-- {-# BUILTIN NATPLUS  _+_ #-}
-- {-# BUILTIN NATTIMES _*_ #-}
-- {-# BUILTIN NATMINUS _∸_ #-}

-- Binary representation.
-- Modified from PLFA exercise (thanks to David Darais).

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

inc : Bin-ℕ → Bin-ℕ
inc  bits  =  bits x1
inc (m x0) =     m x1
inc (m x1) = inc m x0

_ : inc (bits x1 x0 x1 x0) ≡    bits x1 x0 x1 x1
_ = refl
_ : inc (bits x1 x0 x1 x1) ≡    bits x1 x1 x0 x0
_ = refl
_ : inc (bits x1 x1 x1 x1) ≡ bits x1 x0 x0 x0 x0
_ = refl

-- 842 exercise: To/From (2 points)
-- Hint: avoid addition and multiplication. Use the provided dbl.

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc m) = suc (suc (dbl m))

tob : ℕ → Bin-ℕ
tob   zero   = bits
tob (suc m)  = inc (tob m)

fromb : Bin-ℕ → ℕ
fromb  bits  = 0
fromb (n x0) =      dbl (fromb n)
fromb (n x1) = suc (dbl (fromb n))

_ : tob 6 ≡    bits x1 x1 x0
_ = refl
_ : tob 7 ≡    bits x1 x1 x1
_ = refl
_ : tob 8 ≡ bits x1 x0 x0 x0
_ = refl

_ : fromb    (bits x1 x1 x0) ≡ 6
_ = refl
_ : fromb    (bits x1 x1 x1) ≡ 7
_ = refl
_ : fromb (bits x1 x0 x0 x0) ≡ 8
_ = refl

-- 842 exercise: BinAdd (2 points)
-- Write the addition function for binary notation.
-- Do NOT use 'to' and 'from'. Work with Bin-ℕ as if ℕ did not exist.
-- Hint: use recursion on both m and n.

_bin-+_ : Bin-ℕ → Bin-ℕ → Bin-ℕ
m      bin-+ bits   =       m
bits   bin-+  n     =               n
(m x0) bin-+ (n x0) =      (m bin-+ n) x0
(m x0) bin-+ (n x1) =      (m bin-+ n) x1
(m x1) bin-+ (n x0) =      (m bin-+ n) x1
(m x1) bin-+ (n x1) = inc ((m bin-+ n) x1)

-- Tests can use to/from, or write out binary constants as below.
-- Again: write more tests!

_ : (bits x1 x0) bin-+ (bits x1 x1) ≡ (bits x1 x0 x1)
_ = refl

_ : tob 0 bin-+ tob 0 ≡ tob 0
_ = refl

_ : tob 7 bin-+ tob 7 ≡ tob 14
_ = refl

_ : tob 3 bin-+ tob 4 ≡ tob 7
_ = refl

{-
-- Many definitions from above are also in the standard library.
-- open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_; _∸_)

-- Unicode used in this chapter:

    ℕ  U+2115  DOUBLE-STRUCK CAPITAL N (\bN)
    →  U+2192  RIGHTWARDS ARROW (\to, \r, \->)
    ∸  U+2238  DOT MINUS (\.-)
    ≡  U+2261  IDENTICAL TO (\==)
    ⟨  U+27E8  MATHEMATICAL LEFT ANGLE BRACKET (\<)
    ⟩  U+27E9  MATHEMATICAL RIGHT ANGLE BRACKET (\>)
    ∎  U+220E  END OF PROOF (\qed)
-}
