module z01-fp-with-booleans where

open import bool

{-
data 𝔹 : Set where
  tt : 𝔹
  ff : 𝔹

˜_ : 𝔹 → 𝔹
˜ tt = ff
˜ ff = tt

if_then_else_ : ∀ {ℓ} {A : Set ℓ} → 𝔹 → A → A → A
if tt then t else f = t
if ff then t else f = f
-}

-- load / type-check file
-- C-c, C-l

-- check type of expression
-- C-c, C-d ; then prompt for expression

------------------------------------------------------------------------------
-- p 24 1.8 Exercises

z01-1-8-01a : 𝔹
z01-1-8-01a = tt && (ff xor ~ ff)

z01-1-8-01b : 𝔹
z01-1-8-01b = ~ tt && (ff imp ff)

z01-1-8-01c : 𝔹
z01-1-8-01c = if tt xor tt then ff else ff

-- 1-8-4
data day : Set where
 mon : day
 tue : day
 wed : day
 thu : day
 fri : day
 sat : day
 sun : day

-- 1-8-5
nextday : day → day
nextday mon = tue
nextday tue = wed
nextday wed = thu
nextday thu = fri
nextday fri = sat
nextday sat = sun
nextday sun = mon

-- 1-8-6
data suit : Set where
  hearts   : suit
  spades   : suit
  diamonds : suit
  clubs    : suit

-- 1-8-7
is-red : suit → 𝔹
is-red hearts   = tt
is-red spades   = ff
is-red diamonds = tt
is-red clubs    = ff

