module x01 where

-- open import level

data 𝕓 : Set where
  tt : 𝕓
  ff : 𝕓

{-# BUILTIN BOOL  𝕓                 #-}
{-# BUILTIN TRUE  tt                #-}
{-# BUILTIN FALSE ff                #-}
-- {-# COMPILED_DATA 𝕓 Bool True False #-}

~_ : 𝕓 → 𝕓
~ tt = ff
~ ff = tt
infix 7 ~_


_&&_ : 𝕓 → 𝕓 → 𝕓
tt && b = b
ff && _ = ff
infix 6 _&&_

_||_ : 𝕓 → 𝕓 → 𝕓
tt || _ = tt
ff || b = b
infix 5 _||_

{-
infix 6 _xor_ _nand_
-}
{-
hcIte_ : ∀ (ℓ) (A : Set ℓ) → 𝕓 → A → A → A
hcIte tt t _ = t
hcIte ff _ f = f
-}
{-
if tt then (𝕓 → 𝕓) else 𝕓
-}
