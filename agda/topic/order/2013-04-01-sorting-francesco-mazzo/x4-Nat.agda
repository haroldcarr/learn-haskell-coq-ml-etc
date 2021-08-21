open import x1-Base

module x4-Nat where

data ℕ : Set where
  zero :     ℕ
  suc  : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}

data _≤_ : Rel ℕ where
  z≤n : ∀ {x}   → zero ≤ x
  s≤s : ∀ {x y} →     x ≤     y
                → suc x ≤ suc y

≤-suc : ∀ {x y} → suc x ≤ suc y
                →     x ≤     y
≤-suc (s≤s x≤y) = x≤y

_≤?_ : Decidable _≤_
zero  ≤? _      = left  z≤n
suc _ ≤? zero   = right λ()
suc x ≤? suc y with x ≤? y
... | left  x≤y = left  (s≤s x≤y)
... | right x>y = right (λ sx≤sy → x>y (≤-suc sx≤sy))

open import x3-PropositionalEquality using (_≡_; refl; cong; equivalence)

antisym : ∀ {x y} → x ≤ y → y ≤ x → x ≡ y
antisym  z≤n       z≤n      = refl
antisym (s≤s x≤y) (s≤s y≤x) = cong suc (antisym x≤y y≤x)

trans : ∀ {x y z} → x ≤ y → y ≤ z → x ≤ z
trans  z≤n             _  = z≤n
trans (s≤s x≤y) (s≤s y≤z) = s≤s (trans x≤y y≤z)

total : ∀ x y → Either (x ≤ y) (y ≤ x)
total  zero        _ = left  z≤n
total (suc x)   zero = right z≤n
total (suc x) (suc y) with total x y
... | left  x≤y      = left  (s≤s x≤y)
... | right y≤x      = right (s≤s y≤x)

reflexive : ∀ {x y} → x ≡ y → x ≤ y
reflexive {zero}  refl = z≤n
reflexive {suc _} refl = s≤s (reflexive refl)

totalOrder : TotalOrder _≡_ _≤_
totalOrder = record
  { antisym     = antisym
  ; trans       = trans
  ; total       = total
  ; reflexive   = reflexive
  ; equivalence = equivalence
  }

