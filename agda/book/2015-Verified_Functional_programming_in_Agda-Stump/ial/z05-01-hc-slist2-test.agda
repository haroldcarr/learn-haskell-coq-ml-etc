open import bool
open import eq using (_≡_)
open import nat
open import nat-thms
open import z05-01-hc-slist2-base

module z05-01-hc-slist2-test where

data _R≤_ : ℕ → ℕ → Set where

  z≤n : ∀ {n : ℕ}
      --------
    → zero R≤ n

  s≤s : ∀ {m n : ℕ}
    →     m R≤     n
      -------------
    → suc m R≤ suc n

≤-pred : ∀ {m n} → suc m R≤ suc n → m R≤ n
≤-pred (s≤s m≤n) = m≤n

_≤?_ : Decidable _R≤_
zero  ≤? _     = left z≤n
suc m ≤? zero  = right λ()
suc m ≤? suc n with m ≤? n
... | left  m≤n = left  (s≤s m≤n)
... | right m≰n = right λ x → m≰n (≤-pred x)

ℕ-Equivalence : Equivalence {X = ℕ} _≡_
ℕ-Equivalence = record
  { refl  = _≡_.refl
  ; sym   = λ x≡y → eq.sym x≡y
  ; trans = λ {_≡_.refl _≡_.refl → eq.trans _≡_.refl _≡_.refl} }

xR≤y→x≤y≡tt : ∀ {x y : ℕ} → x R≤ y → x ≤ y ≡ bool.𝔹.tt
xR≤y→x≤y≡tt {x} {y}  z≤n       = 0-≤ y
xR≤y→x≤y≡tt {x} {y} (s≤s xR≤y) = xR≤y→x≤y≡tt xR≤y

x≤y≡tt→xR≤y : ∀ {x y : ℕ} → x ≤ y ≡ bool.𝔹.tt → x R≤ y
x≤y≡tt→xR≤y  {zero} {_}         _ = z≤n
x≤y≡tt→xR≤y {suc x} {y} sucx≤y≡tt = {!!} -- TODO

R≤-total : (x y : ℕ) → Either (x R≤ y) (y R≤ x)
R≤-total zero     zero   = left z≤n
R≤-total zero    (suc y) = left z≤n
R≤-total (suc x)  zero   = right z≤n
R≤-total (suc x) (suc y) with R≤-total x y
... | left  l = left  (s≤s l)
... | right r = right (s≤s r)

x≡y→xR≤y : ∀ {x y : ℕ} → x ≡ y → x R≤ y
x≡y→xR≤y  {zero}        {_}        _ = z≤n
x≡y→xR≤y {suc x} {.(suc x)} _≡_.refl = s≤s (x≤y≡tt→xR≤y (≤-refl x))

ℕ-TotalOrder : TotalOrder {X = ℕ} _≡_ _R≤_
ℕ-TotalOrder = record
  { antisym     = λ x≤y y≤x → ≤-antisym (xR≤y→x≤y≡tt x≤y) (xR≤y→x≤y≡tt y≤x)
  ; trans       = λ {x} {y} {z} xR≤y yR≤z →
                    x≤y≡tt→xR≤y (≤-trans {x} {y} {z} (xR≤y→x≤y≡tt xR≤y) (xR≤y→x≤y≡tt yR≤z))
  ; total       = R≤-total
  ; reflexive   = x≡y→xR≤y
  ; equivalence = ℕ-Equivalence
  }

import z05-01-hc-slist2 as SL
open   SL {ℕ}
          {_≡_}
          {_R≤_}
          _≤?_
          ℕ-TotalOrder

empty : OList ⟦ 0 ⟧ ⟦ 9 ⟧
empty = nil (≤-lift z≤n)

l-9 : OList ⟦ 0 ⟧ ⟦ 9 ⟧
l-9 = insert 9 empty (≤-lift z≤n) (≤-lift (s≤s (s≤s (s≤s (s≤s (s≤s (s≤s (s≤s (s≤s (s≤s z≤n))))))))))

l-5-9 : OList ⟦ 0 ⟧ ⟦ 9 ⟧
l-5-9 = insert 5 l-9 (≤-lift z≤n) (≤-lift (s≤s (s≤s (s≤s (s≤s (s≤s z≤n))))))

l-1-5-9 : OList ⟦ 0 ⟧ ⟦ 9 ⟧
l-1-5-9 = insert 1 l-5-9 (≤-lift z≤n) (≤-lift (s≤s z≤n))

l-1-5-9' : OList ⊥ ⊤
l-1-5-9' = isort' (9 ∷ 1 ∷ 5 ∷ [])

_ : toList l-1-5-9 ≡ toList l-1-5-9'
_ = eq.refl

{-

_ : bst-search 0 empty ≡ nothing
_ = refl

_ : bst-insert 5 empty ≡ bst-node 5 (bst-leaf refl) (bst-leaf refl) refl refl
_ = refl

bst-5 : bst 0 5
bst-5 = bst-insert 5 empty

_ : maybe (Σ ℕ (λ z → (5 < z || 5 =ℕ z) && (z < 5 || z =ℕ 5) ≡ tt))
_ = bst-search 5 bst-5

_ : bst-insert 9 bst-5 ≡ bst-node 5 (bst-leaf refl)
                           (bst-node 9 (bst-leaf refl) (bst-leaf refl) refl refl) refl refl
_ = refl

_ : bst 0 9
_ = bst-insert 9 bst-5

_ : bst 0 5
_ = bst-dec-lb {0} {0} {5} (bst-leaf refl) refl
-}
