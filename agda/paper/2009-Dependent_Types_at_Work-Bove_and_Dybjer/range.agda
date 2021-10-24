-- https://stackoverflow.com/questions/61037572/how-to-define-the-range-function-on-a-relation-in-agda-set-theory

module range where

open import Data.Unit
open import Data.Product renaming (_,_ to ⟨_,_⟩)
open import Data.Sum
open import Function

Subset : Set → Set₁
Subset A = A → Set

_∈_ : ∀ {A} → A → Subset A → Set
a ∈ P = P a

Relation : ∀ A B → Set₁
Relation A B = Subset (A × B)

Range : ∀ {A B} → Relation A B → Subset B
Range R b = ∃ (R ∘ ⟨_, b ⟩)  -- equivalent to ∃ \a → R ⟨ a , b ⟩

_⊆_ : ∀ {A} → Subset A → Subset A → Set
A ⊆ B = ∀ x → x ∈ A → x ∈ B

wholeSet : ∀ A → Subset A
wholeSet _ _ = ⊤

∀subset⊆set : ∀ {A sub} → sub ⊆ wholeSet A
∀subset⊆set _ _ = tt

_∩_ : ∀ {A} → Subset A → Subset A → Subset A
(A ∩ B) x = x ∈ A × x ∈ B


open import Data.Nat

x : Set₁
x = Subset ℕ

y : Set₁
y = Relation ℕ ℕ

z : Subset ℕ
z = Range {ℕ} λ { ⟨ n1 , n2 ⟩ → (x₁ : Σ ℕ (λ _ → ℕ)) → {!!} ∈ {!!}}

