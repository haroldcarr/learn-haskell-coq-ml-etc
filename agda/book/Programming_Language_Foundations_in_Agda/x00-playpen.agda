module x00-playpen where

-- prove properties of inductive naturals and operations on them via induction

import      Relation.Binary.PropositionalEquality as Eq
open        Eq             using (_≡_; refl; cong; sym)
open        Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat       using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)

*-assoc : ∀ (m n p : ℕ)
        → (m *  n) * p
        ≡  m * (n  * p)
*-assoc m n p = {!!}

+-xx : ∀ (n  : ℕ)
       → (n ^ 3) ∸ ((n * n) * n)
       ≡ zero
+-xx zero = refl
+-xx (suc n) = {!!}
{-
  begin
              (n * (n ^ 2))   ∸ ((n * n) * n)
  ≡⟨⟩
         (n * (n * (n ^ 1)))  ∸ ((n * n) * n)
  ≡⟨⟩
    (n * (n * (n * (n ^ 0)))) ∸ ((n * n) * n)
  ≡⟨⟩
    (n * (n * (n *  1     ))) ∸ ((n * n) * n)
  ≡⟨⟩
    (n * (n *  n          ))  ∸ ((n * n) * n)
  ≡⟨ cong ( ((n * (n *  n))) ∸_) (*-assoc n n n) ⟩
    (n * (n *  n          ))  ∸ ( n * (n * n))
  ∎
-}
