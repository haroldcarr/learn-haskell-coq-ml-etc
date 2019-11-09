-- Give a natural deduction proof of 𝐴∧𝐵 from hypothesis 𝐵∧𝐴.
example (A B : Prop) : B ∧ A → A ∧ B := λ (h : B ∧ A), and.intro h.right h.left

-- Give a natural deduction proof of (𝑄→𝑅)→𝑅 from hypothesis 𝑄.
example (Q R : Prop) : Q ∧ (Q → R) → R := λ (h₁ : Q ∧ (Q → R)), h₁.right h₁.left

