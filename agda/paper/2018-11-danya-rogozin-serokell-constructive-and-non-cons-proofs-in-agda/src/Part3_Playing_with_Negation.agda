{-

https://serokell.io/blog/playing-with-negation

Constructive and Non-Constructive Proofs in Agda (Part 3): Playing with Negation

Danya Rogozin Friday, November 30th, 2018


Present an empty type to work with constructive negation.
Discuss Markov’s principle and apply this principle for one use case.
Declare the double negation elimination as a postulate.
Some examples of non-constructive proofs in Agda.

------------------------------------------------------------------------------
Empty type

-}
-- Type with no constructors (so-called bottom).
-- Type that corresponds to an absurd statement or contradiction.
data ⊥ : Set where

-- ¬ defines negation. ¬A denotes that any proof of A yields a proof of a contradiction:

¬_ : Set → Set
¬ A = A → ⊥

-- elimination of ⊥ : derives an arbitrary statement from bottom
exFalso : {A : Set} → ⊥ → A
exFalso ()

-- examples of propositions with negation that are provable constructively.

-- Derives an arbitrary formula from a contradiction.
-- The first argument f has a type ¬ A (or A → ⊥).
-- The second argument x has a type A.
-- Thus f x is an object of type ⊥.
-- Hence exContr (f x) has a type B.
exContr : {A B : Set} → ¬ A → A → B
exContr f x = exFalso (f x)

exContr2 : {A B : Set} → (A → ⊥) → A → B
exContr2 f x = exFalso (f x)

-- g proves B → ⊥
-- f proves A → B
-- g ∘ f proves A → ⊥, i.e. ¬ A.
contraposition : {A B : Set} → (A → B) → (¬ B → ¬ A)
contraposition f g = g ∘ f

-- negation on formula A.
-- If possible to derive contradictionary consequences B and ¬ B from the statement A,
-- then any proof of A yields ⊥.
¬-intro : {A B : Set} → (A → B) → (A → ¬ B) → ¬ A
¬-intro f g x = g x (f x)

{-
disjImpl establishes a connection between disjunction and implication.
If prove ¬ A ∨ B and have a proof of A.
If have a proof of ¬ A, then we have proved B by ex-falso.
If have a proof of B, then we have already proved B trivially.
So, we have obtained a proof of B by case analysis (i.e., pattern-matching).
-}
disjImpl : {A B : Set} → ¬ A ⊎ B → A → B
disjImpl (inj₁ x) a = exContr x a
disjImpl (inj₂ y) a = y






