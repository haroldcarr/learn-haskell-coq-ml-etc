open import x1-Base

module x3-PropositionalEquality where

data _≡_ {X} : Rel X where
  refl : ∀ {x} → x ≡ x
{-
What propositional equality means.

DEFINITIONAL EQUALITY

To decide if two types/terms are "the same", it reduces them to their normal form,
then compares them syntactically, plus some additional laws.

Every term terminates, so refl : ((λ x → x) 1) ≡ 1 is acceptable

PROPOSITIONAL EQUALITY
User-level equality expressed by the inductive family defined.

Having a propositional equality in scope does not imply definitional equality for the
related terms, unless the propositional equality is a closed term.
That is the reason ≡ is not used from the beginning.
Instead, chose to parametrise the equality relation:
- sometimes propositional equality does not work, for example when working with functions.

In general there might be propositional equalities in scope
 that do not necessarily hold or involve abstracted variables, think of λ (p : 3 ≡ 1) → ....
-}
-- prove ≡ is an equivalence relation

sym : ∀ {X} {x y : X} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {X} {x y z : X} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

equivalence : ∀ {X} → Equivalence {X} _≡_
equivalence = record { refl = refl; sym = sym; trans = trans }

-- useful
cong : ∀ {X} {x y : X} → (f : X → X) → x ≡ y → f x ≡ f y
cong _ refl = refl
{-
Above uses pattern matching in a new way:

Since value of indices of ≡ depends on constructors,
matching on constructor refines context with new information.

E.g., sym matching refl will unify y and x,
turning them into the same variable in the context for the body of sym,
therefore letting refl be invoked again.

DEPENDENT PATTERN MATCHING
Pattern matching in Agda can change the context AND constraint possible constructors
 of other parameters, if they are a type with indices and those indices have been refined.
-}
