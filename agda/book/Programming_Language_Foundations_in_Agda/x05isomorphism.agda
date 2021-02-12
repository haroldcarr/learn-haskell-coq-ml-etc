module x05isomorphism where
{-
------------------------------------------------------------------------------
Isomorphism: Isomorphism and Embedding

isomorphism
- way of asserting that two types are equal

embedding
- way of asserting that one type is smaller than another

apply isomorphisms in next chapter to show ops product and sum satisfy properties
similar to associativity, commutativity, and distributivity
-}

import Relation.Binary.PropositionalEquality as Eq
open        Eq                  using (_≡_; refl; cong; cong-app)
open        Eq.≡-Reasoning
open import Data.Nat            using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm)
{-
------------------------------------------------------------------------------
Lambda expressions : anonymous funs

λ{ P₁ → N₁; ⋯ ; Ppₙ → Np }

equivalent def by equations

f P₁ = N₁
⋯
f Pp = Npₙ

where
- P are patterns    (left-hand sides of an equation)
- N are expressions (right-hand side of an equation)

for case of one equation and pattern is a variable:

λ x → N

or

λ (x : A) → N

equivalent to

λ{x → N}

------------------------------------------------------------------------------
Function composition
-}

-- first apply f, then apply g to result
_∘_ : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
(g ∘ f) x  = g (f x)


-- equivalent
_∘′_ : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
g ∘′ f  =  λ x → g (f x)

{-
------------------------------------------------------------------------------
Extensionality : only way to distinguish functions is by applying them

If two functions applied to the same arg always yield same result, then they are same.

converse of cong-app

Agda does not presume extensionality, so
-}

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    →              f   ≡ g

{-
Postulating extensionality does not lead to difficulties.
It is known to be consistent with the theory that underlies Agda.

example

need results from two libraries
- one where addition is defined (Chapter Naturals)
- one where it is defined the other way around
-}

_+′_ : ℕ → ℕ → ℕ
m +′ zero  = m
m +′ suc n = suc (m +′ n)

-- applying commutativity, can show both operators always return same result for same arguments:
same-app : ∀ (m n : ℕ) → m +′ n ≡ m + n
same-app m n                --     (m +′ n) ≡      m + n
  rewrite +-comm m n        --     (m +′ n) ≡      n + m
  = helper m n
 where
  helper : ∀ (m n : ℕ) → m +′ n ≡ n + m
  helper m   zero  = refl
  helper m (suc n)          -- suc (m +′ n) ≡ suc (n + m)
    = cong suc (helper m n)

-- instead, assert the two operators are indistinguishable via two applications of extensionality:
same : _+′_ ≡ _+_
same = extensionality (λ m → extensionality (λ n → same-app m n))
{-
We occasionally need to postulate extensionality in what follows.

More generally, to postulate extensionality for dependent functions.
-}

-- type of f and g has changed from A → B to ∀ (x : A) → B x
postulate
  ∀-extensionality : ∀ {A : Set} {B : A → Set} {f g : ∀(x : A) → B x}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    →              f   ≡ g

{-
------------------------------------------------------------------------------
Isomorphism : sets are isomorphic if they are in one-to-one correspondence (via RECORDS)

formal definition:
-}

infix 0 _≃_
-- isomorphism between sets A and B consists of four things
record _≃_ (A B : Set) : Set where
  field
    to      : A → B                       -- function from A to B
    from    : B → A                       -- function from B to A
    from∘to : ∀ (x : A) → from (to x) ≡ x -- evidence asserting 'from' is a  left-inverse for 'to'
    to∘from : ∀ (y : B) → to (from y) ≡ y -- evidence asserting 'from' is a right-inverse for 'to'
open _≃_ -- makes field names 'to', etc., available without needing to write '_≃_.to'

{-
from ∘ to AND to ∘ from are identities

record declaration behaves similar to single-constructor data declaration
(there are minor differences - see Connectives):
-}
data _≃′_ (A B : Set): Set where
  mk-≃′ : ∀ (to      : A → B)
        → ∀ (from    : B → A)
        → ∀ (from∘to : (∀ (x : A) → from (to x) ≡ x))
        → ∀ (to∘from : (∀ (y : B) → to (from y) ≡ y))
        → A ≃′ B

to′      : ∀ {A B : Set} → (A ≃′ B) → (A → B)
to′      (mk-≃′ f g g∘f f∘g) = f

from′    : ∀ {A B : Set} → (A ≃′ B) → (B → A)
from′    (mk-≃′ f g g∘f f∘g) = g

from∘to′ : ∀ {A B : Set} → (A≃B : A ≃′ B) → (∀ (x : A) → from′ A≃B (to′ A≃B x) ≡ x)
from∘to′ (mk-≃′ f g g∘f f∘g) = g∘f

to∘from′ : ∀ {A B : Set} → (A≃B : A ≃′ B) → (∀ (y : B) → to′ A≃B (from′ A≃B y) ≡ y)
to∘from′ (mk-≃′ f g g∘f f∘g) = f∘g

{-
construct record values via

record
  { to      = f
  ; from    = g
  ; from∘to = g∘f
  ; to∘from = f∘g
  }

corresponds to using the constructor of the corresponding inductive type

mk-≃′ f g g∘f f∘g
where f, g, g∘f, and f∘g are values of appropriate types

------------------------------------------------------------------------------
Isomorphism is an equivalence : reflexive, symmetric, and transitive

REFLEXIVE
-}
≃-refl : ∀ {A : Set}
    -----
  → A ≃ A
≃-refl =
  record
    { to      = λ{x → x}    -- bind to identity function
    ; from    = λ{y → y}    -- ditto
    -- refl next is proof since for left inverse, from (to x) simplifies to x, and vice versa
    ; from∘to = λ{x → refl} -- bound to fun that discards arg; returns refl
    ; to∘from = λ{y → refl} -- ditto
    }

-- SYMMETRIC : swap roles of to/from and from∘to/to∘from
≃-sym : ∀ {A B : Set}
  → A ≃ B
    -----
  → B ≃ A
≃-sym A≃B =
  record
    { to      = from A≃B
    ; from    = to   A≃B
    ; from∘to = to∘from A≃B
    ; to∘from = from∘to A≃B
    }

-- TRANSITIVE : compose to/from and use equational reasoning to combine inverses
≃-trans : ∀ {A B C : Set}
  → A ≃ B
  → B ≃ C
    -----
  → A ≃ C
≃-trans A≃B B≃C =
  record
    { to       = to   B≃C ∘ to   A≃B
    ; from     = from A≃B ∘ from B≃C
    ; from∘to  = λ{x →
      begin
        (from A≃B ∘ from B≃C) ((to B≃C ∘ to A≃B) x)   ≡⟨⟩
         from A≃B  (from B≃C   (to B≃C  (to A≃B  x))) ≡⟨ cong (from A≃B) (from∘to B≃C (to A≃B x)) ⟩
         from A≃B                       (to A≃B  x)   ≡⟨ from∘to A≃B x ⟩
                                                 x
      ∎}
    ; to∘from = λ{y →
      begin
        (to B≃C ∘ to A≃B) ((from A≃B ∘ from B≃C) y)   ≡⟨⟩
         to B≃C  (to A≃B   (from A≃B  (from B≃C  y))) ≡⟨ cong (to B≃C) (to∘from A≃B (from B≃C y)) ⟩
         to B≃C                       (from B≃C  y)   ≡⟨ to∘from B≃C y ⟩
                                                 y
      ∎}
     }
{-
------------------------------------------------------------------------------
Equational reasoning for isomorphism

Essentially copy previous definition of equality for isomorphism.
Omit the form that corresponds to _≡⟨⟩_, since simple isomorphisms arise less often than simple equalities
-}

module ≃-Reasoning where

  infix  1 ≃-begin_
  infixr 2 _≃⟨_⟩_
  infix  3 _≃-∎

  ≃-begin_ : ∀ {A B : Set}
    → A ≃ B
      -----
    → A ≃ B
  ≃-begin A≃B = A≃B

  _≃⟨_⟩_ : ∀ (A : Set) {B C : Set}
    → A ≃ B
    → B ≃ C
      -----
    → A ≃ C
  A ≃⟨ A≃B ⟩ B≃C = ≃-trans A≃B B≃C

  _≃-∎ : ∀ (A : Set)
      -----
    → A ≃ A
  A ≃-∎ = ≃-refl

open ≃-Reasoning

{-
------------------------------------------------------------------------------
Embedding : weakening of isomorphism : embedding shows 1st type included in second;
or, equivalently, there is a many-to-one correspondence between the second type and the first.
-}

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_
{-
same as isomorphism, except it omits 'to∘from' field
- says 'from' is left-inverse for 'to', but not a right-inverse

Embedding is not symmetric

proofs of REFLEXIVE and TRANSITIVE same as ISOMORPHISM except omitting one unneeded case
-}

-- REFLEXIVE
≲-refl : ∀ {A : Set} → A ≲ A
≲-refl =
  record
    { to      = λ{x → x}
    ; from    = λ{y → y}
    ; from∘to = λ{x → refl}
    }

-- TRANSITIVE
≲-trans : ∀ {A B C : Set} → A ≲ B → B ≲ C → A ≲ C
≲-trans A≲B B≲C =
  record
    { to      = λ{x → to   B≲C (to   A≲B x)}
    ; from    = λ{y → from A≲B (from B≲C y)}
    ; from∘to = λ{x →
      begin
        from A≲B (from B≲C (to B≲C (to A≲B x))) ≡⟨ cong (from A≲B) (from∘to B≲C (to A≲B x)) ⟩
        from A≲B                   (to A≲B x)   ≡⟨ from∘to A≲B x ⟩
                                           x
      ∎}
     }
{-
weak form of ANTI-SYMMETRY
if two types embed in each other, and embedding functions correspond, they are isomorphic
-}

≲-antisym : ∀ {A B : Set}
  → (A≲B : A ≲ B)
  → (B≲A : B ≲ A)
  → (to   A≲B ≡ from B≲A)
  → (from A≲B ≡ to   B≲A)
    -------------------
  → A ≃ B
≲-antisym A≲B B≲A to≡from from≡to =
  record
    { to      = to      A≲B
    ; from    = from    A≲B
    ; from∘to = from∘to A≲B
    ; to∘from = λ{y →
      begin
        to   A≲B (from A≲B y)   ≡⟨ cong (to A≲B) (cong-app from≡to y) ⟩
        to   A≲B (to   B≲A y)   ≡⟨ cong-app to≡from (to B≲A y) ⟩
        from B≲A (to   B≲A y)   ≡⟨ from∘to B≲A y ⟩
                           y
      ∎}
    }

{-
First three components are copied from the embedding.
The last combines left inverse of B ≲ A with the equivalences of the 'to' and 'from' components
from the two embeddings to obtain the right inverse of the isomorphism.

------------------------------------------------------------------------------
Equational reasoning for embedding
-}

module ≲-Reasoning where

  infix  1 ≲-begin_
  infixr 2 _≲⟨_⟩_
  infix  3 _≲-∎

  ≲-begin_ : ∀ {A B : Set}
    → A ≲ B
      -----
    → A ≲ B
  ≲-begin A≲B = A≲B

  _≲⟨_⟩_ : ∀ (A : Set) {B C : Set}
    → A ≲ B
    → B ≲ C
      -----
    → A ≲ C
  A ≲⟨ A≲B ⟩ B≲C = ≲-trans A≲B B≲C

  _≲-∎ : ∀ (A : Set)
      -----
    → A ≲ A
  A ≲-∎ = ≲-refl

open ≲-Reasoning

{-
------------------------------------------------------------------------------
Exercise ≃-implies-≲ (practice) Show that every isomorphism implies an embedding. TODO

postulate
  ≃-implies-≲ : ∀ {A B : Set}
    → A ≃ B
      -----
    → A ≲ B
-- Your code goes here

------------------------------------------------------------------------------
Exercise _⇔_ (practice) Define equivalence of propositions TODO
(also known as “if and only if”) as follows:

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
Show that equivalence is reflexive, symmetric, and transitive.

-- Your code goes here

------------------------------------------------------------------------------
Exercise Bin-embedding (stretch) TODO

Bin and Bin-laws define a datatype Bin of bitstrings representing natural numbers
with functions

to : ℕ → Bin
from : Bin → ℕ

that satisfy

from (to n) ≡ n

Using the above, establish that there is an embedding of ℕ into Bin.

-- Your code goes here

Why do to and from not form an isomorphism?

------------------------------------------------------------------------------
Standard library

import Function             using (_∘_)
import Function.Inverse     using (_↔_)
import Function.LeftInverse using (_↞_)

The standard library _↔︎_ and _↞_ correspond to our _≃_ and _≲_, respectively,
but those in the standard library are less convenient,
since they depend on a nested record structure
and are parameterised with regard to an arbitrary notion of equivalence.

------------------------------------------------------------------------------
Unicode
This chapter uses the following unicode:

∘  U+2218  RING OPERATOR              (\o, \circ, \comp)
λ  U+03BB  GREEK SMALL LETTER LAMBDA  (\lambda, \Gl)
≃  U+2243  ASYMPTOTICALLY EQUAL TO    (\~-)
≲  U+2272  LESS-THAN OR EQUIVALENT TO (\<~)
⇔ U+21D4  LEFT RIGHT DOUBLE ARROW    (\<=>)
-}
