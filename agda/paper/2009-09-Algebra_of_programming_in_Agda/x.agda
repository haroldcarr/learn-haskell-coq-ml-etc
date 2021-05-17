module x where

{-
------------------------------------------------------------------------------
Abstract

Relational program derivation technique :
- stepwise refining a relational spec to a program by algebraic rules.
- program obtained is correct by construction

Dependent type theory rich enough to express correctness properties verified by type checker.

Library, Algebra of Programming in Agda (AoPA)
- http://www.iis.sinica.edu.tw/~scm/2008/aopa/
- encodes relational derivations
- A program is coupled with an algebraic derivation whose correctness is guaranteed by type system

------------------------------------------------------------------------------
1 Introduction

relational program derivation
- specs are input-output relations that are stepwise refined by an algebra of programs.

paper show how program derivation can be encoded in a type and its proof term.

- case study using Curry-Howard isomorphism
- modelled a many concepts that occur in relational program derivation
  including relational folds, relational division, and converse-of-a-function.
  - e.g., Minimum, is defined using division and intersection
  - e.g., greedy theorem proved using the universal property of minimum.
  - the theorem can be used to deal with a number of optimisation problems specified as folds.
• to ensure termination with unfolds and hylomorphisms, enable programmer to
  model an unfold as the relational converse of a fold, but demand a proof of accessibility
  before it is refined to a functional unfold.

------------------------------------------------------------------------------
2 Overview of Relational Program Derivation

Merit of functional programming : programs can be manipulated by equational reasoning.

Program derivation in Bird-Meertens style 1989b) typically start with a specification,
as a function, that is obviously correct but not efficient.

Algebraic identities are then applied, in successive steps,
to show that the specification equals another functional program that is more efficient.

Typical example : Maximum Segment Sum (Gries, 1989; Bird, 1989a), specification :
    max · map sum · segs
where
- segs produces all consecutive segments of the input list of numbers
- map sum computes their summation
- maximum is picked by max
Specification can be shown, after transformation, to be equal a version using foldr (linear).

In 90’s there was a trend in the program derivation community to move from functions to relations.
- spec given in terms of an input/output relation
- relation is refined to smaller, more deterministic relations in each step,
- until obtaining a function

Relational derivation advantages:
- spec often more concise than corresponding functional specification
- optimisation problems can be specified using relations generating all possible solutions
- easier to talk about program inversion

The catch: *must reason in terms of inequalities rather than equalities*

denoted by R : B ← A
- relation R from A to B
- subset of set of pairs B × A
- A function f is seen as a special case where (b, a) ∈ f and (b' , a) ∈ f implies b = b'

converse R˘ : A ← B of R : B ← A
- defined by (a, b) ∈ R ˘ if (b, a) ∈ R

composition of two relations R : C ← B and S : B ← A
- defined by: (c, a) ∈ R ◦ S if ∃b : (c, b) ∈ R ∧ (b, a) ∈ S

power transpose ΛR of R : B ← A
- is a function from A to P B (subsets of B)
- ΛR a = { b | (b, a) ∈ R }.

relation ∈ : A ← P A
- maps a set to one of its arbitrary members
- converse: ∋ : P A ← A

product of two relations, R × S
- defined by ((c, d), (a, b)) ∈ R × S if (c, a) ∈ R and (d, b) ∈ S

--      step fun      base  input    output
foldr : (A → B → B) → B   → List A → B

generalisation to a relation, denote foldR

              input    step relation  base cases
foldR R : B ← List A ← (B ← (A×B))    (set s : PB)

-- relation mapping a list to one of its arbitrary subsequences
subseq = foldR (cons ∪ outr) {[ ]}
where
  cons (x, xs) = x :: xs -- cons keeps the current element
  outr (x, xs) = xs      -- outr drops the current element

Relational fold defined in terms of functional fold:

   foldR R s = ∈ ◦ foldr Λ(R ◦ (id " ∈)) s

of type List A → P B that collects all the results in a set.

step function Λ(R ◦ (id " ∈))  : has type (A × P B) → P B
(id " ∈) : (A × B) ← (A × P B) : pairs current element with one of the results from previous step
                                 before passing the pair to R

foldR R s satisfies the universal property:
foldR R s = S ⇔ R ◦ (id " S) = S ◦ cons ∧ s = ΛS [ ]

-- induction rule
-- states that foldR R s is the unique fixed-point of the monotonic function
λ X → (R ◦ (id " X) ◦ cons ˘) ∪ {(b, [ ]) | b ∈ s}

-- computation rule
-- foldR R s is also the least prefix-point, therefore
foldR R s ⊆ S ⇐ R ◦ (id " S) ⊆ S ◦ cons ∧ s ⊆ ΛS [ ],
R ◦ (id " foldR R s) ⊆ foldR R s ◦ cons ∧ s ⊆ Λ(foldR R s) [ ].

If an optimisation problem can be specified by generating all possible solutions using
foldR or converse of foldR before picking the best one, Bird and de Moor (1997) gave a
number of conditions under which the specification can be refined to a greedy algorithm, a
dynamic programming algorithm, or something in-between.

------------------------------------------------------------------------------
3 A Crash Course in Agda
-}

-- Fig. 2. Some examples of datatype definitions.

data List (A : Set) : Set where
  []   : List A
  _∷_  : A → List A → List A

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

data _≤_ : ℕ → ℕ → Set where
  ≤-refl : {n   : ℕ} → n ≤ n
  ≤-step : {m n : ℕ} → m ≤ n → m ≤ suc n

_<_ : ℕ → ℕ → Set
m < n = suc m ≤ n

-- Fig. 3. An encoding of first-order intuitionistic logic in Agda.

-- Truth : has one unique term — a record with no fields
record ⊤ : Set where

-- Falsity : type with no constructors therefore no inhabitants
data ⊥ : Set where

-- Disjunction : proof deduced either from a proof of P or a proof of Q
data _⨄_ (A B : Set) : Set where
  inj1 : A → A ⨄ B
  inj2 : B → A ⨄ B

-- Dependent Pair : type of 2nd may depend on the first component
data Σ (A : Set)(B : A → Set) : Set where
  _,_ : (x : A) → (y : B x) → Σ A B

proj₁ : ∀ {A B} → Σ A B → A
proj₁ (x , y) = x

proj₂ : ∀ {A B} → (p : Σ A B) → B (proj₁ p)
proj₂ (x , y) = y

-- Conjunction
_×_ : (A B : Set) → Set
A × B = Σ A (λ _ → B)

-- Existential Quantification
-- to prove the proposition ∃ P, where P is a predicate on terms of type A
-- provide, in a pair, a witness w : A and a proof of P w.
∃ : {A : Set} (P : A → Set) → Set
∃ = Σ _

-- Universal Quantification
-- of predicate P on type A is encoded as a dependent function type
-- whose elements, given any x : A, must produce a proof of P x.
-- Agda provides a short hand ∀ x → P x in place of (x : A) → P x when A can be inferred.

-- Implication
-- P → Q is represented as a function taking a proof of P to a proof of Q
-- no new notation for it

-- Predicates
-- on type A are represented by A → Set.
-- e.g., (λ n → zero < n) : N → Set
-- is a predicate stating that a natural number is positive.

{-
3.2 Identity Type

term of type x ≡ y is proof x and y are equal
-}

data _≡_ {A : Set}(x : A) : A → Set where
  ≡-refl : x ≡ x

-- REFLEXIVE : by definition

-- SYMMETRIC
≡-sym : {A : Set}{x y : A} → x ≡ y → y ≡ x
≡-sym {A}{x}{.x} ≡-refl = ≡-refl

-- TRANSITIVE
≡-trans : {A : Set}{x y z : A} → x ≡ y → y ≡ z → x ≡ z
≡-trans {A}{x}{.x}{z} ≡-refl x≡z = x≡z -- could replace x≡z with ≡-refl

-- SUBSTITUTIVE — if x ≡ y, they are interchangeable in all contexts
≡-subst : {A : Set}(P : A → Set){x y : A} → x ≡ y → P x → P y
≡-subst P ≡-refl Px = Px

≡-cong : {A B : Set}(f : A → B){x y : A} → x ≡ y → f x ≡ f y
≡-cong f ≡-refl = ≡-refl
{-
≡ NOT EXTENSIONAL
Qquality of terms is checked by expanding them to normal forms.
Problem comparing higher-order values: e.g., sum·map sum     and     sum · concat
while defining the same function summing up a list of lists of numbers, are not “equal” under ≡ .

Can define extensional equality for (non-dependent) functions on first-order values:
-}
_≐_ : {A B : Set} → (A → B) → (A → B) → Set
f ≐ g = ∀ x → f x ≡ g x
{-
≐ NOT SUBSTITUTIVE : congruence of ≐ has to be proved for each context.

Refinement in preorder reasoning usually involves replacing terms in provably monotonic contexts.
This is a significant overhead; but given that this overhead
is incurred anyway, not having extensional equality is no extra trouble.

3.3 Preorder Reasoning

To prove e1 ≡ e2 is to construct a term having that type.

For any binary relation ∼ that is reflexive and transitive
- for which one can construct terms ∼-refl and ∼-trans having the types described in above
can derive a set of combinators, shown in Fig. 4,
which enables one to construct a term of type e₁ ∼ en  in algebraic style.

-- Fig. 4. Combinators for preorder reasoning.
infixr 2 _∼⟨_⟩_
infix  2 _∼∎

_∼⟨_⟩_ : {A : Set}(x : A){y z : A} → x ∼ y → y ∼ z → x ∼ z
x ∼⟨ x∼y ⟩ y∼z = ∼-trans x∼y y∼z

_∼∎ : {A : Set}(x : A) → x ∼ x
x ∼∎ = ∼-refl


implication as a relation:
-}
_⇒_ : Set → Set → Set
P ⇒ Q = P → Q

_⇐_ : Set → Set → Set
P ⇐ Q = Q ⇒ P
{-
Reflexivity and transitivity of ⇐ , for example, can be simply given by ⇐-refl = id and
⇐-trans = · , where · is function composition. Therefore, they induce a pair of operators
_⇐⟨_⟩_ and ⇐∎ for logical reasoning.

Hierarchy of universes
- Set, the type of small types, is in sort Set1

When instantiating ∼ in Fig. 4 to ⇐ : Set → Set → Set
- notice that the type A : Set itself cannot be instantiated to Set, which is in Set1

We resolve this by using different module generators for different universes.
More on this in Sect. 4.1.

3.4 Functional Derivation

Since _≐_ is reflexive and transitive (not shown), it induces its preorder reasoning operators.

Fig. 5 shows a proof of the universal property of foldr.

The steps using ≡-refl are equivalences Agda proves by expanding definitions.
The inductive hypothesis is established by a recursive call to foldr-universal.
-}
infix  3 _≡∎
infixr 2 _≡⟨⟩_ _≡⟨_⟩_
infix  1 begin_

begin_ : {A : Set} {x y : A} → x ≡ y → x ≡ y
begin_ x≡y = x≡y

_≡⟨⟩_ : {A : Set} (x {y} : A) → x ≡ y → x ≡ y
_ ≡⟨⟩ x≡y = x≡y

_≡⟨_⟩_ : {A : Set} (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
_ ≡⟨ x≡y ⟩ y≡z = ≡-trans x≡y y≡z

_≡∎ : {A : Set} (x : A) → x ≡ x
_≡∎ _ = ≡-refl
{-
Fig. 5. Proving the universal property for foldr
-}
foldr              : {A B : Set}
                   → (A → B → B) → B → List A
                   → B
foldr f e      []  = e
foldr f e (x ∷ xs) = f x (foldr f e xs)

{- https://www.cs.nott.ac.uk/~pszgmh/fold.pdf
For finite lists, the universal property of fold can be stated as the following equivalence
between two definitions for a function g that processes lists:
g      []  = v
                         ⇔ g = fold f v
g (x ∷ xs) = f x (g xs)

right-to-left : substituting g = fold f v into the two equations for g gives recursive fold def
left-to-right : two equations for g are the assumptions required to show that g = fold f v
using a simple proof by induction on finite lists (Bird, 1998)

universal property states that for finite lists the function fold f v is not just a
solution to its defining equations, but the unique solution.

Utility of universal property : makes explicit the two assumptions
required for a certain pattern of inductive proof.

For specific cases, by verifying the two assumptions (typically done without need for induction)
can then appeal to the universal property to complete the inductive proof that g = fold f v.

universal PROPERTY of fold : encapsulates a pattern of inductive proof concerning lists
just as the fold OPERATOR  : encapsulates a pattern of recursion for processing lists
-}
foldr-universal : ∀ {A B} (h : List A → B) f e
                → (h [] ≡ e)
                → (∀ x xs → h (x ∷ xs) ≡ f x (h xs))
                → h ≐ foldr f e
foldr-universal h f e base step      []  = base
foldr-universal h f e base step (x ∷ xs) =
  begin
    h (x ∷ xs)
  ≡⟨ step x xs ⟩
    f x (h xs)
  ≡⟨ ≡-cong (f x) (foldr-universal h f e base step xs) ⟩
    f x (foldr f e xs)
  ≡⟨ ≡-refl ⟩
    foldr f e (x ∷ xs)
  ≡∎
{-
-- Can use the universal property to prove the foldr-fusion theorem:

_·_ : {A : Set} {B : A → Set} {C : (x : A) → B x → Set}
    → (f : {x : A} (y : B x) → C x y)
    → (g : (x : A) → B x)
    → (x : A) → C x (g x)
(f · g) x = f (g x)

-- TODO compile problems
foldr-fusion : ∀ {A B C} (h : B → C) {f : A → B → B} {g : A → C → C}
             → {e : B}
             → (∀ x y → h (f x y) ≡ g x (h y))
             → (h · foldr f e) ≐ (foldr g (h e))
foldr-fusion h {f} {g} e fuse =
  foldr-universal (h · foldr f e) g (h e) ≡-refl (λ x xs → fuse x (foldr f e xs))
-}

