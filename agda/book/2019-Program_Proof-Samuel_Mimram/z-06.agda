module z-06 where

open import Data.Nat            using (ℕ; zero; suc; _+_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)
{-
data ℕ : Set where
  zero :     ℕ -- base case
  suc  : ℕ → ℕ -- inductive case

_+_ : ℕ → ℕ → ℕ
zero    + n =          n
(suc m) + n = suc (m + n)
infixl 6  _+_
-}
{-
------------------------------------------------------------------------------
-- p 287 6.5 Inductive types: logic

Use inductive types to implement types used as logical formulas,
though the Curry-Howard correspondence.

What follows is a dictionary between the two.

------------------------------------------------------------------------------
-- 6.5.1 Implication : corresponds to arrow (→) in types
-}

--classical formula   A ⇒ B ⇒ A  proved by
K : {A B : Set} → A → B → A
K x y = x

-- classical formula     (A ⇒ B ⇒ C) ⇒ (A ⇒ B) ⇒ A ⇒ C  proved by
S : {A B C : Set} → (A → B → C) → (A → B) → A → C
S g f x = g x (f x)

{-
------------------------------------------------------------------------------
-- 6.5.2 Product : corresponds to conjunction
defined in Data.Product
-}

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

-- aka fst
proj₁ : {A B : Set} → A × B → A
proj₁ (a , b) = a

-- aka snd
proj₂ : {A B : Set} → A × B → B
proj₂ (a , b) = b

-- proof of A ∧ B ⇒ B ∧ A (commutativity of conjunction)
×-comm : {A B : Set} → A × B → B × A
×-comm (a , b) = (b , a)

-- proof of curryfication
×-→ : {A B C : Set} → (A × B → C) → (A → B → C)
×-→ f x y = f (x , y)
-- and
→-× : {A B C : Set} → (A → B → C) → (A × B → C)
→-× f (x , y) = f x y

{-
introduction rule for conjunction:
    Γ ⊢ A    Γ ⊢ B
    -------------- (∧I)
    Γ ⊢ A      ∧ B

general : when logical connectives are defined with inductive types,
- CONSTRUCTORS CORRESPOND TO INTRODUCTION RULES

------------------------------------------------------------------------------
Induction principle : ELIMINATION RULE CORRESPONDS TO THE ASSOCIATED INDUCTION PRINCIPLE
-}

-- for case where P does not depend on its arg,
-- below dependent induction principle (x-ind) implies simpler principle:
×-rec : {A B : Set}
  → (P : Set)
  → (A → B → P)
  → A × B → P
×-rec P Pp (x , y) = Pp x y
{-
corresponds to elimination rule for conjunction:
    Γ, A,B ⊢ P      Γ ⊢ A ∧ B
    ------------------------- (∧E)
               Γ ⊢ P
if the premises are true then the conclusion is also true
-}
-- dependent induction principle
-- corresponds to the elimination rule in dependent types
-- see 8.3.3
×-ind : {A B : Set}
  → (P : A × B → Set)
  → ((x : A) → (y : B) → P (x , y))
  → (p : A × B) → P p
×-ind P Pp (x , y) = Pp x y

{-
------------------------------------------------------------------------------
-- p 289 6.5.3 Unit type : corresponds to truth
Data.Unit
-}

data ⊤ : Set where
  tt : ⊤ -- constructor is introduction rule

{-
    ----- (>I)
    Γ ⊢>

know from logic : there is no introduction rule associated to truth.
-}

-- induction principle
⊤-rec
  : (P : ⊤ → Set)
  → P tt
  → (t : ⊤)
  → P t
⊤-rec P Ptt tt = Ptt

{-
    Γ ⊢ P    Γ ⊢> ⊤
    --------------- (>E)
         Γ ⊢ P

not interesting from a logical point of view:
if P holds and > holds
then can deduce that P holds, which was already known

------------------------------------------------------------------------------
-- 6.5.4 Empty type : corresponds to falsity
Data.Empty
-}

data ⊥ : Set where
  -- no constructor, thus no introduction rule

-- dependent induction principle
⊥-d-elim : (P : ⊥ → Set) → (x : ⊥) → P x
⊥-d-elim P ()

-- non-dependent variant of this principle
⊥-elim : (P : Set) → ⊥ → P
⊥-elim P () -- () is the empty pattern in Agda
            -- indicates there are no cases to handle when matching on a value of type

{-
corresponds to explosion principle, which is the associated elimination rule

    Γ ⊢ ⊥
    ---- (⊥E)
    Γ ⊢ P

------------------------------------------------------------------------------
6.5.5 Negation
Relation.Nullary
-}

¬ : Set → Set
¬ A = A → ⊥

-- e.g., A ⇒ ¬¬A  proved:
nni : {A : Set} → A → ¬ (¬ A)
nni x f = f x

{-
------------------------------------------------------------------------------
p 290 6.5.6 Coproduct : corresponds to disjunction
Data.Sum
-}
data _⊎_ (A : Set) (B : Set) : Set where
  inj₁ : A → A ⊎ B -- called injection of A into A ⊎ B
  inj₂ : B → A ⊎ B -- ditto               B

{-
The two constructors correspond to the two introduction rules
    Γ ⊢ A
    --------- (∨ᴸI)
    Γ ⊢ A ∨ B

    Γ ⊢     B
    --------- (∨ᴿI)
    Γ ⊢ A ∨ B
-}

-- e.g., commutativity of disjunction
⊎-comm : (A B : Set) → A ⊎ B → B ⊎ A
⊎-comm A B (inj₁ x) = inj₂ x
⊎-comm A B (inj₂ y) = inj₁ y

{-
-- e.g., proof of (A ∨ ¬A) ⇒ ¬¬A ⇒ A
lem-raa : {A : Set}
  → A ⊎ ¬ A
  → ¬ (¬ A)
  → A
lem-raa (inj₁ a)  k = a
lem-raa (inj₂ a') k = ⊥-elim (k a')

-- induction principle
-}
⊎-rec : {A B : Set}
  → (P : A ⊎ B → Set)
  → ((x : A) → P (inj₁ x))
  → ((y : B) → P (inj₂ y))
  → (u : A ⊎ B)
  → P u
⊎-rec P P₁ P₂ (inj₁ x) = P₁ x
⊎-rec P P₁ P₂ (inj₂ y) = P₂ y
{-
non-dependent induction principle corresponds to the elimination rule
    Γ, A ⊢ P    Γ, B ⊢ P    Γ ⊢ A ∨ B
    ----------------------------- (∨E)
                   Γ ⊢ P

------------------------------------------------------------------------------
Decidable types : A type A is decidable when it is known whether it is inhabited or not
i.e. a proof of A ∨ ¬A

could define predicate: Dec A is a proof that A is decidable
-}
Dec' : Set → Set
Dec' A = A ⊎ ¬ A -- by definition of the disjunction
{-
Agda convention
  write yes/no instead of inj₁/inj₂
because it answers the question: is A provable?

defined in Relation.Nullary as

-- p 291
A type A is decidable when
-}
data Dec (A : Set) : Set where
  yes :   A → Dec A
  no  : ¬ A → Dec A

{-
logic of Agda is intuitionistic
therefore A ∨ ¬A not provable for any type A, and not every type is decidable

it can be proved that no type is not decidable (see 2.3.5 and 6.6.8)
-}

nndec : (A : Set) → ¬ (¬ (Dec A))
nndec A n = n (no (λ a → n (yes a)))

{-
------------------------------------------------------------------------------
-- p 291 6.5.7 Π-types : corresponds to universal quantification

dependent types : types that depend on terms

some connectives support dependent generalizations

e.g., generalization of function types
  A → B
to dependent function types
  (x : A) → B
where x might occur in B.
the type B of the returned value depends on arg x

e.g.,

replicate : {A : Set} → A → (n : ℕ) → Vec A n

Dependent function types are also called Π-types

often written

    Π(x : A).B


can be define as (note: there is builtin notation in Agda)
-}
data Π {A : Set} (A : Set) (B : A → Set) : Set where
  Λ : ((a : A) → B a) → Π A B

{-
an element of Π A B is a dependent function

    (x : A) → B x

bound universal quantification bounded (the type A over which the variable ranges)

   corresponds to

    ∀x ∈ A.B(x)

proof of that formula corresponds to a function which
- to every x in A
- associates a proof of B(x)

why Agda allows the notation
    ∀ x → B x
for the above type (leaves A implicit)

Exercise 6.5.7.1.
Show that the type
     Π Bool (λ { false → A ; true → B })
is isomorphic to
     A × B

------------------------------------------------------------------------------
-- p 292 6.5.8 Σ-types : corresponds to bounded existential quantification
Data.Product
-}

-- dependent variant of product types : a : A , b : B a
data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (a : A) → B a → Σ A B

-- actual Agda def is done using a record

dproj₁ : {A : Set} {B : A → Set} → Σ A B → A
dproj₁ (a , b) = a

dproj₂ : {A : Set} {B : A → Set} → (s : Σ A B) → B (dproj₁ s)
dproj₂ (a , b) = b

{-
Logical interpretation

type

    Σ A B

is bounded existential quantification

    ∃x ∈ A.B(x)

set theoretic interpretation corresponds to constructing sets by comprehension

    {x ∈ A | B(x)}

the set of elements x of A such that B(x) is satisfied

e.g., in set theory
- given a function f : A → B
- its image Im(f) is the subset of B consisting of elements in the image of f.

formally defined

     Im(f) = {y ∈ B | ∃x ∈ A.f (x) = y}

translated to with two Σ types
- one for the comprehension
- one for the universal quantification
Im : {A B : Set} (f : A → B) → Set
Im {A} {B} f = Σ B (λ y → Σ A (λ x → f x ≡ y))

e.g., can show that every function f : A → B has a right inverse (or section)
g : Im(f) → A

sec : {A B : Set} (f : A → B) → Im f → A
sec f (y , x , p) = x

------------------------------------------------------------------------------
-- p 293 : the axiom of choice

for every relation R ⊆ A × B satisfying ∀x ∈ A.∃y ∈ B.(x, y) ∈ R
there is a function f : A → B such that ∀x ∈ A.(x, f (x)) ∈ R

section 6.5.9 defined type Rel A B
corresponding to relations between types A and B

using Rel, can prov axiom of choice
AC : {A B : Set} (R : Rel A B)
  → ((x : A) → Σ B (λ y → R x y))
  → Σ (A → B) (λ f → ∀ x → R x (f x))
AC R f = (λ x → proj₁ (f x)) , (λ x → proj₂ (f x))

the arg that corresponds to the proof of (6.1), is constructive
a function which to every element x of type A
associates a pair of an element y of B
and a proof that (x, y) belongs to the relation.

By projecting it on the first component,
necessary function f is obtained (associates an element of B to each element of A)

use the second component to prove that it satisfies the required property

the "classical" variant of the axiom of choice
does not have access to the proof of (6.1) -- it only knows its existence.

another description is thus
where the double negation has killed the contents of the proof, see section 2.5.9

postulate CAC : {A B : Set} (R : Rel A B)
  → ¬ ¬ ((x : A) → Σ B (λ y → R x y))
  → ¬ ¬ Σ (A → B) (λ f → ∀ x → R x (f x))
see 9.3.4.

------------------------------------------------------------------------------
-- p 293 : 6.5.9 Predicates

In classical logic, the set B of booleans is the set of truth values:
- a predicate on a set A can either be false or true
- modeled as a function A → B

In Agda/intuitionistic logic
- not so much interested in truth value of predicate
- but rather in its proofs
- so the role of truth values is now played by Set

predicate P on a type A is term of type

  A → Set

which to every element x of A associates the type of proofs of P x.

------------------------------------------------------------------------------
-- p 294 Relations
Relation.Binary

In classical mathematics, a relation R on a set A is a subset of A × A (see A.1)

x of A is in relation with an element y when (x, y) ∈ R

relation on A can also be encoded as a function : A × A → 𝔹
or, curryfication,                              : A → A → 𝔹

In this representation, x is in relation with y when R(x, y) = 1

In Agda/intuitionistic
- relations between types A and B as type Rel A
- obtained by replacing the set 𝔹 of truth values with Set in the above description:

    Rel : Set → Set₁
    Rel A = A → A → Set

e.g.
- _≤_ : type Rel ℕ
- _≡_ : type Rel A

------------------------------------------------------------------------------
Inductive predicates (predicates defined by induction)
--e.g.,
-}

data isEven : ℕ → Set where
  even-z : isEven zero                               -- 0 is event
  even-s : {n : ℕ} → isEven n → isEven (suc (suc n)) -- if n is even then n + 2 is even
{-
corresponds to def of set E ⊆ N of even numbers
as the smallest set of numbers such that
    0 ∈ E
and
    n ∈ E ⇒ n+2 ∈ E
-}
data _≤_ : ℕ → ℕ → Set where
  z≤n : {n : ℕ}                 →  zero ≤     n
  s≤s : {m n : ℕ} (m≤n : m ≤ n) → suc m ≤ suc n
{-
smallest relation on ℕ such that
     0 ≤ 0
and
     m ≤ n implies m+1 ≤ n+1

inductive predicate definitions enable reasoning by induction over those predicates/relations
-}

≤-refl : {n : ℕ} → (n ≤ n)
≤-refl  {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl

-- p 295

≤-trans : {m n p : ℕ} → (m ≤ n) → (n ≤ p) → (m ≤ p)
≤-trans      z≤n       n≤p  = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

{-
inductive defs good because of Agda's support for
reasoning by induction (and dependent pattern matching)
leading to simpler proofs

other defs possible

-- base on classical equivalence, for m, n ∈ N, m ≤ n ⇔ ∃m' ∈ N.m + m' = n
_≤'_ : ℕ → ℕ → Set
m ≤' n = Σ (λ m' → m + m' ≡eq n)

another:

le : ℕ → ℕ → Bool
le   zero       n  = true
le (suc m)   zero  = false
le (suc m) (suc n) = le m n

_≤'_ : ℕ → ℕ → Set
m ≤' n = le m n ≡ true

EXERCISE : show reflexivity and transitivity with the alternate formalizations

involved example
implicational fragment of intuitionistic natural deduction is formalized in section 7.2
the relation Γ ⊢ A
between a context Γ and a type A
which is true when the sequent is provable
is defined inductively

------------------------------------------------------------------------------
-- p 295 6.6 Equality
Relation.Binary.PropositionalEquality

-- typed equality : compare elements of same type
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x -- only way to be equal is to be the same

------------------------------------------------------------------------------
6.6.1 Equality and pattern matching

example proof with equality : successor function on natural numbers is injective
- for every natural numbers m and n
- m + 1 = n + 1 ⇒ m = n
-}

-- p 296

suc-injective : {m n : ℕ} → suc m ≡ suc n → m ≡ n
-- suc-injective {m} {n} p  -- Goal: m ≡ n;   p : suc m ≡ suc n
                            -- case split on p : it can ONLY be refl
                            -- therefore m is equal to n
                            -- so agda provide .m - not really an arg - something equal to m
suc-injective {m} {.m} refl --       m ≡ m
  = refl

{-
------------------------------------------------------------------------------
-- p 296 6.6.2 Main properties of equality: reflexive, congruence, symmetric, transitive
-}

sym : {A : Set} {x y : A}
  → x ≡ y
  → y ≡ x
sym refl = refl

trans : {A : Set} {x y z : A}
  → x ≡ y
  → y ≡ z
  → x ≡ z
trans refl refl = refl

cong : {A B : Set} (f : A → B) {x y : A}
  →   x ≡   y
  → f x ≡ f y
cong f refl = refl

-- substitutivity : enables transporting the elements of a type along an equality
subst : {A : Set} (P : A → Set) → {x y : A}
  → x ≡ y
  → P x
  → P y
subst P refl p = p

-- coercion : enables converting an element of type to another equal type
{-
coe : {A B : Set}
  → A ≡ B
  → A
  → B
coe p x = subst (λ A → A) p x
-}
-- see 9.1

{-
------------------------------------------------------------------------------
-- p 296 6.6.3 Half of even numbers : example every even number has a half
using proof strategy from 2.3

traditional notation : show  ∀n ∈ N. isEven(n) ⇒ ∃m ∈ N.m + m = n
-}

+-suc : ∀ (m n : ℕ)
      →      m + suc n
      ≡ suc (m +     n)
+-suc  zero   n = refl
+-suc (suc m) n = cong suc (+-suc m n)
{-
even-half : {n : ℕ}
  → isEven n
  → Σ (λ m → m + m ≡ n)
even-half  even-z = zero , refl
even-half (even-s e) with even-half e
even-half (even-s e) | m , p =
  suc m , cong suc (trans (+-suc m m) (cong suc p))

second case : by induction have m such that m + m = n
need to construct a half for n + 2: m + 1
show that it is a half via

    (m + 1) + (m + 1) =  (m + (m  + 1)) + 1   by definition of addition
                      = ((m +  m) + 1)  + 1   by +-suc
                      =       (n  + 1)  + 1   since m + m = n

implemented using transitivity of equality and
fact that it is a congruence (m + 1 = n + 1 from m = n)
also use auxiliary lemma m + (n + 1) = (m + n) + 1
-}
{-
------------------------------------------------------------------------------
-- p 297 6.6.4 Reasoning
-}

--import      Relation.Binary.PropositionalEquality as Eq
--open        Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

infix  3 _∎
infixr 2 _≡⟨⟩_ _≡⟨_⟩_
infix  1 begin_

begin_ : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → x ≡ y
begin_ x≡y = x≡y

_≡⟨⟩_ : ∀ {ℓ} {A : Set ℓ} (x {y} : A) → x ≡ y → x ≡ y
_ ≡⟨⟩ x≡y = x≡y

_≡⟨_⟩_ : {A : Set} (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
_ ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

_∎ : ∀ {ℓ} {A : Set ℓ} (x : A) → x ≡ x
_∎ _ = refl

+-zero : ∀ (n : ℕ) → n + zero ≡ n
+-zero   zero  = refl
+-zero (suc n) rewrite +-zero n = refl

+-comm : (m n : ℕ) → m + n ≡ n + m
+-comm m zero    = +-zero m
+-comm m (suc n) =
  begin
    (m + suc n) ≡⟨ +-suc m n ⟩             -- m + (n + 1) = (m + n) + 1 -- by +-suc
    suc (m + n) ≡⟨ cong suc (+-comm m n) ⟩ -- = (n + m) + 1 -- by induction hypothesis
    suc (n + m)
  ∎

-- p 298
-- another proof using properties of equality
+-comm' : (m n : ℕ) → m + n ≡ n + m
+-comm' m   zero  = +-zero m
+-comm' m (suc n) = trans (+-suc m n) (cong suc (+-comm m n))

{-
------------------------------------------------------------------------------
-- p 298 6.6.5 Definitional equality
...
-}
