module Part2_Agda_in_a_Nutshell where

{-
https://serokell.io/blog/agda-in-nutshell

Danya Rogozin
Monday, November 26th, 2018

Theorem proving in Agda based on a constructive logical framework.
Dependently typed programming and theorem proving in Agda.
Example use of Agda in the formalisation of algebra.

------------------------------------------------------------------------------
General

dependently typed : based on variant of Martin-Löf type theory.

Martin-Löf is constructive, so Agda doesn’t have default features for writing classical
(in contrast to HOL or Isabelle).

Agda : proof-assistant based on propositions-as-types.

------------------------------------------------------------------------------
Types in Agda

datatypes introduced via GADTs
-}

data Writers    : Set where Wilde Shelley Byron Sartre Camus                    : Writers
data Literature : Set where DorianGrey Alastor ChildeHarold LaNausée L’Étranger : Literature

data ℕ : Set where
  zero :     ℕ
  suc  : ℕ → ℕ

{-
Parameterized and indexed datatypes

see: https://agda.readthedocs.io/en/latest/language/data-types.html#indexed-datatypes

parameterized datatype
- depends on parameter that should remain the same in the types of constructors
-}

data List (A : Set) : Set where
  Nil  :              List A
  Cons : A → List A → List A

-- can have indexes that could differ from constructor to constructor

-- Vec is parameterized by A and indexed by ℕ.
data Vec (A : Set) : ℕ → Set where
  Nil  :                         Vec A  zero
  -- n is an implicit argument of the constructor.
  -- Cons is an example of a dependent function.
  Cons : {n : ℕ} → A → Vec A n → Vec A (suc n)

-- defines types of finite cardinality
-- Fin is a datatype indexed by a natural number.
data Fin : ℕ → Set where
  -- zero reflects an empty set which is an element of any finite set
  zero : {n : ℕ} →         Fin (suc n)
  -- suc applies the finite set i and yields a new finite set, larger than i on one element.
  suc  : {n : ℕ} → Fin n → Fin (suc n)

-- Vec and Fin in Agda are examples of dependent types,
-- types that depend on values (e.g., natural numbers), not only on types.

------------------------------------------------------------------------------
-- Functions

bookToWriter : Literature → Writers
bookToWriter DorianGrey   = Wilde
bookToWriter Alastor      = Shelley
bookToWriter ChildeHarold = Byron
bookToWriter LaNausée     = Sartre
bookToWriter L’Étranger   = Camus

-- A dependent function is a function that
--   takes a term a of type A and
--   returns some result of type B, where a may appear in B

-- Here types A and B are explicit arguments.
const₁ : (A B : Set) → A → B → A
const₁ A B x y = x

-- may take them implicitly:
const₂ : {A B : Set} → A → B → A
const₂ x y = x

data Ireland : Set where Dublin : Ireland
data England : Set where London : England
data France  : Set where Paris  : France

WriterToCountry : Writers → Set
WriterToCountry Wilde   = Ireland
WriterToCountry Shelley = England
WriterToCountry Byron   = England
WriterToCountry Sartre  = France
WriterToCountry Camus   = France

WriterToCity : (w : Writers) → WriterToCountry w
WriterToCity    Wilde   = Dublin
WriterToCity    Shelley = London
WriterToCity    Byron   = London
WriterToCity    Sartre  = Paris
WriterToCity    Camus   = Paris

{-
WriterToCity is a dependent function, because the type of the result depends on the argument.
The type of a dependent function is called a Π-type.

Generally, given function P: A → Set,
then Π-type is a type of the function that assigns
to every term t : A
some object of type P t.

In Agda : (t : A) → P t for some {A : Set} {P : A → Set} in context.

Logically Π-type encodes intuitionistic universal quantifier.

P is a predicate and
some function of a type (t : A) → P t
is a proof that
for each t : A the property P has established.

------------------------------------------------------------------------------
Dependent pairs : Σ-type

inductive predicate on two lists:
-}

data IsReverse {A : Set} : (xs ys : List A) → Set where
  ReverseNil  : IsReverse [] []
  ReverseCons : (x : A) (xs ys : List A)
              → IsReverse xs ys
              → IsReverse (x ∷ xs) (ys ++ [ x ])
{-
IsReverse xs ys denotes that the list ys is equal to reversed xs.

IsReverse is a datatype parameterized over a type A and
indexed over two lists of elements of type A.

prove theorem : for all list xs there exists list ys such that ys is a reversed list xs,
i.e., produce a dependent pair, where
  the first projection of this pair is some list ys and
  the second one is a proof that ys is reversed list xs.

Use the dependent pair type to prove existence:
-}

theoremReverse₁ : {A : Set} (xs : List A) → Σ (List A) (λ ys → IsReverse xs ys)
theoremReverse₁ [] = [] , ReverseNil
theoremReverse₁ (z ∷ zs) =
    let ys      = proj₁ (theoremReverse₁ zs) in
    let ysProof = proj₂ (theoremReverse₁ zs) in
    ys ++ [ x ] , ReverseCons z zs ys ysProof
{-
Read the theoremReverse₁ signature as
  let A be type and
  xs be a list with elements of the type A,
  then there exists list ys, such that ys is the reversed list xs.

proved by induction on xs and build ys for every case.
Base case: if xs is empty,
  then we present an empty list as the desired list ys and
  ReverseNil is a proof that ys is equal to reversed list xs, because both of them are empty.
Inductive step : xs is a non-empty
  List zs is smaller than z :: zs,
  so apply induction hypothesis to zs and
  show that :: preserves the desired property using the ReverseCons constructor.

Generally, Σ-type is defined in Agda:
-}
record Σ {a b} (A : Set a) (B : A → Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    fst : A
    snd : B fst -- apply B to arg from first field
{-
Defined for arbitrary universes (see below).

logical point of view : Σ A B
  there exists some element of a type A, such that this element satisfies property B
i.e. Σ-type encodes constructive existential quantifier.

To prove the existence of some object with desired property constructively,
present the particular object and prove that object satisfies the property.

------------------------------------------------------------------------------
Universes

Hierarchy of types : initiated by Bertrand Russell.

Basic types (e.g., Nat, Bool) have a type called Set (i.e., Set₁)

Universe polymorphism enables defining functions that can be called on types of arbitrary levels. E.g.,
-}
s : {A B C : Set} → (A → B → C) → (A → B) → A → C
s f g x = f x (g x)

-- indicate that types A, B, C are types of arbitrary level a:
s₁ : {a : Level} {A B C : Set a} → (A → B → C) → (A → B) → A → C
s₁ f g x = f x (g x)

-- generalize s further, because A, B and C in s₁ belong to the same level above,
-- but these types may generally belong to different levels :
s₂ : {a b c : Level} {A : Set a} {B : Set b} {C : Set c}
    → (A → B → C) → (A → B) → A → C
s₂ f g x = f x (g x)

-- most general version of s : a universe polymorphic dependent function

S : {a b c : Level} {A : Set a} {B : A → Set b} {C : (x : A) → B x → Set c}
   → (f : (x : A) → (y : B x) → C x y) → (g : (x : A) → B x)
   → (x : A) → C x (g x)
S f g x = f x (g x)

------------------------------------------------------------------------------
-- Records

record Person : Set where
  field
    name    : String
    country : String
    age     : Int

------------------------------------------------------------------------------
-- Typeclasses are introduced in Agda as records. E.g.,

record Functor {a} (F : Set a → Set a) : Set (suc a) where
  field
    fmap : ∀ {A B} → (A → B) → F A → F B

-- Instances may be declared either
-- by constructing a record explicitly:
instance
  ListFunctor₁ : Functor List
  ListFunctor₁ = record { fmap = map }
-- or by using copatterns:
instance
  ListFunctor₂ : Functor List
  fmap {{ListFunctor₂}} = map

------------------------------------------------------------------------------
-- Propositional equality

-- The identity type is a datatype that reflects equality of terms in predicate logic.

data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  refl : x ≡ x

-- examples using propositional equality:

-- proof of distributivity of natural number multiplication over addition
*-+-distr : ∀ a b c → (b + c) * a ≡ b * a + c * a
*-+-distr a zero c = refl
*-+-distr a (suc b) c =
    begin
    (suc b + c) * a     ≡⟨ refl ⟩
    a + (b + c) * a     ≡⟨ cong (_+_ a) (*-+-distr a b c) ⟩
    a + (b * a + c * a) ≡⟨ sym (+-assoc a (b * a) (c * a)) ⟩
    suc b * a + c * a
    ∎

-- Proved by induction on b.
-- When b equals zero, the equality holds trivially.
-- Otherwise, equality is proved using the induction hypothesis.

{-
------------------------------------------------------------------------------
Algebraic example

A ring is an algebraic system
⟨R, +, ⋅, −, 0⟩
, where
R is a non-empty set,
0 ∈ R,
+, ⋅ are binary operations on R
 and
− is a unary operation on R
 such that:
1. ∀a,b,c ∈ R, (a+b)+c=a+(b+c);
2. ∀a,b ∈ R, a+b=b+a;
3. ∀a ∈ R, −a + a = 0;
4. ∀a ∈ R, a + 0 = a;
5. ∀a,b,c ∈ R, (a+b)⋅c=(a⋅c)+(b⋅c);
6. ∀a,b,c ∈ R, a⋅(b+c)=(a⋅b)+(a⋅c).
-}

record Ring (R : Set) : Set₁ where
  constructor mkRing
  infixr 7 _·_
  infixr 6 _+_
  field
      θ   : R
      -_  : R → R
      _+_ : R → R → R
      _·_ : R → R → R

      +-assoc       : (a b c : R) → (a + b) + c ≡ a + (b + c)
      +-commute     : (a b : R)   → a + b ≡ b + a
      +-inv₁        : (a : R)     → - a + a ≡ θ
      +-θ           : (a : R)     → a + θ ≡ a
      ·-distr-right : (a b c : R) → (a + b) · c ≡ (a · c) + (b · c)
      ·-distr-left  : (a b c : R) → a · (b + c) ≡ (a · b) + (a · c)
open Ring {{...}} public

