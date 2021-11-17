module xx where

------------------------------------------------------------------------------
{-
-- p 1 1.1 Hilbert's consistency program

late 19th century : formalization of proofs : Frege, Peano


1899 : David Hilbert : foundations of geometry
- conception of math theories as formal system that can be given varying interpretations
- did not work with a formalized logic --- it is assumed informally
  - logical connectives, quantifiers, etc., not formal (i.e., symbols devoid of meaning)
    but have standard informal meaning

1905 : Hilbert : mathematical analysis of proofs considered as mathematical objects

1910-1913 : Whitehead and Russell : Principia Mathematica (higher-order logic, i.e., type theory)

1917 : Hilbert
- problem of "the decidability of a math question in a finite number of operations"
- problem of consistency of math

1917-1918 : Hilbert : The Principles of mathematics

1918 : Paul Bernays (studied under Hilbert) : distinction between syntax and semantics
- propositional logic
  - formal system
  - semantic interpretation in terms of truth-values
  - proof of soundess and completeness relative to that semantics
- shows propositional logic is decidable using normal forms

1918 : Hermann Weyl : Das Kontinuum
- criticized set theory and classical analysis (non-constructive and infinitary)
- developed "predicativity" : arithmetical continuum in which large portions of analysis can be done

1921 : Brouwer : intuitionism
- abandons : excluded middle for infinite totalities; non-constructive math; infinitary math

Hilbert rejected Brouwer/Weyl; aimed at foundations without giving up classical math
- branch of meth known as proof theory originated with this program

1922 : Hilbert : distinction between mathematics proper and metamathematics

1928 : Hilbert and Ackermann : Principles of Theoretical Logic

- propositional, predicate, higher-order logic
- metatheory : consistency and completeness for propositional logic,
               independence of axioms, ...

1931 : Godel's incompleteness theorems

-- p 6 1.2 Gentzen's proof theory

1930s : Gerhard Gentzen : developed basis for what is now called "proof theory"

1933 : Gentzen : any deriviation in classical arithmetic can be translated into
                 a   deriviation in a sysystem of intuitionistic arithmetic

1935 : Gentzen : dissertation "Investigations into logical deduction"
- natural deduction (proof is a tree of formulas)
  - reasoning from hypothetical assumptions (formulas assumed to be true)
  - rules enable inference of consequences of assumptions
    - some rules enable discharging assumptions
      - conclusion of such a rule then no longer depends on the formula that was
        assumed to be true at the outset
      - a formula is proved outright if all assumptions are discharged
  - in contrast to axiomatic derivation systems (have many axioms and few inference rules)
    Gentzen's natural deduction system has many inference rules an NO axioms
- sequent calculus
  - TODO

-- p 10 1.3 Proof theory after Gentzen
TODO
-}

------------------------------------------------------------------------------
-- p 13 Axiomatic calculi

{-
Before Gentzen, the axiomatic presentation of logical system was typical
(e.g., Whitehead, Russell, Hilbert).

logic
- axioms (aka postulates) : logical laws (taken as starting points)
- inference rules : enable moving from starting points to new logical validities

--------------------------------------------------
-- p 13 2.1 Propositional logic

Gentzen worked only with intuitionistic and classical logic.
1937 : Johansson : introduced "minimal logic" - a weaker system.
Here intuitionistic and classical is built on top of minimal.
Axiomatization follows Heyting (1930) and Johansson.
-}

--------------------------------------------------
-- p 17 2.3 Sub-formulas and main connectives

-- disjunction
data _v_ (S : Set) (T : Set) : Set where
  inl : S -> S v T
  inr : T -> S v T

-- conjunction
record _^_ (S : Set) (T : Set) : Set where
  constructor _,_
  field
    fst : S
    snd : T
open _^_

-- conditional
-- "->"

data ⊥ : Set where

-- negated formula
¬_ : Set → Set
¬ A = A → ⊥
infix 3 ¬_

¬-elim : ∀ {A : Set}
  → ¬ A
  →   A
    ---
  →   ⊥
¬-elim ¬a a = ¬a a

⊥-elim : {A : Set} -> ⊥ -> A
⊥-elim ()

{-
--------------------------------------------------
-- p 18 2.4 Logical calculi
minimal, intuitionistic, classical - each stronger than previous
subscript ₀ indicates the propositional fragment
subscript ₁               predicate
Gentzen J₁ = LHJ : Logistic (i.e., axiomatic) calculus; Hilbert; J = Intuitionistic
        K₁ = LHK                                                 K = Classical

-------------------------
-- p 19 2.4.1 Minimal logic
-- M₀
-}

pl1 : {A : Set} -> A -> (A ^ A)
pl1 a = a , a

pl2 : {A B : Set} -> (A ^ B) -> (B ^ A)
pl2 (a , b) = b , a
^-swap = pl2

pl3 : {A B C : Set} -> (A -> B) -> ((A ^ C) -> (B ^ C))
pl3 a→b = λ (a , c) -> (a→b a , c)

-- diagrammatic composition
pl4 : {A B C : Set} -> ((A -> B) ^ (B -> C)) -> (A -> C)
pl4 (a→b , b→c) = λ a -> b→c (a→b a)

pl5 : {A B : Set} -> B -> (A -> B)
pl5 b = λ a -> b
const = pl5

pl6 : {A B : Set} -> (A ^ (A -> B)) -> B
pl6 (a , a→b) = a→b a

pl7 : {A B : Set} -> A -> (A v B)
pl7 a = inl a

pl8 : {A B : Set} -> (A v B) -> (B v A)
pl8 (inl a) = inr a
pl8 (inr b) = inl b
v-swap = pl8

pl9 : {A B C : Set} -> ((A -> C) ^ (B -> C)) -> ((A v B) -> C)
pl9 (a→c , b→c) = λ { (inl a) → a→c a ; (inr b) → b→c b}
v-elim = pl9

pl10 : {A B : Set} -> ((A -> B) ^ (A -> ¬ B)) -> ¬ A
pl10 (a→b , a→¬b) = λ a → (a→¬b a) (a→b a)

-------------------------
-- p 19 2.4.2 Intuitionistic logic
-- J₀

pl11 : {A B : Set} -> ¬ A -> (A -> B)
pl11 ¬a a
  with ¬-elim ¬a
... | a→⊥
  with a→⊥ a
...| ⊥
  = ⊥-elim ⊥

-------------------------
-- p 19 2.4.3 Classical logic
-- K₀

postulate -- cannot be defined constructively
  pl12 : {A : Set} -> ¬ ¬ A -> A

{-
--------------------------------------------------
-- p 20 2.5 Inference rules

All three calculi above have ONE inference rule : modus ponens:
- from A and A -> B infer B
-}

-- derive p1 -> (p2 v p1) in M₀
d1 : {p1 p2 : Set} -> (p1 -> (p1 v p2)) -> (p1 -> (p2 v p1))
d1 p1→p1vp2 = λ p1' -> v-swap (p1→p1vp2 p1') -- TODO

-- p 22
d2 : {C D : Set} -> C -> D -> (C ^ D)
d2 {C} {D} c d                   -- 1.
  with pl5 {D} {C} c             -- 2.
...| D→C                         -- 3.
  with pl3 {D} {C} {D} D→C       -- 4.
...| D^D→C^D                     -- 5.
  with pl1 d                     -- 6. 7.
...| D^D                         -- 8.
  = D^D→C^D D^D                  -- 9.

-- transitivity of "->" (aka diagrammatic composition)
d3 : {A B C : Set} -> (A -> B) -> (B -> C) -> (A -> C)
d3 {A} {B} {C} A→B B→C           -- 1. 2.
  with d2 A→B B→C                -- 3.
...| A→B^B→C
  = pl4 {A} {B} {C} A→B^B→C      -- 4. 5.

^-intro : {A B : Set} -> A -> B -> (A ^ B)
^-intro a b = a , b

->-trans = d3
_>>_ = ->-trans

-- p 23

d1' : {A B : Set} -> (A -> (B v A))
d1' = ->-trans pl7 pl8

-- problem 2.7

e1 : {A B C : Set}
  -> ((A -> (B -> A)) -> ((A ^ B) -> ((B -> A) ^ B)))
  -> A
e1 {A} {B} {C} pl3'
  with pl5 {B} {A}
...| A→B→A
  with pl3' A→B→A
...| A^B→-B→A-^B
  with pl6 {B} {A}
...| B^-B→A-→A
  with ->-trans {_} {_} {A} A→B→A {!!}
...| xxx
  = {!!}
 where
  foo : ((A ^ (A -> B)) -> B) -> (((A -> B) ^ A) -> B)
  foo = λ z z₁ → z (snd z₁ , _^_.fst z₁)

-- the prime versions are not axiomatic
e1' : {A B : Set} -> (A ^ B) -> A
e1' (a , _) = a

e2' : {A : Set} -> A -> A
e2' a = a

e3' : {A B : Set} -> (A ^ B) -> B
e3' (_ , b) = b

e4' : {A B : Set} -> B -> A -> (A ^ B)
e4' {A} {B} b a = ^-intro a b

e5' : {A B : Set} -> B -> A -> B
e5' = pl5

e6' : {A B C : Set} -> (A -> (B ^ C)) -> (A -> (C ^ B))
e6' = λ a→b^c a → pl2 (a→b^c a)

e7' : {A B C D : Set} -> (A -> (B -> C)) -> (A -> (C -> D)) -> (A -> (B -> D))
e7' a→b→c a→c→d = λ a b → a→c→d a (a→b→c a b)

e8' : {A B C D : Set} -> ((A -> B) ^ (C -> D)) -> ((A ^ C) -> (B ^ D))
e8' (a→b , c→d) = λ (a , c) -> (a→b a , c→d c)

e9' : {A B C D : Set} -> ((A -> B) ^ (A -> C)) -> (A -> (B ^ C))
e9' (a→b , a→c) = λ a -> (a→b a , a→c a)

e10' : {A B C : Set} -> (A -> (B ^ C)) -> ((A -> B) ^ (A -> C))
e10' a→b^c = (λ a -> fst (a→b^c a)) , (λ a → snd (a→b^c a))

e11' : {A B C : Set} -> (B ^ (A -> C)) -> (A -> (B ^ C))
e11' (b , a→c) = λ a -> (b , a→c a)

e12' : {A B : Set} -> B -> (A -> (A ^ B))
e12' = e4'

e13' : {A B C : Set} -> (A -> (B -> C)) -> ((A ^ B) -> C)
e13' a→b→c = λ a^b → a→b→c (fst a^b) (snd a^b)

e14' : {A B C : Set} -> ((A ^ B) -> C) -> (A -> (B -> C))
e14' a^b→c = λ a b -> a^b→c (a , b)

theorem-2-8 : {A B C : Set} -> (A -> (B -> C)) -> ((A -> B) -> (A -> C))
theorem-2-8 a→b→c = λ a→b a → a→b→c a (a→b a)

{-
--------------------------------------------------
-- p 24 2.6 Derivations from assumptions and provability

Proving theorems in an axiomatic system is difficult.

--------------------------------------------------
-- p 26 2.7 Proofs by induction



-}
