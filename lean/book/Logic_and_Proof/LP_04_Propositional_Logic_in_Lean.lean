------------------------------------------------------------------------------
-- 4.1 Expressions for Propositions and Proofs

-- Declare variables.
variables A B C D : Prop

-- Check the type of an expression.
#check A ∧ ¬ B → C

-- Declare that a hypothesis is true (in this case A ∨ ¬ B).
-- Any proposition can be viewed as a type, the type of proof of that proposition.
-- A hypothesis, or premise, is just a variable of tyhat type.
-- Build proofs by creating expressions of the correct type.
variable  h₀ : A ∧ ¬ B
#check h₀

-- left elimination says A is true
#check and.left  h₀
-- right elimination says ¬ B is true
#check and.right h₀
-- construct ¬ B ∧ A
#check and.intro (and.right h₀) (and.left h₀)

-- implication-elimination (modus ponens) is application (associates to left)
/-    A → B     A
      ------------ →E
          B
-/
section
  example : A ∧ (A → B) → B :=
  λ   (h₁ : A ∧ (A → B))
, have h₂ : A    , from and.left  h₁
, have h₃ : A → B, from and.right h₁
, show B         , from h₃ h₂ -- APPLICATION
end

variable h₁' : A → (B → C)
variable h₂' : D → A
variable h₃' : D
variable h₄' : B

#check h₂' h₃'
#check h₁' (h₂' h₃')
#check (h₁' (h₂' h₃')) h₄'

-- Implication-introduction via assume (note: can cancel a hypothesis).
/-
----- 1
  A
  .
  .
  B
----- 1 →I
A → B
-/
-- A and B have type Prop
-- h is premise that A holds
-- P is proof of B, possibly involving h.
-- expression 'assume h : A, P'
-- proof of A → B
-- e.g., proof of A → A ∧ A
#check (assume h : A, and.intro h h)

-- Above, do no need declare A as a premise.
-- 'assume" makes premise local to expression in parens (parens not always needed):
#check assume h : A, and.intro h h

-- proof of A ∧ ¬ B → ¬ B ∧ A
#check (assume h : A ∧ ¬ B, and.intro (and.right h) (and.left h))

-- 'assume' is alternative to lambda
#check (λ h : A ∧ ¬ B, and.intro (and.right h) (and.left h))

------------------------------------------------------------------------------
-- 4.2 More commands

-- 'example' says you are proving a theorem of a given type, followed by a proof (an expression)
-- Lean type checks the expression.

example : A ∧ ¬ B → ¬ B ∧ A :=
assume h : A ∧ ¬ B, and.intro (and.right h) (and.left h)

-- 'example' provides type info, so can omit info, e.g., type of assumption:

example : A ∧ ¬ B → ¬ B ∧ A :=
assume h, and.intro (and.right h) (and.left h)

-- Lean knows proving implication with premise A ∧ ¬ B.
-- Infers h labels assumption A ∧ ¬ B.

-- Can also give more info via 'show'.
-- If A is proposition and P is proof, then 'show A, from P' means same thing as P alone,
-- but signals intention that P is a proof of A.
-- Lean confirms P is a proof of A, before parsing surrounding expression).

example : A ∧ ¬ B → ¬ B ∧ A :=
assume
  h  : A ∧ ¬ B
, show ¬ B ∧ A
, from and.intro (and.right h) (and.left h)

example : A ∧ ¬ B → ¬ B ∧ A :=
assume
  h  : A ∧ ¬ B
, show ¬ B ∧ A
, from and.intro
         (show ¬ B, from and.right h)
         (show A  , from and.left  h)

-- rather than declare variables and premises, present them as "arguments"
example (A B : Prop) : A ∧ ¬ B → ¬ B ∧ A :=
assume
  h : A ∧ ¬ B
, show ¬ B ∧ A
, from and.intro (and.right h) (and.left h)

-- SORRY (e.g., like Haskell's 'undefined')
-- Provides proof of anything.
-- Helps to construct proofs incrementally.

example : A ∧ ¬ B → ¬ B ∧ A :=
assume h, sorry

example : A ∧ ¬ B → ¬ B ∧ A :=
assume h, and.intro sorry sorry

example : A ∧ ¬ B → ¬ B ∧ A :=
assume h, and.intro (and.right h) sorry

example : A ∧ ¬ B → ¬ B ∧ A :=
assume h, and.intro (and.right h) (and.left h)

-- PLACEHOLDERS (i.e., "holes")

-- UNDERSCORE
-- ask Lean to fill in value from the context.
-- _ for proof : error msg saying what is missing.
-- Helps write proof terms in a backward-driven fashion.
-- Above, try replacing 'sorry' by _.

-- Delimit scope of variables or premises introduced via 'variables', put them in a block that begins 'section' and ends 'end'.

------------------------------------------------------------------------------
-- 4.3 Building Natural Deduction Proofs


-- IMPLICATION

-- implication introduction : 'assume' (or lambda)
-- implication elimination  : application

example : A → B :=
assume h : A, show B, from sorry

section
  variable h₁ : A → B
  variable h₂ : A

  example : B := h₁ h₂
end

-- CONJUNCTION

-- and-introduction : and.intro
/-
    A   B
   ------- ∧I
    A ∧ B
-/

section
  variables (h₁ : A) (h₂ : B)

  example : A ∧ B := and.intro h₁ h₂
end

-- and-elimination  : and.left and.right
/-
    A ∧ B                      A ∧ B
   ------- ∧E left            ------- ∧E right
    A                              B
-/

section
  variable h : A ∧ B

  example : A := and.left  h
  example : B := and.right h
end

-- DISJUNCTION

-- or-introduction : or.inl or.inr
/-
  A                  B
----- ∨I left      ----- ∨I right
A ∨ B              A ∨ B
-/

section
  variable h : A

  example : A ∨ B := or.inl h
end

section
  variable h : B

  example : A ∨ B := or.inr h
end

-- or-elimination
/-
          --- 1  --- 1
           A      B
           .      .
           .      .
A ∨ B      C      C
------------------- 1 ∨E
          C
-/
-- prove C from A ∨ B
-- need three arguments
--   proof h' of A ∨ B
--   proof of C from A
--   proof of C from B
-- Note : can reuse local label h₁ in each branch
section
  variable h' : A ∨ B
  variables (ha : A → C) (hb : B → C)
  example : C :=
  or.elim
    h'
    (assume h1 : A, show C, from ha h1)
    (assume h1 : B, show C, from hb h1)
end

example (A B C : Prop) : C :=
  have h : A ∨ B, from sorry
, show C
, from
  or.elim
    h
    (assume h₁ : A, show C, from sorry)
    (assume h₂ : B, show C, from sorry)

-- NEGATION

-- negation ¬ A is defined by A → false, says A implies something impossible.
-- So rules for negation similar to implication.
-- To prove ¬ A, assume A and derive a contradiction.

-- There is no introduction rule.
-- "false" is false.
-- There is no way to prove it, other than extract it from contradictory hypotheses.
/-
    A
    .
    .
    ⊥
   --- 1 ¬I  -- does not exist
   ¬A
-/

section
  example : ¬ A :=
  assume h : A,
  show false, from sorry
end

-- After proving ¬ A, get a contradiction by applying it to a proof of A.

-- not-elimination
/-
¬A    A
------- ¬E
   ⊥
-/
section
  variable h₁ : ¬ A
  variable h₂ : A
  example : false := h₁ h₂
end

-- Can conclude anything from a contradiction.
-- "ex falso sequitur quodlibet" : anything you want follows from falsity"
/-
 ⊥
--- ⊥E
 A
-/

-- TRUTH and FALSITY

-- false-elimination : ex falso rule : false.elim
/-
 ⊥
--- ⊥E
 A
-/

section
  variable h' : false
  example : A := false.elim h'
end

-- true is true
-- true-introduction
/-
--- ⊤I
 ⊤
-/

example : true := trivial

-- BI-IMPLICATION

-- bi-implication-introduction : "if and only if" : iff.intro
/-
--- 1    --- 1
 A        B
 .        .
 .        .
 B        A
------------ 1 ↔I
   A ↔ B
-/
example : A ↔ B :=
iff.intro
  (assume h : A,
    show B, from sorry)
  (assume h : B,
    show A, from sorry)

-- Note h used in scoped branches

-- bi-implication elimination iff.elim_left iff.elim_right
/-
A ↔ B     A       A ↔ B     B
----------- ↔El   ----------- ↔Er
    B             A
-/
section
  variable h₁ : A ↔ B
  variable h₂ : A

  example : B := iff.elim_left h₁ h₂
end

section
  variable h₁ : A ↔ B
  variable h₂ : B

  example : A := iff.elim_right h₁ h₂
end

-- abbreviations
-- iff.mp  for iff.elim_left  ("mp" : "modus ponens)
-- iff.mpr for iff.elim_right ("modus ponens reverse)

-- Reductio ad absurdum (PROOF BY CONTRADICTION)
/-
----- 1
 ¬A
  .
  .
  ⊥
----- 1 RAA
  A
-/
-- see Ch 5 for more detail
-- by_contradiction
-- one arg : proof of false from ¬ A

-- enable classical reasoning : open classical

section
  open classical

  example : A :=
  by_contradiction
    (assume h : ¬ A,
      show false, from sorry)
end

-- EXAMPLES

-- TRANSITIVITY

/-
--- 1
 A      A → B
-------------
     B          B → C
    ------------------
            C
          ----- 1
          A → C
-/

section
  variable h₁ : A → B
  variable h₂ : B → C
  example     : A → C :=
  assume   h  : A
, show          C
, from     h₂ (h₁ h)
end

section
  variable h₁ : A → B
  variable h₂ : B → C
  example     : A → C :=
  assume   h  : A
, have     h₃ : B, from h₁ h
, show     C     , from h₂ h₃
end

/-
                 ------ 1
                 A ∧ B
------------- 2  -----   -----
 A → (B → C)       A     A ∧ B
--------------------     -----
      B → C                B
      ----------------------
                 C
              ---------- 1
              A ∧ B → C
    ---------------------------- 2
    (A → (B → C)) → (A ∧ B → C)
-/

example (A B C : Prop) : (A → (B → C)) → (A ∧ B → C) :=
  assume h₁ : A → (B → C)
, assume h₂ : A ∧ B
, show   C
, from h₁ (and.left  h₂) -- proof of A
          (and.right h₂) -- proof of B

example (A B C : Prop) : (A → (B → C)) → (A ∧ B → C) :=
  assume h₁ : A → (B → C)
, assume h₂ : A ∧ B
, have   h₃ : A, from and.left h₂
, have   h₄ : B, from and.right h₂
, show   C     , from h₁ h₃ h₄

example (A B C : Prop) : (A → (B → C)) → (A ∧ B → C) :=
  assume h₁ : A → (B → C)
, assume h₂ : A ∧ B
, have   h₃ : A    , from and.left h₂
, have   h₄ : B    , from and.right h₂
, have   h₅ : B → C, from h₁ h₃
, show   C         , from h₅ h₄

-- DISTRIBUTION

/-
            ----------- 2             ----------- 2
            A ∧ (B ∨ C)               A ∧ (B ∨ C)
            -----------      -- 1     -----------    -- 1
                 A            B           A           C
----------- 2    --------------           -------------
A ∧ (B ∨ C)          A ∧ B                    A ∧ C
-----------    -----------------      -----------------
     B ∨ C     (A ∧ B) ∨ (A ∧ C)      (A ∧ B) ∨ (A ∧ C)
     -------------------------------------------------- 1
               (A ∧ B) ∨ (A ∧ C)
          ---------------------------------- 2
          (A ∧ (B ∨ C) → ((A ∧ B) ∨ (A ∧ C))
-/

-- see or-elimination description to understand
example (A B C : Prop) : A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C) :=
assume h₁ : A ∧ (B ∨ C)
, or.elim
    -- proof of (B ∨ C)
    (and.right h₁)
    -- proof of (A ∧ B) ∨ (A ∧ C)
    ( assume h₂ : B
    , show (A ∧ B) ∨ (A ∧ C)
    , from or.inl (and.intro      -- A ∧ B
                    (and.left h₁) -- A
                    h₂) )         -- B
    -- proof of (A ∧ B) ∨ (A ∧ C)
    ( assume h₂ : C
    , show (A ∧ B) ∨ (A ∧ C)
    , from or.inr (and.intro      -- A ∧ C
                    (and.left h₁) -- A
                    h₂) )         -- C

-- assume is λ
-- Lean can infer type of assumption
-- so same thing as above
example (A B C : Prop) : A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C) :=
λ h₁, or.elim
        (and.right h₁)
        (λ h₂, or.inl (and.intro (and.left h₁) h₂))
        (λ h₂, or.inr (and.intro (and.left h₁) h₂))

example (A B C : Prop) : A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C) :=
  assume h₁ : A ∧ (B ∨ C)
, have   h₂ : A    , from and.left  h₁
, have   h₃ : B ∨ C, from and.right h₁
, show (A ∧ B) ∨ (A ∧ C)
, from
  or.elim h₃
    ( assume h₄ : B
    , have  h₅ : A ∧ B, from and.intro h₂ h₄
    , show (A ∧ B) ∨ (A ∧ C), from or.inl h₅ )
    ( assume h₄ : C
    , have h₅ : A ∧ C, from and.intro h₂ h₄
    , show (A ∧ B) ∨ (A ∧ C), from or.inr h₅ )

------------------------------------------------------------------------------
-- 4.4 Forward Reasoning via 'have'
-- (hc: mixed in with proofs above)

-- AND COMMUTES (with 'have')
example (A B : Prop) : A ∧ B → B ∧ A :=
  assume h₁ : A ∧ B
, have   h₂ : A, from and.left  h₁
, have   h₃ : B, from and.right h₁
, show   B ∧ A , from and.intro h₃ h₂

-- AND COMMUTES (better without 'have')
example (A B : Prop) : A ∧ B → B ∧ A :=
  assume h₁ : A ∧ B
, show        B ∧ A
, from
  and.intro
    (show B, from and.right h₁)
    (show A, from and.left  h₁)

-- AMD COMMUTES (better, in this case)
example (A B : Prop) : A ∧ B → B ∧ A :=
λ h, and.intro (and.right h) (and.left h)

------------------------------------------------------------------------------
-- 4.5 Definitions and Theorems

-- name DEFINITIONS

def triple_and (A B C : Prop) : Prop := A ∧ (B ∧ C)

-- for later use, e.g.,

variables E F G : Prop
#check triple_and (D ∨ E) (¬ F → G) (¬ D)

def double (n : ℕ) : ℕ := n + n

-- name THEOREMS

theorem and_commute0 (A B : Prop) : A ∧ B → B ∧ A :=
assume h, and.intro (and.right h) (and.left h)

section
  variable h₁ : C ∧ ¬ D
  variable h₂ : ¬ D ∧ C → E
  example : E := h₂ (and_commute0 C (¬ D) h₁)
end

-- to obviate giving args C and ¬ D explicitly (because implicit in h₁), use 'implicits'

theorem and_commute1 {A B : Prop} : A ∧ B → B ∧ A :=
assume h, and.intro (and.right h) (and.left h)

-- squiggly : args A and B are implicit (Lean infers them)

section
  variable h₁ : C ∧ ¬ D
  variable h₂ : ¬ D ∧ C → E
  example : E := h₂ (and_commute1 h₁)
end

-- avoid 'assume' by making hypothesis into an arg
theorem and_commute2 {A B : Prop} (h : A ∧ B) : B ∧ A :=
and.intro (and.right h) (and.left h)

-- NAMESPACE

-- Lean’s defines or.resolve_left, or.resolve_right, and absurd.
-- Define again without clashing:
namespace hidden

-- variables {A B : Prop}

theorem or_resolve_left (h₁ : A ∨ B) (h₂ : ¬ A) : B :=
or.elim h₁
  (assume h₃ : A, show B, from false.elim (h₂ h₃))
  (assume h₃ : B, show B, from h₃)

theorem or_resolve_right (h₁ : A ∨ B) (h₂ : ¬ B) : A :=
or.elim h₁
  (assume h₃ : A, show A, from h₃)
  (assume h₃ : B, show A, from false.elim (h₂ h₃))

theorem absurd (h₁ : ¬ A) (h₂ : A) : B :=
false.elim (h₁ h₂)

end hidden

------------------------------------------------------------------------------
-- 4.6 Additional Syntax (for power users)

-- SUBSCRIPTS : h5 : h backslash 5 : h₅

-- OMIT ASSUME LABEL (i.e., "anonymous” hypothesis")
-- refer to last anonymous via THIS


example : A → A ∨ B :=
  assume : A
, show A ∨ B, from or.inl this

-- or via French quotes:

example : A → B → A ∧ B :=
assume : A,
assume : B,
show A ∧ B, from and.intro ‹A› ‹B›

-- 'have' without label via THIS and French quotes

theorem my_theorem {A B C : Prop} :
  A ∧ (B ∨ C) → (A ∧ B) ∨ (A ∧ C) :=
  assume h : A ∧ (B ∨ C)
, have A    , from and.left  h
, have B ∨ C, from and.right h
, show (A ∧ B) ∨ (A ∧ C)
, from
  or.elim ‹B ∨ C›
    ( assume : B
    , have A ∧ B, from and.intro ‹A› ‹B›
    , show (A ∧ B) ∨ (A ∧ C), from or.inl this )
    ( assume : C
    , have A ∧ C, from and.intro ‹A› ‹C›
    , show (A ∧ B) ∨ (A ∧ C), from or.inr this )

-- AND/OR shorthand

-- AND
--   h.left h.right instead of and.left h and.right h
--   ⟨h1, h2⟩ (using \< and \>) instead of and.intro h1 h2

example (A B : Prop) : A ∧ B → B ∧ A :=
assume h : A ∧ B,
show B ∧ A, from and.intro (and.right h) (and.left h)

example (A B : Prop) : A ∧ B → B ∧ A :=
assume h : A ∧ B,
show B ∧ A, from ⟨h.right, h.left⟩

example (A B : Prop) : A ∧ B → B ∧ A :=
assume h, ⟨h.right, h.left⟩

-- take apart a conjunction with ASSUME
example (A B : Prop) : A ∧ B → B ∧ A :=
assume ⟨h₁, h₂⟩, ⟨h₂, h₁⟩

-- if h is BICONDITIONAL
--   h.mp and h.mpr instead of iff.mp h and iff.mpr h
--   ⟨h1, h2⟩ instead of iff.intro h1 h2

example (A B : Prop) : B ∧ (A ↔ B) → A :=
assume ⟨hB, hAB⟩, hAB.mpr hB

example (A B : Prop) : A ∧ B ↔ B ∧ A :=
⟨ assume ⟨h₁, h₂⟩, ⟨h₂, h₁⟩
, assume ⟨h₁, h₂⟩, ⟨h₂, h₁⟩ ⟩

------------------------------------------------------------------------------
-- 4.7 Exercises

-- variables A B C D : Prop

example    : A ∧ (A → B) → B :=
  λ     (h : A ∧ (A → B))
, have   a : A    , from and.left  h
, have fab : A → B, from and.right h
, fab a -- show  B, from fab a

example : A → ¬ (¬ A ∧ B) :=
sorry

example : ¬ (A ∧ B) → (A → ¬ B) :=
sorry

-- HC : warmup for two following this one
example (h₁ : A) (h₂ : A → C) (h₃ : A → D) : C ∨ D :=
  have hc : C, from h₂ h₁
, or.inl hc
example (h₁ : A) (h₂ : A → C) (h₃ : A → D) : C ∨ D :=
  have hd : D, from h₃ h₁
, or.inr hd

-- from 3.4
/-
𝐴∨𝐵, 𝐴→𝐶, and 𝐵→𝐷, conclude 𝐶∨𝐷.
                 --- 1             --- 1
        A → C     A       B → D     B
        -----------       -----------
A ∨ B     C ∨ D             C ∨ D
--------------------------------- 1
          C ∨ D
-/
-- left version
section
  variables (hbc : B → C)
  example (h₁ : A ∨ B) (h₂ : A → C) (h₃ : B → D) : C ∨ D :=
  have hc : C, from or.elim
    h₁
    (λ (a : A), show C, from h₂  a)
    (λ (b : B), show C, from hbc b)
, or.inl hc
end

-- right version
section
  variables (had : A -> D)
  example (h₁ : A ∨ B) (h₂ : A → C) (h₃ : B → D) : C ∨ D :=
  have hc : D, from or.elim
    h₁
    (λ (a : A), show D, from had a)
    (λ (b : B), show D, from h₃  b)
, or.inr hc
end

-- HC : from 3 - the reverse of the example after this
/-
                  --- 1                    --- 2
                   A                         B
--------- 3     -------     -------- 3    -------
¬(A ∨ B)         A ∨ B      ¬(A ∨ B)       A ∨ B
-----------------------     ---------------------
           ⊥                          ⊥
          --- 1                      --- 2
          ¬A                         ¬B
          ------------------------------
                     ¬A ∧ ¬B
               ------------------
               ¬(A ∨ B) → ¬A ∧ ¬B
-/
/-
example (h : ¬ (A ∨ B)) : ¬ A ∧ ¬ B  :=
  λ (a : A) (b : B), show ¬ A ∧ ¬ B, from false.elim (h (or.inl a))

example (h : ¬ (A ∨ B)) : ¬ A ∧ ¬ B  :=
  λ (a : A)
, have aOrB₁ : A ∨ B, from or.inl a
, have x     : false, from h aOrB₁
, have notA  : ¬ A  , from false.elim x
, have notB  : ¬ B  , from false.elim x
, show ¬ A ∧ ¬ B    , from and.intro notA notB
-/

example (h : ¬ A ∧ ¬ B) : ¬ (A ∨ B) :=
sorry

example : ¬ (A ↔ ¬ A) :=
sorry
