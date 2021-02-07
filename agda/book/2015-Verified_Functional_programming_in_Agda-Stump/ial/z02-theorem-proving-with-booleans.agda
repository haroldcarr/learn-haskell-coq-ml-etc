module z02-theorem-proving-with-booleans where

open import bool
open import eq
open import level

-- p 28
~~tt : ~ ~ tt ≡ tt
~~tt = refl

-- p 32
~~ff : ~ ~ ff ≡ ff
~~ff = refl

-- p 33
~~-elim : ∀ (b : 𝔹) → ~ ~ b ≡ b
~~-elim tt = refl
~~-elim ff = refl

-- p 34
~~-elim2 : ∀ (b : 𝔹) → ~ ~ b ≡ b
~~-elim2 tt = ~~tt
~~-elim2 ff = ~~ff

{-
-- p 34 - does not typecheck (on purpose) because of IMPLICIT ARGS
-- IMPLICIT ARGS: stated inside curly brackets (like IF previously)
~~-elim3 : ∀ (b : 𝔹) → ~ ~ b ≡ b
~~-elim3 tt = ~~ff
~~-elim3 ff = ~~tt

'refl' has implicit args
- the expression 'x' in 'x ≡ x' (e.g., in ~~tt the implicit is 'tt')
  - in ~~-elim3 implicit arg 'tt' does not match '~~ff'
-}

{-
-- p 35 : HOLES

~~-elim4 : ∀ (b : 𝔹) → ~ ~ b ≡ b
~~-elim4 tt = ?
~~-elim4 ff = ?
-}

-- p 36
&&-same : ∀ (b : 𝔹) → b && b ≡ b
&&-same tt = refl
&&-same ff = refl

-- infer type of 'b'
&&-same2 : ∀ b → b && b ≡ b
&&-same2 tt = refl
&&-same2 ff = refl

-- 'b' as implicit argument
&&-same3 : ∀ {b} → b && b ≡ b
&&-same3 {tt} = refl
&&-same3 {ff} = refl

-- p 37
-- so no need to supply implicit arg to &&-same3
test-&&-same : tt && tt ≡ tt
test-&&-same = &&-same3

-- ok to explicitly supply implicit arg
test-&&-same2 : tt && tt ≡ tt
test-&&-same2 = &&-same3 {tt}

-- p 38
||-same : ∀ {b} → b || b ≡ b
||-same {tt} = refl
||-same {ff} = refl

------------------------------------------------------------------------------
-- p. 38 2.4 Theorems with Hypotheses

{-
to express an assumption, write fun that
- takes a proof of assumption
- returns proof of desired result
-}

-- if b1 || b2 is false, then b1 must be false
||≡ff₁ : ∀ {b1 b2}
       → b1 || b2 ≡ ff -- assumption
       → b1       ≡ ff -- desired result

-- 1st (implicit) arg is 'ff'
-- 2nd (explicit) arg is ((b1 || b2) ≡ ff) bound to 'p' (but not used)
||≡ff₁ {ff} p = refl -- b1 = ff ≡ ff

-- absurd pattern '()'
||≡ff₁ {tt} ()       -- no '=' sign because impossible

{-
To prove the impossible, but only if some impossible assumption is true,
use absurd, since that impossible assumption can never be satisfied by a use of this theorem.
-}

-- p 41 alternative

||≡ff₁-2 : ∀ {b1 b2} → b1 || b2 ≡ ff → b1 ≡ ff
||≡ff₁-2 {ff} p = refl
||≡ff₁-2 {tt} p = p -- 'p' is equal to 'tt ≡ ff' -- the desired result of 'b1' instantiated to 'tt'

-- prefer version that uses absurd -- more robust to later changes

------------------------------------------------------------------------------
-- p 42 2.4.3 Matching on equality proofs

{-
special case on CONGRUENCE
if X equals Y then any TYPE containing X is equal to that same TYPE with X's replaced by Y

Congruence holds for definitional equality automatically.
Congruence must be proved for PROPOSITIONAL equalities 'a ≡ b'
-}

||-cong₁ : ∀ {b1 b1’ b2}        -- implicit args
         → b1       ≡ b1’       -- explicit arg
         → b1 || b2 ≡ b1’ || b2 -- desired result
||-cong₁ refl = refl

{-
-- p 43

only one way to construct an equality proof : via 'refl' constructor

refl can only prove something of the form  x ≡ x
- so Agda deduces b1 must be exactly b1’

Agda uses definitional
- knows b1 must be definitionally equal to b1’
- so refines the desired type using this definitional equality

So only need to prove : ((b1 || b2) ≡ (b1 || b2)) -- b1' replaced with b1 since equal
- via 'refl'

------------------------------------------------------------------------------
REFL

Important rule about using type refinements (refl).

Not allowed in such a way that non-variable terms are discovered
 to be definitionally equal to other such terms.

e.g.,

two non-variable terms are definitionally equal:
  b1 || b2
  ff

------------------------------------------------------------------------------
DOT PATTERN

b1, b1’, and b2 are implicit arguments above.

To include them in the pattern on the left-hand side of the equation:

||-cong₁ : ∀ {b1 b1’ b2}        -- implicit args
         → b1       ≡ b1’       -- explicit arg
         → b1 || b2 ≡ b1’ || b2 -- desired result
||-cong₁ {b1}{b1’}{b2} refl = refl

but Agda will complain because to type check this pattern, b1 and b1' can NOT be distinct.
Agda deduces b1 and b1’ to be definitionally equal.

Workaround : dot pattern:

||-cong₁ : ∀ {b1 b1’ b2}        -- implicit args
         → b1       ≡ b1’       -- explicit arg
         → b1 || b2 ≡ b1’ || b2 -- desired result
||-cong₁ {b1}{.b1}{b2} refl = refl

Writing “.” in front of term in pattern tells Agda that term is not a subpattern to match.
Rather it has a form which is dictated by the type of the whole pattern.
-}

------------------------------------------------------------------------------
-- p 44 2.4.4 The REWRITE directive

||-cong₂ : ∀ {b1 b2 b2'}
         → b2       ≡ b2'
         → b1 || b2 ≡ b1 || b2'
||-cong₂ p rewrite p = refl

{-
pattern variable p : with type b2 ≡ b2’

rewrite every occurrence of the left-hand side,
as it appears in the current goal,
with the right-hand side.

goal is formula to prove on right-hand side of current equation

rewrite p : transform the b2 (the left side of p) in
   b1 || b2  ≡ b1 || b2'
to
         b2' (the right side of p)
so goal (the type Agda is checking against the right-hand side of the equation) becomes
   b1 || b2’ ≡ b1 || b2’

REWRITE

given proof 'p' of some equation X ≡ Y
(any expression which has X ≡ Y as its type, e.g., call to lemma of that type)
look in goal
for any occurrences of X
transform those into Y

X and Y can be complex expressions (not just variables)

------------------------------------------------------------------------------
-- p 44 2.5 Further examples

proving a polymorphic theorem

for every type level ℓ
and every type A at that level
and for all booleans
the given ITE holds
-}

ite-same : ∀ {ℓ} {A : Set ℓ}    -- two implicit args
                                --     so no patterns needed on left hand side of equations
         → ∀ (b : 𝔹) (x : A)    -- two explicit args
         → if b then x else x
         ≡ x
-- reason by cases on 'b'
ite-same tt _ = refl -- x ≡ x
ite-same ff _ = refl

------------------------------------------------------------------------------
-- p 45

B-contra : ff ≡ tt           -- if ff ≡ tt holds
         → (∀ {P : Set} → P) -- then can prove anything
                             --      every formula P is true (which is NOT true)
B-contra ()

------------------------------------------------------------------------------
-- p 46 SUMMARY

{-
- equality TYPE t ≡ t’ expresses equality of expressions t and t’, which must have the same type

- prove equality with refl; will prove t ≡ t' for any definitionally equal t and t’

- two exprs definitionally equal if they can be proved equal using their defining equations

- proof is program defined using pattern matching

- Pattern matching can instantiate universally quantified variables which appear in the type.
  Has the effect of specializing the desired formula to the specific instances of those variables.

- Args can be declared implicit with curly braces in the type.
  Can pattern match on implicit arguments,
  using curly braces around the pattern on left-hand side of equation

- assumptions in theorems are args to programs which serve as proofs of those theorems

- absurd pattern can be used when an assumption is definitionally equal to an impossible equality,
  where the equated terms start with different constructors

- When using refl as the pattern for an equality assumption t ≡ t’,
  Agda will try to refine the type by making t and t’ definitionally equal.
-}

------------------------------------------------------------------------------
-- p 47 Exercises

-- 1

&&-idem : ∀ {b}         -- implicit arg
        → b && b ≡ b    -- result
&&-idem {tt} = refl
&&-idem {ff} = refl

||-idem : ∀ {b}         -- implicit arg
        → b || b ≡ b    -- result
||-idem {tt} = refl
||-idem {ff} = refl

||≡ff₂ : ∀ {b1 b2}      -- implicit args
       → b1 || b2 ≡ ff  -- explicit arg
       →       b2 ≡ ff  -- result
||≡ff₂ {ff}{ff} _ = refl
{-
||≡ff₂ {tt} ()
||≡ff₂ {ff}{tt} ()
-}

||-tt : ∀ (b : 𝔹)       -- explicit arg
      → b || tt ≡ tt    -- result
||-tt tt = refl
||-tt ff = refl

ite-arg : ∀{ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'}                    -- implicit args
        → (f : A → B) (b : 𝔹) (x y : A)                       -- explicit args
        → (f (if b then x else y)) ≡ (if b then f x else f y) -- result
ite-arg _ tt _ _ = refl
ite-arg _ ff _ _ = refl

𝔹-contra : ff ≡ tt
         → ∀{ℓ} {P : Set ℓ}
         → P
𝔹-contra ()
{-
open import sum
||-split : ∀ {b b' : 𝔹}
         → b || b' ≡ tt
         → b ≡ tt ⊎ b' ≡ tt
||-split{tt}{tt} p = inj₁ refl
||-split{tt}{ff} p = inj₁ refl
||-split{ff}{tt} p = inj₂ refl
||-split{ff}{ff} ()

𝔹-dec : ∀ (b : 𝔹)
      → b ≡ tt ⊎ b ≡ ff
𝔹-dec tt = inj₁ refl
𝔹-dec ff = inj₂ refl
-}
&&-snd : {p1 p2 : 𝔹}    -- implicit args
       → p1 && p2 ≡ tt  -- explicit arg
       →       p2 ≡ tt  -- result
&&-snd {tt} {tt} _ = refl
--&&-snd{ff} ()

&&-fst : {p1 p2 : 𝔹}
       → p1 && p2 ≡ tt
       → p1       ≡ tt
&&-fst {tt} {tt} _ = refl
-- &&-fst{ff} ()

&&-combo : {p1 p2 : 𝔹}         -- implicit
         → p1       ≡ tt       -- explcit
         →       p2 ≡ tt       -- explcit
         → p1 && p2 ≡ tt       -- result
&&-combo {tt} {tt} _ _ = refl
{- alternative
&&-combo{tt} pr1 pr2 = pr2
&&-combo{ff} pr1 pr2 = 𝔹-contra pr1
-}

&&-ff : ∀(b : 𝔹)
      → b && ff ≡ ff
&&-ff tt = refl
&&-ff ff = refl

------------------------------------------------------------------------------
-- 2

ff-imp : ∀ (b : 𝔹)
       → ff imp b ≡ tt
ff-imp tt = refl
ff-imp ff = refl

imp-tt : ∀ (b : 𝔹)
       → b imp tt ≡ tt
imp-tt tt = refl
imp-tt ff = refl

imp-ff : ∀ (b : 𝔹)
       → b imp ff ≡ ~ b
imp-ff tt = refl
imp-ff ff = refl

tt-imp : ∀ (b : 𝔹)
       → tt imp b ≡ b
tt-imp tt = refl
tt-imp ff = refl

&&-tt : ∀ (b : 𝔹)
      → b && tt ≡ b
&&-tt tt = refl
&&-tt ff = refl

||-ff : ∀ (b : 𝔹)
      → b || ff ≡ b
||-ff tt = refl
||-ff ff = refl

&&-contra : ∀ (b : 𝔹)
          → b && ~ b ≡ ff
&&-contra tt = refl
&&-contra ff = refl

&&-comm : ∀ (b1 b2 : 𝔹)
        → b1 && b2 ≡ b2 && b1
&&-comm ff ff = refl
&&-comm ff tt = refl
&&-comm tt ff = refl
&&-comm tt tt = refl

||-comm : ∀ (b1 b2 : 𝔹)
        → b1 || b2 ≡ b2 || b1
||-comm ff ff = refl
||-comm ff tt = refl
||-comm tt ff = refl
||-comm tt tt = refl

&&-assoc : ∀ (b1 b2 b3 : 𝔹)
         → b1 && (b2 && b3) ≡ (b1 && b2) && b3
&&-assoc ff ff ff = refl
&&-assoc ff ff tt = refl
&&-assoc ff tt ff = refl
&&-assoc ff tt tt = refl
&&-assoc tt ff ff = refl
&&-assoc tt ff tt = refl
&&-assoc tt tt ff = refl
&&-assoc tt tt tt = refl

||-assoc : ∀ (b1 b2 b3 : 𝔹)
         → b1 || (b2 || b3) ≡ (b1 || b2) || b3
||-assoc ff ff ff = refl
||-assoc ff ff tt = refl
||-assoc ff tt ff = refl
||-assoc ff tt tt = refl
||-assoc tt ff ff = refl
||-assoc tt ff tt = refl
||-assoc tt tt ff = refl
||-assoc tt tt tt = refl

~-over-&& : ∀ (b1 b2 : 𝔹)
          → ~ ( b1 && b2 ) ≡ (~ b1 || ~ b2)
~-over-&& ff ff = refl
~-over-&& ff tt = refl
~-over-&& tt ff = refl
~-over-&& tt tt = refl

~-over-|| : ∀ (b1 b2 : 𝔹)
          → ~ ( b1 || b2 ) ≡ (~ b1 && ~ b2)
~-over-|| ff ff = refl
~-over-|| ff tt = refl
~-over-|| tt ff = refl
~-over-|| tt tt = refl

&&-over-||-l : ∀ (a b c : 𝔹)
             → a && (b || c) ≡ (a && b) || (a && c)
&&-over-||-l ff ff ff = refl
&&-over-||-l ff ff tt = refl
&&-over-||-l ff tt ff = refl
&&-over-||-l ff tt tt = refl
&&-over-||-l tt ff ff = refl
&&-over-||-l tt ff tt = refl
&&-over-||-l tt tt ff = refl
&&-over-||-l tt tt tt = refl
{-
&&-over-||-r : ∀ (a b c : 𝔹) → (a || b) && c ≡ (a && c) || (b && c)
||-over-&&-l : ∀ (a b c : 𝔹) → a || (b && c) ≡ (a || b) && (a || c)
||-over-&&-r : ∀ (a b c : 𝔹) → (a && b) || c ≡ (a || c) && (b || c)
&&-cong₁ : ∀ {b1 b1' b2 : 𝔹} → b1 ≡ b1' → b1 && b2 ≡ b1' && b2
&&-cong₂ : ∀ {b1 b2 b2' : 𝔹} → b2 ≡ b2' → b1 && b2 ≡ b1 && b2'
&&-intro : ∀ {b1 b2 : 𝔹} → b1 ≡ tt → b2 ≡ tt → b1 && b2 ≡ tt
||-intro1 : ∀ {b1 b2 : 𝔹} → b1 ≡ tt → b1 || b2 ≡ tt
--&&-elim : ∀ {b1 b2 : 𝔹} → b1 && b2 ≡ tt → b1 ≡ tt ∧ b2 ≡ tt
&&-elim1 : ∀ {b1 b2 : 𝔹} → b1 && b2 ≡ tt → b1 ≡ tt
&&-elim2 : ∀ {b1 b2 : 𝔹} → b1 && b2 ≡ tt → b2 ≡ tt
--||-elim : ∀ {b1 b2 : 𝔹} → b1 || b2 ≡ tt → b1 ≡ tt ∨ b2 ≡ tt
-}
~-cong : ∀ {b b' : 𝔹}
       →   b ≡   b'
       → ~ b ≡ ~ b'
~-cong refl = refl

ite-cong₁ : ∀{ℓ}{A : Set ℓ} {b b' : 𝔹}
            (x y : A)
          →     b                ≡     b'
          → (if b then x else y) ≡ (if b' then x else y)
ite-cong₁ _ _ refl = refl

ite-cong₂ : ∀{ℓ} {A : Set ℓ}
            (b : 𝔹)
            {x x' : A}
            (y : A)
          →            x         ≡            x'
          → (if b then x else y) ≡ (if b then x' else y)
ite-cong₂ _ _ refl = refl

ite-cong₃ : ∀{ℓ}{A : Set ℓ}
            (b : 𝔹)
            (x : A)
            {y y' : A}
          →                   y  ≡                   y'
          → (if b then x else y) ≡ (if b then x else y')
ite-cong₃ _ _ refl = refl

--&&-split : ∀ {b b' : 𝔹} → b || b' ≡ ff → b ≡ ff ⊎ b' ≡ ff
{-
imp-same : ∀ (b : 𝔹) → b imp b ≡ tt
imp-to-|| : ∀ (b1 b2 : 𝔹) → (b1 imp b2) ≡ (~ b1 || b2)
imp-mp : ∀ {b b' : 𝔹} → b imp b' ≡ tt → b ≡ tt → b' ≡ tt
imp-antisymm : ∀ {b1 b2 : 𝔹} → b1 imp b2 ≡ tt → b2 imp b1 ≡ tt → b1 ≡ b2
ff-xor : ∀ (b : 𝔹) → ff xor b ≡ b
tt-xor : ∀ (b : 𝔹) → tt xor b ≡ ~ b
~-xor-distrb : ∀ (a b : 𝔹) → ~ (a xor b) ≡ ~ a xor b
xor-distrib-&& : ∀ (x y : 𝔹) → x xor (y && x) ≡ ~ y && x
xor~hop : ∀ (a b : 𝔹) → ~ a xor b ≡ a xor ~ b
xor-comm : ∀ (b1 b2 : 𝔹) → b1 xor b2 ≡ b2 xor b1
xor-assoc : (b1 b2 b3 : 𝔹) → b1 xor (b2 xor b3) ≡ (b1 xor b2) xor b3
xor-anti-idem : (b : 𝔹) → b xor b ≡ ff
xor-≡ : {b1 b2 : 𝔹} → b1 xor b2 ≡ ff → b1 ≡ b2
nor-not : ∀ (b : 𝔹) → b nor b ≡ ~ b
nor-or : ∀ (b1 b2 : 𝔹) → (b1 nor b2) nor (b1 nor b2) ≡ b1 || b2
nor-and : ∀ (b1 b2 : 𝔹) → (b1 nor b1) nor (b2 nor b2) ≡ b1 && b2
nor-comm : ∀ (b1 b2 : 𝔹) → b1 nor b2 ≡ b2 nor b1
nand-comm : ∀ (b1 b2 : 𝔹) → b1 nand b2 ≡ b2 nand b1
-}
------------------------------------------------------------------------------
-- 3

z02-3a : tt ≡ tt
z02-3a = refl

z02-3b : ff ≡ ff
z02-3b = refl

-- NO z02-3c : ff ≡ tt

z02-3d : ff && ff ≡ ~ tt
z02-3d = refl

z02-3e : ∀ (x : 𝔹)
       → tt && x
       ≡ x
z02-3e _ = refl

z02-3f : ∀ (x : 𝔹)
       → x && tt
       ≡ x
z02-3f tt = refl
z02-3f ff = refl


