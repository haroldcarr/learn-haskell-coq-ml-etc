/-
------------------------------------------------------------------------------
3. Propositions and Proofs

How to write mathematical assertions and proofs in the language of dependent type theory.

------------------------------------------------------------------------------
3.1. Propositions as Types

type, Prop, to represent propositions, and constructors to build new propositions from others.
-/

namespace ns31
constant and     : Prop → Prop → Prop
constant or      : Prop → Prop → Prop
constant not     : Prop → Prop
constant implies : Prop → Prop → Prop

variables p q r : Prop
#check and p q                      -- Prop
#check or (and p q) r               -- Prop
#check implies (and p q) (and q p)  -- Prop
end ns31

/-
For each element p : Prop, another type Proof p, for the type of proofs of p.
An “axiom” would be a constant of such a type.
-/

namespace ns31
constant Proof : Prop → Type

constant and_comm : Π p q : Prop,
  Proof (implies (and p q) (and q p))

variables p q : Prop
#check and_comm p q      -- Proof (implies (and p q) (and q p))
end ns31

-- Rules to build new proofs from old ones.

namespace ns31
constant modus_ponens :
  Π p q : Prop, Proof (implies p q) →  Proof p → Proof q
end ns31

/-
Assuming p as a hypothesis, we have a proof of q.
Then “cancel” the hypothesis p and obtain a proof of implies p q.
-/

namespace ns31
constant implies_intro :
  Π p q : Prop, (Proof p → Proof q) → Proof (implies p q).
end ns31

/-
Determining that an expression t is a correct proof of assertion p is checking that t has type Proof p.

Avoid writing the term Proof repeatedly by conflating Proof p with p itself.
Given p : Prop, interpret p as a type, namely, the type of its proofs.
Then t : p is an assertion that t is a proof of p.

Then the rules for implication mean can go back and forth between implies p q and p → q,
i.e., implication between propositions p and q corresponds to having a function that takes any element of p
to an element of q.

Therefore 'implies' is redundant: just use function p → q as implication.

As done in Calculus of Constructions and Lean.

CURRY-HOWARD ISOMORPHISM (PROPOSITIONS-AS-TYPES):
Rules for implication in a proof system for natural deduction correspond to the
rules governing abstraction and application for functions.

Prop is sugar for Sort 0 (bottom of type hierarchy).

Type u is sugar for Sort (u+1).

propositions as types
1. constructive : proposition p represents a sort of data type,
a specification of the type of data that constitutes a proof.
A proof of p is then simply an object t : p of the right type.

2. Coding trick. for eac proposition p we associate a type that is empty if p is false
and has a single element, say *, if p is true.
When true, then the type associated with p is inhabited.

Rules for function application and abstraction can keep track of which elements of Prop are inhabited.

Constructing an element t : p tells us that p is indeed true.

You can think of the inhabitant of p as being the “fact that p is true.”
A proof of p → q uses “the fact that p is true” to obtain “the fact that q is true.”

If p : Prop is any proposition, Lean’s treats any two elements t1 t2 : p as being definitionally equal,
much the same way as it treats (λ x, t)s and t[s/x] as definitionally equal.
This is known as proof irrelevance.
It means that even though we can treat proofs t : p as ordinary objects
in the language of dependent type theory, they carry no information beyond the fact that p is true.

Above 1 and 2 differ in a fundamental way.
1. constructive : proofs are abstract mathematical objects denoted by expressions in dependent type theory.
2. coding trick : expressions do not denote anything interesting.
The fact that we can write them down and check that they are well-typed that
ensures that the proposition in question is true. In other words, the expressions themselves are the proofs.

1. expression “constructs” or “produces” or “returns” a proof of a proposition
2. expression “is” such a proof.

bottom line:
To express a mathematical assertion need to exhibit a term p : Prop.
To prove that assertion, we need to exhibit a term t : p.

------------------------------------------------------------------------------
3.2. Working with Propositions as Types

'theorem' introduces a new theorem:
-/

namespace ns32
constants p q : Prop

-- Assumes p and q are true.
-- Uses the 'hp' to establish that the conclusion, p, is true.
theorem t1 : p → q → p := λ hp : p, λ hq : q, hp
#print t1

-- 'theorem' looks exactly like definition of the constant function in the last chapter,
-- the only difference being that the arguments are elements of Prop rather than Type.

-- 'theorem' is a version of the definition command:
-- proving p → q → p is the same as defining an element of the associated type.

-- Pragmatic differences between definitions and theorems.
-- Usually not necessary to unfold the “definition” of a theorem;
-- by proof irrelevance, any two proofs of that theorem are definitionally equal.

-- When proof of a theorem is complete, usually only need it exists; it doesn’t matter what the proof is.

-- alternative syntax
theorem t1' : p → q → p :=
assume hp : p,
assume hq : q,
hp

-- alternative syntax
theorem t1'' : p → q → p :=
assume hp : p,
assume hq : q,
show p, from hp -- 'show' annotates the type (for humans)

-- alternative
lemma t1''' : p → q → p :=
assume hp : p,
assume hq : q,
show p, from hp

-- alternative
theorem t1'''' (hp : p) (hq : q) : p := hp
#check t1''''    -- p → q → p

-- apply theorem t1 via function application
-- 'axiom' alternative syntax for constant
-- (declaring a “constant” hp : p is declaring that p is true, as witnessed by hp).
-- Applying the theorem t1 : p → q → p to the fact hp : p that p is true yields the theorem t2 : q → p.
axiom hp : p
theorem t2 : q → p := t1 hp

-- Theorem is true for any propositions p and q, not just the particular constants declared.
-- So more natural to define ∀ p q : Prop, p → q → p.
-- ∀ is alternate syntax for Π.
theorem t1''''' (p q : Prop) (hp : p) (hq : q) : p := hp
#check t1'''''

-- Later will show how Pi types model universal quantifiers, e.g., move parameters to the right of the colon:
theorem t1'''''' : ∀ (p q : Prop), p → q → p := λ (p q : Prop) (hp : p) (hq : q), hp

-- If p and q have been declared as variables, generalize automatically:
variables pp qq : Prop
theorem t1_7 : pp → qq → pp := λ (hp : pp) (hq : qq), hp

-- can declare the assumption hp that p holds, as another variable:
variables p q : Prop
variable  hp : p
theorem t1_8 : q → p := λ (hq : q), hp

-- Lean detects that the proof uses hp and automatically adds hp : p as a premise.
-- In all cases, #check t1 yields ∀ p q : Prop, p → q → p.
-- This type can be written ∀ (p q : Prop) (hp : p) (hq :q), p, since arrow denotes a Pi type
-- where the target does not depend on the bound variable.

-- When t1 generalized like above, it can be applied to different pairs of propositions,
-- to obtain different instances of the general theorem.
theorem t1_9 (p q : Prop) (hp : p) (hq : q) : p := hp

variables /-p q9-/ r s : Prop
#check t1_9 p q                -- p → q → p
#check t1_9 r s                -- r → s → r
#check t1_9 (r → s) (s → r)    -- (r → s) → (s → r) → r → s

-- variable h of type r → s can be viewed as the hypothesis, or premise, that r → s holds.
variable h : r → s
#check t1_9 (r → s) (s → r) h  -- (s → r) → r → s

-- composition with propositions instead of types.
-- variables p q r s : Prop
theorem t2' (h₁ : q → r) (h₂ : p → q) : p → r :=
assume h₃ : p,
show r, from h₁ (h₂ h₃)
-- As a theorem of propositional logic, what does t2 say?

end ns32

/-
------------------------------------------------------------------------------
3.3. Propositional Logic

true
false
¬     \not, \neg
∧     \and
∨     \or
→     \to, \r, \imp
↔     \iff, \lr
-/

namespace ns33
variables p q : Prop
#check p → q → p ∧ q
#check ¬p → p ↔ false
#check p ∨ q → q ∨ p
end ns33

/-
precedence hight/low : ¬, ∧, ∨, →, ↔

→ and other binary connectives associate to the right (i.e., curried)

lambda abstraction : "introduction rule" for → (implication)

application can : "elimination rule" for  implication

other propositional connectives defined in init.core (see Section 6.1 for more info)

--------------------------------------------------
3.3.1. Conjunction
-/

namespace ns331a

-- and-introduction
variables p q : Prop
example (hp : p) (hq : q) : p ∧ q := and.intro hp hq
#check assume (hp : p) (hq : q), and.intro hp hq

-- and-elimination
example (h : p ∧ q) : p := and.elim_left h
example (h : p ∧ q) : q := and.elim_right h

-- shorthand
example (h : p ∧ q) : q ∧ p := and.intro (and.right h) (and.left h)

end ns331a

/-
'example' states a theorem without naming/storing it.  Useful for illustration.

and-introduction/elimination similar to pairing/projection for cartesian product.

difference
- hp : p and hq : q, and.intro hp hq has type p ∧ q : Prop
- pair hp hq                         has type p × q : Type

Similarity between ∧ and × is instance of Curry-Howard isomorphism,
but in contrast to implication and function space constructor,
∧ and × are treated separately in Lean.
But by analogy, the proof above is similar to a function that swaps the elements of a pair.

Certain types are structures
e.g., ∧
canonical construction via and.intro
alternative : anonymous constructor ⟨arg1, arg2, ...⟩ when type is nductive and can be inferred
e.g., can write ⟨hp, hq⟩ instead of and.intro hp hq
-/

namespace ns331b
variables p q : Prop
variables  (hp : p) (hq : q)
#check (⟨hp, hq⟩ : p ∧ q)
-- \< and \> or ASCII (| and |)
example : p ∧ q := (|hp, hq|)
end ns331b

-- Shorthand: expr e of inductive type foo (possibly applied to some arguments),
-- e.bar is short for foo.bar e.

namespace ns331c
variables p q : Prop
variable l : list ℕ
#check list.head l
#check l.head

example (h : p ∧ q) : q ∧ p := ⟨h.right, h.left⟩

-- flatten nested constructors that associate to the right: equivalent:
example (h : p ∧ q) : q ∧ p ∧ q:= ⟨h.right, ⟨h.left, h.right⟩⟩
example (h : p ∧ q) : q ∧ p ∧ q:= ⟨h.right,  h.left, h.right⟩

end ns331c


--------------------------------------------------
-- 3.3.2. Disjunction

namespace ns332a
-- or-introduction
variables p q : Prop
example (hp : p) : p ∨ q := or.intro_left  q hp
example (hq : q) : p ∨ q := or.intro_right p hq

-- or-elimination :
-- proof by cases
-- or.elim hpq hpr hqr, or.elim takes three arguments, hpq : p ∨ q, hpr : p → r and hqr : q → r,
--    produces a proof of r.
example (h : p ∨ q) : q ∨ p :=
or.elim h
  (assume hp : p,
    show q ∨ p, from or.intro_right q hp)
  (assume hq : q,
    show q ∨ p, from or.intro_left  p hq)

-- 1st arg to or.intro_right and or.intro_left can usually be inferred.
-- So shorthand: or.inr or.inl : or.intro_right _ and or.intro_left _
example (h : p ∨ q) : q ∨ p :=
or.elim h
  (λ hp, or.inr hp)
  (λ hq, or.inl hq)

-- or has two constructors, so annot use anonymous constructor notation.
-- Can write h.elim instead of or.elim h:
example (h : p ∨ q) : q ∨ p :=
h.elim
  (assume hp : p, or.inr hp)
  (assume hq : q, or.inl hq)
end ns332a

--------------------------------------------------
-- 3.3.3. Negation and Falsity

namespace ns333a
variables p q : Prop

-- ¬p is p → false
-- obtain ¬p by deriving a contradiction from p
-- expr hnp hp produces a proof of false from hp : p and hnp : ¬p
example (hpq : p → q) (hnq : ¬q) : ¬p :=
λ (hp : p), /-show false, from-/ hnq (hpq hp)

-- false only has an elimination rule.
-- false-elimination : false.elim : anything follows from a contradiction : "ex falso sequitur quodlibet"
example (hp : p) (hnp : ¬p) : q := false.elim (hnp hp)

-- The arbitrary fact, q, that follows from falsity is an implicit argument in false.elim
-- and is inferred automatically. This pattern, deriving an arbitrary fact from contradictory hypotheses,
-- is quite common, and is represented by absurd.
example (hp : p) (hnp : ¬p) : q := absurd hp hnp

-- e.g., proof of ¬p → q → (q → p) → r:
variable r : Prop
example (hnp : ¬p) (hq : q) (hqp : q → p) : r := absurd (hqp hq) hnp

-- true has only an introduction rule, true.intro : true (abbreviated trivial : true).
-- true is simply true, and has a canonical proof, trivial.
end ns333a

/-
--------------------------------------------------
3.3.4. Logical Equivalence

The expression iff.intro h1 h2 produces a proof of p ↔ q from h1 : p → q and h2 : q → p. The expression iff.elim_left h produces a proof of p → q from h : p ↔ q. Similarly, iff.elim_right h produces a proof of q → p from h : p ↔ q. Here is a proof of p ∧ q ↔ q ∧ p:

try it!
theorem and_swap : p ∧ q ↔ q ∧ p :=
iff.intro
  (assume h : p ∧ q,
    show q ∧ p, from and.intro (and.right h) (and.left h))
  (assume h : q ∧ p,
    show p ∧ q, from and.intro (and.right h) (and.left h))

#check and_swap p q    -- p ∧ q ↔ q ∧ p
Because they represent a form of modus ponens, iff.elim_left and iff.elim_right can be abbreviated iff.mp and iff.mpr, respectively. In the next example, we use that theorem to derive q ∧ p from p ∧ q:

try it!
variable h : p ∧ q
example : q ∧ p := iff.mp (and_swap p q) h
We can use the anonymous constructor notation to construct a proof of p ↔ q from proofs of the forward and backward directions, and we can also use . notation with mp and mpr. The previous examples can therefore be written concisely as follows:

try it!
theorem and_swap : p ∧ q ↔ q ∧ p :=
⟨ λ h, ⟨h.right, h.left⟩, λ h, ⟨h.right, h.left⟩ ⟩

example (h : p ∧ q) : q ∧ p := (and_swap p q).mp h
3.4. Introducing Auxiliary Subgoals
This is a good place to introduce another device Lean offers to help structure long proofs, namely, the have construct, which introduces an auxiliary subgoal in a proof. Here is a small example, adapted from the last section:

try it!
variables p q : Prop

example (h : p ∧ q) : q ∧ p :=
have hp : p, from and.left h,
have hq : q, from and.right h,
show q ∧ p, from and.intro hq hp
Internally, the expression have h : p, from s, t produces the term (λ (h : p), t) s. In other words, s is a proof of p, t is a proof of the desired conclusion assuming h : p, and the two are combined by a lambda abstraction and application. This simple device is extremely useful when it comes to structuring long proofs, since we can use intermediate have’s as stepping stones leading to the final goal.

Lean also supports a structured way of reasoning backwards from a goal, which models the “suffices to show” construction in ordinary mathematics. The next example simply permutes the last two lines in the previous proof.

try it!
variables p q : Prop

example (h : p ∧ q) : q ∧ p :=
have hp : p, from and.left h,
suffices hq : q, from and.intro hq hp,
show q, from and.right h
Writing suffices hq : q leaves us with two goals. First, we have to show that it indeed suffices to show q, by proving the original goal of q ∧ p with the additional hypothesis hq : q. Finally, we have to show q.

3.5. Classical Logic
The introduction and elimination rules we have seen so far are all constructive, which is to say, they reflect a computational understanding of the logical connectives based on the propositions-as-types correspondence. Ordinary classical logic adds to this the law of the excluded middle, p ∨ ¬p. To use this principle, you have to open the classical namespace.

try it!
open classical

variable p : Prop
#check em p
Intuitively, the constructive “or” is very strong: asserting p ∨ q amounts to knowing which is the case. If RH represents the Riemann hypothesis, a classical mathematician is willing to assert RH ∨ ¬RH, even though we cannot yet assert either disjunct.

One consequence of the law of the excluded middle is the principle of double-negation elimination:

try it!
theorem dne {p : Prop} (h : ¬¬p) : p :=
or.elim (em p)
  (assume hp : p, hp)
  (assume hnp : ¬p, absurd hnp h)
Double-negation elimination allows one to prove any proposition, p, by assuming ¬p and deriving false, because that amounts to proving ¬¬p. In other words, double-negation elimination allows one to carry out a proof by contradiction, something which is not generally possible in constructive logic. As an exercise, you might try proving the converse, that is, showing that em can be proved from dne.

The classical axioms also give you access to additional patterns of proof that can be justified by appeal to em. For example, one can carry out a proof by cases:

try it!
example (h : ¬¬p) : p :=
by_cases
  (assume h1 : p, h1)
  (assume h1 : ¬p, absurd h1 h)
Or you can carry out a proof by contradiction:

try it!
example (h : ¬¬p) : p :=
by_contradiction
  (assume h1 : ¬p,
    show false, from h h1)
If you are not used to thinking constructively, it may take some time for you to get a sense of where classical reasoning is used. It is needed in the following example because, from a constructive standpoint, knowing that p and q are not both true does not necessarily tell you which one is false:

try it!
example (h : ¬(p ∧ q)) : ¬p ∨ ¬q :=
or.elim (em p)
  (assume hp : p,
    or.inr
      (show ¬q, from
        assume hq : q,
        h ⟨hp, hq⟩))
  (assume hp : ¬p,
    or.inl hp)
We will see later that there are situations in constructive logic where principles like excluded middle and double-negation elimination are permissible, and Lean supports the use of classical reasoning in such contexts without relying on excluded middle.

The full list of axioms that are used in Lean to support classical reasoning are discussed in Chapter 11.

3.6. Examples of Propositional Validities
Lean’s standard library contains proofs of many valid statements of propositional logic, all of which you are free to use in proofs of your own. The following list includes a number of common identities. The ones that require classical reasoning are grouped together at the end, while the rest are constructively valid.

try it!
open classical

variables p q r s : Prop

-- commutativity of ∧ and ∨
example : p ∧ q ↔ q ∧ p := sorry
example : p ∨ q ↔ q ∨ p := sorry

-- associativity of ∧ and ∨
example : (p ∧ q) ∧ r ↔ p ∧ (q ∧ r) := sorry
example : (p ∨ q) ∨ r ↔ p ∨ (q ∨ r) := sorry

-- distributivity
example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) := sorry
example : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) := sorry

-- other properties
example : (p → (q → r)) ↔ (p ∧ q → r) := sorry
example : ((p ∨ q) → r) ↔ (p → r) ∧ (q → r) := sorry
example : ¬(p ∨ q) ↔ ¬p ∧ ¬q := sorry
example : ¬p ∨ ¬q → ¬(p ∧ q) := sorry
example : ¬(p ∧ ¬p) := sorry
example : p ∧ ¬q → ¬(p → q) := sorry
example : ¬p → (p → q) := sorry
example : (¬p ∨ q) → (p → q) := sorry
example : p ∨ false ↔ p := sorry
example : p ∧ false ↔ false := sorry
example : ¬(p ↔ ¬p) := sorry
example : (p → q) → (¬q → ¬p) := sorry

-- these require classical reasoning
example : (p → r ∨ s) → ((p → r) ∨ (p → s)) := sorry
example : ¬(p ∧ q) → ¬p ∨ ¬q := sorry
example : ¬(p → q) → p ∧ ¬q := sorry
example : (p → q) → (¬p ∨ q) := sorry
example : (¬q → ¬p) → (p → q) := sorry
example : p ∨ ¬p := sorry
example : (((p → q) → p) → p) := sorry
The sorry identifier magically produces a proof of anything, or provides an object of any data type at all. Of course, it is unsound as a proof method – for example, you can use it to prove false – and Lean produces severe warnings when files use or import theorems which depend on it. But it is very useful for building long proofs incrementally. Start writing the proof from the top down, using sorry to fill in subproofs. Make sure Lean accepts the term with all the sorry’s; if not, there are errors that you need to correct. Then go back and replace each sorry with an actual proof, until no more remain.

Here is another useful trick. Instead of using sorry, you can use an underscore _ as a placeholder. Recall that this tells Lean that the argument is implicit, and should be filled in automatically. If Lean tries to do so and fails, it returns with an error message “don’t know how to synthesize placeholder.” This is followed by the type of the term it is expecting, and all the objects and hypothesis available in the context. In other words, for each unresolved placeholder, Lean reports the subgoal that needs to be filled at that point. You can then construct a proof by incrementally filling in these placeholders.

For reference, here are two sample proofs of validities taken from the list above.

try it!
open classical

variables p q r : Prop

-- distributivity
example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) :=
iff.intro
  (assume h : p ∧ (q ∨ r),
    have hp : p, from h.left,
    or.elim (h.right)
      (assume hq : q,
        show (p ∧ q) ∨ (p ∧ r), from or.inl ⟨hp, hq⟩)
      (assume hr : r,
        show (p ∧ q) ∨ (p ∧ r), from or.inr ⟨hp, hr⟩))
  (assume h : (p ∧ q) ∨ (p ∧ r),
    or.elim h
      (assume hpq : p ∧ q,
        have hp : p, from hpq.left,
        have hq : q, from hpq.right,
        show p ∧ (q ∨ r), from ⟨hp, or.inl hq⟩)
      (assume hpr : p ∧ r,
        have hp : p, from hpr.left,
        have hr : r, from hpr.right,
        show p ∧ (q ∨ r), from ⟨hp, or.inr hr⟩))

-- an example that requires classical reasoning
example : ¬(p ∧ ¬q) → (p → q) :=
assume h : ¬(p ∧ ¬q),
assume hp : p,
show q, from
  or.elim (em q)
    (assume hq : q, hq)
    (assume hnq : ¬q, absurd (and.intro hp hnq) h)
3.7. Exercises
Prove as many identities from the previous section as you can, replacing the “sorry” placeholders with actual proofs.
Prove ¬(p ↔ ¬p) without using classical logic.
-/
-- ==============================================================================
-- 3.3.3. Negation and Falsity

/-
¬p : defined : p → false
obtain ¬p by deriving a contradiction from p.

Similarly, the expression hnp hp produces a proof of false from hp : p and hnp : ¬p.

The next example uses both these rules to produce a proof of (p → q) → ¬q → ¬p.
-/

section
  variables p q r : Prop
  example (hpq : p → q) (hnq : ¬q) : ¬p :=
  assume hp : p,
  show false, from hnq (hpq hp)
end

/-
The connective false has a single elimination rule, false.elim, which expresses the fact that anything follows from a contradiction. This rule is sometimes called ex falso (short for ex falso sequitur quodlibet), or the principle of explosion.

try it!
example (hp : p) (hnp : ¬p) : q := false.elim (hnp hp)
The arbitrary fact, q, that follows from falsity is an implicit argument in false.elim and is inferred automatically. This pattern, deriving an arbitrary fact from contradictory hypotheses, is quite common, and is represented by absurd.

try it!
example (hp : p) (hnp : ¬p) : q := absurd hp hnp
Here, for example, is a proof of ¬p → q → (q → p) → r:

try it!
example (hnp : ¬p) (hq : q) (hqp : q → p) : r :=
absurd (hqp hq) hnp
Incidentally, just as false has only an elimination rule, true has only an introduction rule, true.intro : true, sometimes abbreviated trivial : true. In other words, true is simply true, and has a canonical proof, trivial.
-/
