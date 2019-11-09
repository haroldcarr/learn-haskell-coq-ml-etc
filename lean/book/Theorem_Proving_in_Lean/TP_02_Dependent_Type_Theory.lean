/-
------------------------------------------------------------------------------
2. Dependent Type Theory

Dependent type theory is a language.
Lean based on version of dependent type theory known as the Calculus of Constructions,
with a countable hierarchy of non-cumulative universes and inductive types.

------------------------------------------------------------------------------
2.1. Simple Type Theory

Type theory gets its name from fact that all expressions have types.
-/

/- declare some constants -/

constant m : nat        -- m is a natural number
constant n : nat
constants b1 b2 : bool  -- declare two constants at once

/- check their types (Commands that query Lean for info begin with hash) -/

#check m            -- output: nat
#check n
#check n + 0        -- nat
#check m * (n + 0)  -- nat
#check b1           -- bool
#check b1 && b2     -- "&&" is boolean and
#check b1 || b2     -- boolean or
#check tt           -- boolean "true"

/- Build new types out of others. -/

constant f   : nat → nat         -- type the arrow as "\to" or "\r"
constant f'  : nat -> nat        -- alternative ASCII notation
constant f'' : ℕ → ℕ             -- alternative notation for nat
constant p   : nat × nat         -- type the product as "\times"
constant q   : prod nat nat      -- alternative notation
constant g   : nat → nat → nat
constant g'  : nat → (nat → nat) -- has the same type as g!
constant h   : nat × nat → nat

constant F   : (nat → nat) → nat -- a "functional"

#check f               -- ℕ → ℕ
#check f n             -- ℕ
#check g m n           -- ℕ
#check g m             -- ℕ → ℕ
#check (m, n)          -- ℕ × ℕ
#check p.1             -- ℕ
#check p.2             -- ℕ
#check (m, n).1        -- ℕ
#check (p.1, n)        -- ℕ × ℕ
#check F f             -- ℕ

/-
ℕ  \nat
×  \times
lower-case greek letters (e.g, α, β, γ) fro types   \a, \b, and \g

f x   function application

type expressions, arrows associate to the right (enables partial application)

(m, n)   ordered pair
p.1      projection
p.2      projection

------------------------------------------------------------------------------
2.2. Types as Objects

Dependent type theory extends simple type theory is that types are first-class citizens.
-/

#check nat               -- Type
#check bool              -- Type
#check nat → bool        -- Type
#check nat × bool        -- Type
#check nat → nat         -- ...
#check nat × nat → nat
#check nat → nat → nat
#check nat → (nat → nat)
#check nat → nat → bool
#check (nat → nat) → nat

/- declare constants and constructors for types --/

constants α β : Type
constant F' : Type → Type
constant G : Type → Type → Type

#check α        -- Type
#check F' α      -- Type
#check F' nat    -- Type
#check G α      -- Type → Type
#check G α β    -- Type
#check G α nat  -- Type

-- cartesian product : Type → Type → Type

-- constants α β : Type

#check prod α β       -- Type
#check prod nat nat   -- Type

-- list of α

-- constant α : Type

#check list α    -- Type
#check list nat  -- Type

/-
Think of a type as a set.  The elements of the type are the elements of the set.

What type does Type have?
-/

#check Type      -- Type 1

-- Lean’s foundation has an infinite hierarchy of types:

#check Type     -- Type 1
#check Type 1   -- Type 2
#check Type 2   -- Type 3
#check Type 3   -- Type 4
#check Type 4   -- Type 5

/-
Type 0 : universe of "small" or "ordinary" types.
Type 1 : universe that contains Type 0 as an element
Type 2 : universe that contains Type 1 as an element
...
-/

#check Type
#check Type 0

-- another type, Prop, with special properties (discussed in next chapter)

#check Prop -- Type

/-
Some operations are polymorphic over type universes,
e.g., list α, for any type α, no matter which type universe α lives in.
Thus the type of the function list:
-/

#check list    -- Type u_1 → Type u_1

/-
u_1 is a variable ranging over type levels.
#check output says that whenever α has type Type n, list α also has type Type n.

function prod is similarly polymorphic:
-/

#check prod    -- Type u_1 → Type u_2 → Type (max u_1 u_2)

-- define polymorphic constants and variables via declaring universe variables explicitly:

universe u'
constant α' : Type u'
#check α'

/-
Use polymorphism for constructions to have as much generality as possible.
Type constructors will be treated as instances of mathematical functions.

------------------------------------------------------------------------------
2.3. Function Abstraction and Evaluation

Given m n : nat, then (m, n) : nat × nat : create pairs of naturals.
Given p : nat × nat, then fst p : nat and snd p : nat. : extracting pairs two components.

APPLICATION
Given function f : α → β, then apply it to a : α to obtain f a : β.

ABSTRACTION: create a function
Suppose postulating variable x : α, then construct expression t : β.
Then the expression λ x : α, t, is an object (a "function") of type α → β.
e.g., λ x : nat, x + 5
-/

#check fun x : nat, x + 5
#check   λ x : nat, x + 5

-- constants α β  : Type
constants a1 a2 : α
constants b1' b2' : β

constant faa : α → α
constant gab : α → β
constant haba : α → β → α
constant paa : α → α → bool

#check fun x : α, faa x                      -- α → α
#check   λ x : α, faa x                        -- α → α
#check   λ x : α, faa (faa x)                    -- α → α
#check   λ x : α, haba x b1'                     -- α → α
#check   λ y : β, haba a1 y                     -- β → α
#check   λ x : α, paa (faa (faa x)) (haba (faa a1) b2')  -- α → bool
#check   λ x : α, λ y : β , haba (faa x) y         -- α → β → α
#check   λ (x : α) (y : β), haba (faa x) y        -- α → β → α
#check   λ  x       y     , haba (faa x) y                    -- α → β → α

-- constants α β γ : Type
constants γ : Type
constant fab' : α → β
constant gby' : β → γ
constant b : β

#check λ x : α, x        -- α → α
#check λ x : α, b        -- α → β
#check λ x : α, gby' (fab' x)  -- α → γ
#check λ x    , gby' (fab' x)

-- constant functions
#check λ  b : β, λ x : α , x    -- β → α → α
#check λ (b : β)  (x : α), x    -- β → α → α
-- composition
#check λ (g : β → γ) (f : α → β) (x : α), g (f x)  -- (β → γ) → (α → β) → α → γ

-- abstract over types - using dependent products

#check λ (α β   : Type) (b : β)     (x : α)            , x       -- constant function
#check λ (α β γ : Type) (g : β → γ) (f : α → β) (x : α), g (f x) -- composition

/-
constant f : α → β
constant g : β → γ
-/
constant hidentity : α → α
constants (a' : α) (b' : β)

#reduce (λ x : α, x) a'              -- a'
#reduce (λ x : α, b') a'             -- b'
#reduce (λ x : α, b') (hidentity a')  -- b'
#reduce (λ x : α, gby' (fab' x)) a'  -- g (f a)

#reduce (λ (v : β → γ) (u : α → β) x, v (u x)) gby' fab' a'   -- g (f a)

#reduce (λ (Q R S : Type) (v : R → S) (u : Q → R) (x : Q), v (u x)) α β γ gby' fab' a'        -- g (f a)

/-
#reduce : evaluate/reduce to normal form : do all BETA reductions.

two terms that beta reduce to a common term are called beta equivalent

#reduce does other forms of reduction too:
-/

constants m' n' : nat
constant b'' : bool

#print "reducing pairs"
#reduce (m, n).1        -- m
#reduce (m, n).2        -- n

#print "reducing boolean expressions"
#reduce tt && ff        -- ff
#reduce ff && b''       -- ff
#reduce b'' && ff         -- bool.rec ff ff b

#print "reducing arithmetic expressions"
#reduce n + 0           -- n
#reduce n + 2           -- nat.succ (nat.succ n)
#reduce 2 + 3           -- 5

/-
Feature of dependent type theory: every term has computational behavior; supports reduction/normalization.

Two terms that reduce to the same value are called definitionally equal.
Considered "the same" by the logical framework.

Computational behavior makes it possible to use Lean as a programming language.
-/

#eval 12345 * 54321

/-
#reduce uses Lean’s trusted kernel : part responsible for checking/verifying correctness of exprs and proofs.
reduce is more trustworthy, but less efficient.

more about #eval in Chapter 11.

------------------------------------------------------------------------------
2.4. Introducing Definitions

def : defines new objects.  : def foo : α := bar
-/

def foo : (ℕ → ℕ) → ℕ := λ f, f 0

#check foo    -- (ℕ → ℕ) → ℕ
#print foo    -- λ (f : ℕ → ℕ), f 0

-- can omit type when it can bre inferred

def foo' := λ f : ℕ → ℕ, f 0

-- alternative format : put abstracted variables before the colon and omits the lambda:

def double (x : ℕ) : ℕ := x + x
#print double
#check double 3
#reduce double 3    -- 6

def square (x : ℕ) := x * x
#print square
#check square 3
#reduce square 3    -- 9

def do_twice (f : ℕ → ℕ) (x : ℕ) : ℕ := f (f x)

#reduce do_twice double 2    -- 8

-- These definitions are equivalent to the following:

def double' : ℕ → ℕ := λ x, x + x
def square' : ℕ → ℕ := λ x, x * x
def do_twice' : (ℕ → ℕ) → ℕ → ℕ := λ f x, f (f x)

-- args that are types:

def compose (α β γ : Type) (g : β → γ) (f : α → β) (x : α) : γ := g (f x)

-- exercise
def hc_multiply_by_8 : ℕ → ℕ := λ x, double ((do_twice double) x)
#reduce hc_multiply_by_8 4 -- 32

-- exercise : define Do_Twice : ((ℕ → ℕ) → (ℕ → ℕ)) → (ℕ → ℕ) → (ℕ → ℕ)
-- that applies its argument twice
-- such that Do_Twice do_twice applies its input four times.
-- Then evaluate Do_Twice do_twice double 2.
def Do_Twice : ((ℕ → ℕ) → (ℕ → ℕ)) → (ℕ → ℕ) → (ℕ → ℕ) := λ f g, f (f g)
#reduce Do_Twice do_twice double 2

def   curry (α β γ : Type) (f : α × β → γ) :  α → β  → γ := λ a b, f (a,  b)
def uncurry (α β γ : Type) (f : α → β → γ) : (α × β) → γ := λ x  , f  x.1 x.2

------------------------------------------------------------------------------
-- 2.5. Local Definitions via LET

#check   let y := 2 + 2 in y * y   -- ℕ
#reduce  let y := 2 + 2 in y * y   -- 16

def t (x : ℕ) : ℕ := let y := x + x in y * y -- definitionally equal to the term (x + x) * (x + x)

#reduce t 2   -- 16

#check   let y := 2 + 2, z := y + y in z * z   -- ℕ
#reduce  let y := 2 + 2, z := y + y in z * z   -- 64

/-
1. let a := t1 in t2
similar to
2. (λ a, t2) t1
but not the same.
1. every instance of a in t2 is syntactic abbreviation for t1.
2. a is a variable.  λ a, t2 must make sense independently of the value of a.
-/

def fooey := let a := nat in λ x : a, x + 2
/-
def bar :=    (λ a,          λ x : a, x + 2) nat
-- above does not work because do not know type of a, so do not know how to handle '+'
-/

/-
------------------------------------------------------------------------------
2.6. Variables and Sections

constant : declare new objects in global context.

Warning: declaring a new constant is same to declaring an axiomatic extension of the foundational system.
It may result in inconsistency.

Avoid globals via LAMBDA.
-/

def compose' (α β γ : Type) (g : β → γ) (f : α → β) (x : α) : γ := g (f x)

def do_twice'' (α : Type) (h : α → α) (x : α) : α := h (h x)

def do_thrice (α : Type) (h : α → α) (x : α) : α := h (h (h x))

-- refactor via variable / variables

variables (α β γ : Type)

def compose''   (g : β → γ) (f : α → β) (x : α) : γ := g (f x)
def do_twice''' (h : α → α) (x : α) : α := h (h x)
def do_thrice'  (h : α → α) (x : α) : α := h (h (h x))

-- declare variables of any type

-- variables (α β γ : Type)
variables (g : β → γ) (f : α → β) (h : α → α)
variable x : α

def compose''' := g (f x)
def do_twice'''' := h (h x)
def do_thrice'' := h (h (h x))

#print compose
#print do_twice
#print do_thrice
-- all three types of defs print the same

/-
variable / variables do NOT create permanent entities.
They instruct Lean to insert the declared variables as bound variables in definitions that refer to them.

a 'variable' stays in scope until the end of the file.

To limit the scope of a variable, use SECTION
-/

section useful
  variables (α' β' γ' : Type)
  variables (g' : β → γ) (f' : α → β) (h' : α → α)
  variable x' : α

  def compose''''   := g (f x)
  def do_twice''''' := h (h x)
  def do_thrice'''  := h (h (h x))
end useful

/-
Do NOT have to name a section.
Sections can be nested.
-/

------------------------------------------------------------------------------
-- 2.7. nested hierachical Namespaces

namespace foo
  def nsa         : ℕ := 5
  def nsf (x : ℕ) : ℕ := x + 7
  def nsfa        : ℕ := nsf nsa
  def nsffa       : ℕ := nsf (nsf nsa)

  #print "inside foo"

  #check nsa
  #check nsf
  #check nsfa
  #check nsffa
  #check foo.nsfa
end foo

#print "outside the namespace"


-- #check a  -- error
-- #check f  -- error
#check foo.nsa
#check foo.nsf
#check foo.nsfa
#check foo.nsffa

-- open brings unqualified names into current context
open foo

#print "opened foo"

#check nsa
#check nsf
#check nsfa
#check foo.nsfa

#check list.nil
#check list.cons
#check list.append

open list

#check nil
#check cons
#check append

-- namespaces can be nested

namespace foon
  def na : ℕ := 5
  def nf (x : ℕ) : ℕ := x + 7

  def nfa : ℕ := nf na

  namespace bar
    def ffa : ℕ := nf (nf na)

    #check nfa
    #check ffa
  end bar

  #check nfa
  #check bar.ffa
end foon

#check foon.nfa
#check foon.bar.ffa

open foon

#check nfa
#check bar.ffa

/-
------------------------------------------------------------------------------
2.8. Dependent Types

What makes dependent type theory dependent : types depend on parameters
(e.g., list α enables, list ℕ, list bool).

vec α n : α : Type (of elements) and n : ℕ (length).

cons : α → list α → list α.

What type should cons have?
Guess : Type → α → list α → list α.
NO: α does not refer to anything --- it should refer to the argument of type Type.
Assuming α : Type is the first argument to the function, the type of the next two elements are α and list α.
These types vary depending on the first argument, α.

This is an instance of a Pi type, or dependent function type.
Given α : Type and β : α → Type, think of β as a family of types over α, a type β a for each a : α.
The type Π x : α, β x denotes the type of functions f that, for each a : α, f a is an element of β a.
The type of the value returned by f depends on its input.

Π x : α, β makes sense for any expression β : Type.
When the value of β depends on x, Π x : α, β denotes a dependent function type.
When β doesn’t depend on x, Π x : α, β is no different from the type α → β.
In dependent type theory the Pi construction is fundamental
(α → β is just Π x : α, β when β does not depend on x).

(Π is \Pi)

Example : list operations:
-/

namespace hidden
universe u
constant list   : Type u → Type u
constant cons   : Π α : Type u,      α → list α → list α
constant nil    : Π α : Type u,                   list α
constant head   : Π α : Type u, list α          →      α
constant tail   : Π α : Type u, list α          → list α
constant append : Π α : Type u, list α → list α → list α
end hidden

open list
#check list     -- Type u_1 → Type u_1
#check @cons    -- Π {α : Type u_1}                        ,      α → list α → list α
#check @nil     -- Π {α : Type u_1}                        ,                   list α
#check @head    -- Π {α : Type u_1} [_inst_1 : inhabited α], list α          → α
#check @tail    -- Π {α : Type u_1}                        , list α          → list α
#check @append  -- Π {α : Type u_1} [c : has_append α]     , α → α → α

-- head : type α required to have at least one element.  when passed the empty list, must determine a default.

-- Vector operations
namespace vec
universe u
constant vec    : Type u → ℕ → Type u
constant empty  : Π  α : Type u           ,                     vec α  0
constant cons   : Π (α : Type u) (n : ℕ)  ,     α   → vec α n → vec α (n + 1)
constant append : Π (α : Type u) (n m : ℕ), vec α m → vec α n → vec α (n + m)
end vec


-- Sigma types

/-
Σ x : α, β x (aka dependent products) -- companions to Pi types.

Σ x : α, β x denotes the type of pairs sigma.mk a b where a : α and b : β a.

Just as Pi types Π x : α, β x generalize the notion of a function type α → β by allowing β to depend on α,
Sigma types Σ x : α, β x generalize the cartesian product α × β in the same way:
in the expression sigma.mk a b, the type of the second element of the pair, b : β a,
depends on the first element of the pair, a : α.
-/

namespace ss
variable sα : Type
variable sβ : sα → Type
variable sa : sα
variable sb : sβ sa
#check   sigma.mk sa sb     -- Σ (a : α), β a
#check  (sigma.mk sa sb).1  -- α
#check  (sigma.mk sa sb).2  -- β (sigma.fst (sigma.mk a b))
#reduce (sigma.mk sa sb).1  -- a
#reduce (sigma.mk sa sb).2  -- b
end ss

-- (sigma.mk a b).1 short for sigma.fst (sigma.mk a b)
-- (sigma.mk a b).2           sigma.snd (sigma.mk a b)

/-
------------------------------------------------------------------------------
2.9. Implicit Arguments

Suppose we have an implementation of lists as described above.

try it!
namespace hidden
universe u
constant list : Type u → Type u

namespace list
  constant cons   : Π α : Type u, α → list α → list α
  constant nil    : Π α : Type u, list α
  constant append : Π α : Type u, list α → list α → list α
end list
end hidden
Then, given a type α, some elements of α, and some lists of elements of α, we can construct new lists using the constructors.

try it!
open hidden.list

variable  α : Type
variable  a : α
variables l1 l2 : list α

#check cons α a (nil α)
#check append α (cons α a (nil α)) l1
#check append α (append α (cons α a (nil α)) l1) l2
Because the constructors are polymorphic over types, we have to insert the type α as an argument repeatedly. But this information is redundant: one can infer the argument α in cons α a (nil α) from the fact that the second argument, a, has type α. One can similarly infer the argument in nil α, not from anything else in that expression, but from the fact that it is sent as an argument to the function cons, which expects an element of type list α in that position.

This is a central feature of dependent type theory: terms carry a lot of information, and often some of that information can be inferred from the context. In Lean, one uses an underscore, _, to specify that the system should fill in the information automatically. This is known as an “implicit argument.”

try it!
#check cons _ a (nil _)
#check append _ (cons _ a (nil _)) l1
#check append _ (append _ (cons _ a (nil _)) l1) l2
It is still tedious, however, to type all these underscores. When a function takes an argument that can generally be inferred from context, Lean allows us to specify that this argument should, by default, be left implicit. This is done by putting the arguments in curly braces, as follows:

try it!
namespace list
  constant cons   : Π {α : Type u}, α → list α → list α
  constant nil    : Π {α : Type u}, list α
  constant append : Π {α : Type u}, list α → list α → list α
end list

open hidden.list

variable  α : Type
variable  a : α
variables l1 l2 : list α

#check cons a nil
#check append (cons a nil) l1
#check append (append (cons a nil) l1) l2
All that has changed are the braces around α : Type u in the declaration of the variables. We can also use this device in function definitions:

try it!
universe u
def ident {α : Type u} (x : α) := x

variables α β : Type u
variables (a : α) (b : β)

#check ident      -- ?M_1 → ?M_1
#check ident a    -- α
#check ident b    -- β
This makes the first argument to ident implicit. Notationally, this hides the specification of the type, making it look as though ident simply takes an argument of any type. In fact, the function id is defined in the standard library in exactly this way. We have chosen a nontraditional name here only to avoid a clash of names.

Variables can also be specified as implicit when they are declared with the variables command:

try it!
universe u

section
  variable {α : Type u}
  variable x : α
  def ident := x
end

variables α β : Type u
variables (a : α) (b : β)

#check ident
#check ident a
#check ident b
This definition of ident here has the same effect as the one above.

Lean has very complex mechanisms for instantiating implicit arguments, and we will see that they can be used to infer function types, predicates, and even proofs. The process of instantiating these “holes,” or “placeholders,” in a term is often known as elaboration. The presence of implicit arguments means that at times there may be insufficient information to fix the meaning of an expression precisely. An expression like id or list.nil is said to be polymorphic, because it can take on different meanings in different contexts.

One can always specify the type T of an expression e by writing (e : T). This instructs Lean’s elaborator to use the value T as the type of e when trying to resolve implicit arguments. In the second pair of examples below, this mechanism is used to specify the desired types of the expressions id and list.nil:

try it!
#check list.nil             -- list ?M1
#check id                   -- ?M1 → ?M1

#check (list.nil : list ℕ)  -- list ℕ
#check (id : ℕ → ℕ)         -- ℕ → ℕ
Numerals are overloaded in Lean, but when the type of a numeral cannot be inferred, Lean assumes, by default, that it is a natural number. So the expressions in the first two #check commands below are elaborated in the same way, whereas the third #check command interprets 2 as an integer.

try it!
#check 2            -- ℕ
#check (2 : ℕ)      -- ℕ
#check (2 : ℤ)      -- ℤ
Sometimes, however, we may find ourselves in a situation where we have declared an argument to a function to be implicit, but now want to provide the argument explicitly. If foo is such a function, the notation @foo denotes the same function with all the arguments made explicit.

try it!
#check @id        -- Π {α : Type u_1}, α → α
#check @id α      -- α → α
#check @id β      -- β → β
#check @id α a    -- α
#check @id β b    -- β
Notice that now the first #check command gives the type of the identifier, id, without inserting any placeholders. Moreover, the output indicates that the first argument is implicit.

2.10. Exercises
Define the function Do_Twice, as described in Section 2.4.
Define the functions curry and uncurry, as described in Section 2.4.
Above, we used the example vec α n for vectors of elements of type α of length n. Declare a constant vec_add that could represent a function that adds two vectors of natural numbers of the same length, and a constant vec_reverse that can represent a function that reverses its argument. Use implicit arguments for parameters that can be inferred. Declare some variables and check some expressions involving the constants that you have declared.
Similarly, declare a constant matrix so that matrix α m n could represent the type of m by n matrices. Declare some constants to represent functions on this type, such as matrix addition and multiplication, and (using vec) multiplication of a matrix by a vector. Once again, declare some variables and check some expressions involving the constants that you have declared.
-/

-/

