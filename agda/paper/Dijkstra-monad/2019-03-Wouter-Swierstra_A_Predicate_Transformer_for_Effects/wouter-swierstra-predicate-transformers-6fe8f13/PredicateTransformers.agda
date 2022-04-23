module PredicateTransformers where

open import Prelude hiding (map; all)
open import Level   hiding (lift)

------------------------------------------------------------------------------

{-
https://webspace.science.uu.nl/~swier004/publications/2019-icfp-submission-a.pdf

A predicate transformer semantics for effects

WOUTER SWIERSTRA and TIM BAANEN, Universiteit Utrecht

------------------------------------------------------------------------------
ABSTRACT

reasoning about effectful code harder than pure code

predicate transformer (PT) semantics gives a refinement relation that can be used to
- relate a program to its specification, or
- calculate effectful programs that are correct by construction

refinement type https://en.wikipedia.org/wiki/Refinement_(computing)
- type with a predicate which must hold for any element of the refined type
- can express
  - preconditions  when used as function args
  - postconditions when used as return types

------------------------------------------------------------------------------
1 INTRODUCTION

key techniques
- syntax of effectful computations represented as free monads
  - assigning meaning to these monads gives meaning to the syntactic ops each effect provides
- paper shows how to assign PT semantics to computations arising from Kleisli arrows on free monads
  - enables computing the weakest precondition associated with a given postcondition
- using weakest precondition semantics
  - define refinement on computations
  - show how to use this refinement relation to
    - show a program satisfies its specification, or
    - calculate a program from its specification.
- show how programs and specifications may be mixed,
  enabling verified programs to be calculated from their specification one step at a time

------------------------------------------------------------------------------
2 BACKGROUND

--------------------------------------------------
Free monads
-}

module Free where
  -- C          : type of commands
  -- Free C R   : returns an 'a' or issues command c : C
  -- For each c : C, there is a set of responses R c
  data Free {l : Level} (C : Set) (R : C -> Set) (a : Set l) : Set l where
    Pure : a                              -> Free C R a
    -- 2nd arg of Step is continuation : how to proceed after receiving response R c
    Step : (c : C) -> (R c -> Free C R a) -> Free C R a

  -- show that 'Free' is a monad:

  map : forall {l l' C R} -> {a : Set l} -> {b : Set l'}
     -> (a -> b) -> Free C R a
     -> Free C R b
  map f (Pure a)      = Pure (f a)
  map f (Step c Rc→F) = Step c (\Rc -> map f (Rc→F Rc))

  return : forall {l C R} -> {a : Set l}
        -> a -> Free C R a
  return = Pure

  _>>=_ : forall {l l' C R} -> {a : Set l} -> {b : Set l'}
       -> Free C R a -> (a -> Free C R b)
       -> Free C R b
  Pure a      >>= a→F = a→F a
  Step c Rc→F >>= a→F = Step c (\Rc -> Rc→F Rc >>= a→F)
  infixr 20  _>>=_

  _>>_ : forall {l l' C R} {a : Set l} {b : Set l'}
      -> Free C R a -> Free C R b
      -> Free C R b
  c1 >> c2 = c1 >>= \_ -> c2

  {-
  different effects choose C and R differently, depending on their ops

  --------------------------------------------------
  Weakest precondition (WP) semantics

  associating WP semantics with imperative programs dates to
  Dijkstra’s Guarded Command Language [1975]

  ways to specify behaviour of function f : a -> b
  - reference implementation
  - define a relation R : a -> b -> Set
  - write contracts and test cases
  - PT semantics

  values of type a -> Set are called "predicate on type a"

  PTs are functions between predicates
  e.g., weakest precondition:
  -}

  -- "maps"
  -- - function f : a -> b, and
  -- - desired postcondition on function’s output, b -> Set
  -- to weakest precondition a -> Set on function’s input that ensures postcondition satisfied
  --
  -- non-dependent version
  -- note: definition IS reverse function composition
  wp0 : forall {a : Set} {b : Set}
     -> (a -> b)
     ->      (b -> Set)
     -> (a ->      Set)
  wp0 a→b b→Set = \a -> b→Set (a→b a)

  {-
  wp0 semantics
  - no way to specify that output is related to input
  - fix via making f dependent:
  -}

  -- dependent version
  wp : forall {l l' l''}
    -> {a : Set l} {b : a -> Set l'}
    -> ((x : a) -> b x)
    -> ((x : a) -> b x -> Set l'')
    -> (     a  ->        Set l'')
  wp a→ba a→ba→Set = \a -> a→ba→Set a (a→ba a)

  -- shorthand for working with predicates and predicates transformers
  _⊆_ : forall {l'} -> {a : Set}
     -> (a -> Set l')
     -> (a -> Set l')
     -> Set l'
  P ⊆ Q = ∀ a -> P a -> Q a

  -- refinement relation between PTs
  _⊑_ : {a : Set} {b : a -> Set}
     -> (pt1 pt2 : ((x : a) -> b x -> Set) -> (a -> Set))
     -> Set₁
  pt1 ⊑ pt2 = forall P -> pt1 P ⊆ pt2 P

  ⊑-trans : {a : Set} {b : a -> Set} {P Q R : ((x : a) -> b x -> Set) -> (a -> Set)}
         -> P ⊑ Q -> Q ⊑ R
         -> P ⊑ R
  ⊑-trans P⊑Q Q⊑R a→ba→Set a P_a→ba→Set_a = Q⊑R a→ba→Set a (P⊑Q a→ba→Set a P_a→ba→Set_a)

  ⊑-refl  : {a : Set} {b : a -> Set} {P : ((x : a) -> b x -> Set) -> (a -> Set)}
         -> P ⊑ P
  ⊑-refl a→ba→Set a P_a→ba→Set_a = P_a→ba→Set_a

  ⊑-eq    : {a b : Set}
         ->   (f      g : a -> b)
         -> wp f ⊑ wp g -> (x : a)
         ->    f x == g x
  ⊑-eq f g wpf⊑wpg a = wpf⊑wpg (\_ b -> f a == b) a refl

  eq-⊑    : {a b : Set}
         -> (f g : a -> b)
         -> ((x : a) -> f x == g x)
         -> wp f ⊑ wp g
  eq-⊑ f g eq a→b→Set a a→b→Set_a_b with f a | g a | eq a
  ... | _ | _ | refl = a→b→Set_a_b

  {-
  use refinement relation
  - to relate PT semantics between programs and specifications
  - to show a program satisfies its specification; or
  - to show that one program is somehow ‘better’ than another,
    where ‘better’ is defined by choice of PT semantics

  in pure setting, this refinement relation is not interesting:
  the refinement relation corresponds to extensional equality between functions:

  lemma follows from the ‘Leibniz rule’ for equality in intensional type theory:
  refinement : ∀ (f g : a -> b) -> (wp f ⊑ wp g) ↔ (∀ x -> f x ≡ g x)

  this paper defines PT semantics for Kleisli arrows of form

      a -> Free C R b

  could use 'wp' to assign semantics to these computations directly,
  but typically not interested in syntactic equality between free monads

  rather want to study semantics of effectful programs they represent

  to define a PT semantics for effects
  define a function with form:

    -- how to lift a predicate on 'a' over effectful computation returning 'a'
    pt : (a -> Set) -> Free C R a -> Set

  'pt' def depends on semantics desired for a particulr free monad

  Crucially, choice of
  - pt and
  - weakest precondition semantics :  wp
  together give a way to assign weakest precondition semantics
  to Kleisli arrows representing effectful computations
  -}

------------------------------------------------------------------------------

module Maybe where
  open import Data.Nat public
    using
      (_+_; _>_; _*_
      )
    renaming
      ( ℕ to Nat
      ; zero to Zero
      ; suc to Succ
      )
  open import Data.Nat.DivMod using (_div_)
  open import Data.Nat.Properties using (*-zeroʳ)
  open Free

  {-
  ------------------------------------------------------------------------------
  3 PARTIALITY

  Partial computations : i.e., 'Maybe'

  make choices for commands C and responses R
  -}

  data C : Set where
    Abort : C -- no continuation

  R : C -> Set
  R Abort = ⊥ -- since C has no continuation, valid responses is empty

  Partial : Set -> Set
  Partial = Free C R

  -- smart constructor for failure
  -- responses to Abort command are empty
  -- use emptiness to discharge continuation in 2nd arg of Step
  abort : forall {a} -> Partial a
  abort = Step Abort (\())

  {-
  computation of type 'Partial a' will either
  - return a value of type 'a' or
  - fail, issuing abort command

  --------------------------------------------------
  Example: division

  expression language, closed under division and natural numbers:
  -}

  data Expr : Set where
    Val : Nat  -> Expr
    Div : Expr -> Expr -> Expr

  exv : Expr
  exv = Val 3
  exd : Expr
  exd = Div (Val 3) (Val 3)

  -- semantics specified using inductively defined RELATION:
  -- requires divisor to evaluate to non-zero : rules out bad results
  data _⇓_ : Expr -> Nat -> Set where
    ⇓Base : forall {n}
         -> Val n ⇓ n
    ⇓Step : forall {el er n1 n2}
         ->     el    ⇓  n1
         ->        er ⇓         (Succ n2)
         -> Div el er ⇓ (n1 div (Succ n2))

  exb : Val 3 ⇓ 3
  exb = ⇓Base

  exs : Div (Val 3) (Val 3) ⇓ 1
  exs = ⇓Step ⇓Base ⇓Base

  -- alternatively, semantics specified by an INTERPRETER
  -- evaluate Expr via monadic INTERPRETER, using Partial to handle division-by-zero

  -- define operation used by ⟦_⟧ interpreter
  _÷_ : Nat -> Nat -> Partial Nat
  n ÷ Zero     = abort
  n ÷ (Succ k) = return (n div (Succ k))

  ⟦_⟧ : Expr -> Partial Nat
  ⟦ Val x ⟧     =  return x
  ⟦ Div e1 e2 ⟧ =  ⟦ e1 ⟧ >>= \v1 ->
                   ⟦ e2 ⟧ >>= \v2 ->
                   v1 ÷ v2

  evv   : Free C R Nat
  evv   = ⟦ Val 3 ⟧
  evv'  : evv ≡ Pure 3
  evv'  = refl

  evd   : Free C R Nat
  evd   = ⟦ Div (Val 3) (Val 3) ⟧
  evd'  : evd ≡ Pure 1
  evd'  = refl

  evd0  : Free C R Nat
  evd0  = ⟦ Div (Val 3) (Val 0) ⟧
  evd0' : evd0 ≡ Step Abort (λ ())
  evd0' = refl

  {-
  How to relate the two definitions:
  - stdlib 'div' requires implicit proof that divisor is non-zero
    - ⇓ relation generates via pattern matching
    - _÷_ does explicit check
  - interpreter uses _÷_
    - fails explicitly with abort when divisor is zero

  Assign a weakest precondition semantics to Kleisli arrows of the form

      a -> Partial b
  -}

  mustPT : forall {a : Set} -> {b : a -> Set}
        -> ((x : a) -> b x -> Set)
        ->  (x : a)
        ->    Partial (b x)
        ->                    Set
  mustPT a→ba→Set _ (Pure ba)      = a→ba→Set _ ba
  mustPT _        _ (Step Abort _) = ⊥

  wpPartial : {a : Set} -> {b : a -> Set}
           -> ((x : a) -> Partial (b x))
           -> ((x : a) ->          b x -> Set)
           -> (     a  ->                 Set)
  wpPartial a→partialBa a→ba→Set = wp a→partialBa (mustPT a→ba→Set)

  {-
  To call 'wp', must show how to transform
  -      predicate                  P :         b -> Set
  - to a predicate on partial results : Partial b -> Set
  Done via proposition 'mustPT P c'
  - holds when computation c of type Partial b successfully returns a 'b' that satisfies P

  particular PT semantics of partial computations determined by definition of 'mustPT'
  here: rule out failure entirely
  - so case Abort returns empty type

  Given this PT semantics for Kleisli arrows in general,
  can now study semantics of above monadic interpreter
  via passing
  - interpreter           : ⟦_⟧
  - desired postcondition : _⇓_
  as arguments to wpPartial:
  -}

  exwpp  : Expr -> Set
  exwpp  = wpPartial ⟦_⟧ _⇓_
  exwpp' : wpPartial ⟦_⟧ _⇓_ ≡ λ expr -> mustPT _⇓_ expr ⟦ expr ⟧
  exwpp' = refl

  wppv1  : wpPartial ⟦_⟧ _⇓_ (Val 1)
  wppv1  = ⇓Base
  wppd   : wpPartial ⟦_⟧ _⇓_ (Div (Val 1) (Val 1))
  wppd   = ⇓Step ⇓Base ⇓Base

  xxx    : mustPT _⇓_ (Div (Val 3) (Val 3)) (Pure 3 >>= (λ v1 -> Pure 3 >>= _÷_ v1))
  xxx    = ⇓Step {Val 3} {Val 3} {3} {2} ⇓Base ⇓Base

  xxx'   : mustPT _⇓_ (Div (Val 3) (Val 3)) (3 ÷ 3)
  xxx'   = ⇓Step {Val 3} {Val 3} {3} {2} ⇓Base ⇓Base

  {-
  resulting in a predicate on expressions

  for all expressions satisfying this predicate,
  the monadic interpreter and the relational specification, _⇓_,
  must agree on the result of evaluation

  What does this say about correctness of interpreter?
  To understand the predicate better, define this predicate on expressions:
  -}

  SafeDiv : Expr -> Set
  SafeDiv (Val x)     = ⊤
  SafeDiv (Div e1 e2) = (e2 ⇓ Zero -> ⊥) ∧ SafeDiv e1 ∧ SafeDiv e2

  exsdv : SafeDiv (Val 3) ≡ ⊤
  exsdv = refl
  exsdd : SafeDiv (Div (Val 3) (Val 3))
        ≡ Pair ((x : Val    3 ⇓ Zero) -> ⊥) (Pair (⊤' zero) (⊤' zero))
  exsdd = refl

  exsdx : SafeDiv (Div (Val 3) (Val 0))
        ≡ Pair ((x : Val Zero ⇓ Zero) -> ⊥) (Pair (⊤' zero) (⊤' zero))
  exsdx = refl

  {-
  Expect : any expr e for which SafeDiv e holds
  can be evaluated without division-by-zero

  can prove SafeDiv is sufficient condition for two notions of evaluation to coincide:

  -- lemma relates the two semantics
  -- expressed as a relation and an evaluator
  -- for those expressions that satisfy the SafeDiv property
  -}

  correct : SafeDiv ⊆ wpPartial ⟦_⟧ _⇓_
  correct (Val x) h = ⇓Base
  correct (Div e1 e2) (e2nz , (sde1 , sde2)) with ⟦ e1 ⟧ | ⟦ e2 ⟧ | correct e1 sde1 | correct e2 sde2
  correct (Div e1 e2) (e2nz , (sde1 , sde2)) | Pure v1      | Pure Zero         | e1⇓v1 | e2⇓Z   = magic (e2nz e2⇓Z)
  correct (Div e1 e2) (e2nz , (sde1 , sde2)) | Pure v1      | Pure (Succ v2)    | e1⇓v1 | e2⇓Sv2 = ⇓Step e1⇓v1 e2⇓Sv2
  correct (Div e1 e2) (e2nz , (sde1 , sde2)) | Pure v1      | Step Abort ⊥→FCRN | e1⇓v1 | ()
  correct (Div e1 e2) (e2nz , (sde1 , sde2)) | Step Abort _ | _                 | ()    | _

  {-
  Instead of manually defining SafeDiv,
  define more general predicate characterising the domain of a partial function:
  -}

  dom : {a : Set} -> {b : a -> Set}
     -> ((x : a)
     -> Partial (b x))
     -> (a -> Set)
  dom f = wpPartial f (\_ _ -> ⊤)

  -- can show that the two semantics agree precisely on the domain of the interpreter:

  sound     : dom       ⟦_⟧       ⊆   wpPartial ⟦_⟧ _⇓_

  complete  : wpPartial ⟦_⟧ _⇓_   ⊆   dom       ⟦_⟧

  -- both proofs proceed by induction on the argument expression

  sound (Val x) h = ⇓Base
  sound (Div e1 e2) h with ⟦ e1 ⟧ | ⟦ e2 ⟧ | sound e1 | sound e2
  sound (Div e1 e2) () | Pure v1      | Pure Zero      | ih1 | ih2
  sound (Div e1 e2) h  | Pure v1      | Pure (Succ v2) | ih1 | ih2 = ⇓Step (ih1 tt) (ih2 tt)
  sound (Div e1 e2) () | Pure x       | Step Abort x₁  | ih1 | ih2
  sound (Div e1 e2) () | Step Abort x | v2             | ih1 | ih2

  inDom : {v : Nat} -> (e : Expr) -> ⟦ e ⟧ == Pure v -> dom ⟦_⟧ e
  inDom (Val x) h = tt
  inDom (Div e1 e2) h with ⟦ e1 ⟧ | ⟦ e2 ⟧
  inDom (Div e1 e2) () | Pure v1      | Pure Zero
  inDom (Div e1 e2) h  | Pure v1      | Pure (Succ v2) = tt
  inDom (Div e1 e2) () | Pure _       | Step Abort _
  inDom (Div e1 e2) () | Step Abort _ | _

  aux : (e : Expr) (v : Nat) -> ⟦ e ⟧ ≡ Pure v -> e ⇓ v
  aux e v eq with sound e (inDom e eq)
  ... | H rewrite eq = H

  wpPartial1 : {e1 e2 : Expr} -> wpPartial ⟦_⟧ _⇓_ (Div e1 e2) -> wpPartial ⟦_⟧ _⇓_ e1
  wpPartial1 {e1} {e2} h with ⟦ e1 ⟧ | inspect ⟦_⟧ e1 | ⟦ e2 ⟧
  wpPartial1 {e1} {e2} () | Pure x       | eq         | Pure Zero
  wpPartial1 {e1} {e2} h  | Pure x       | [[[ eq ]]] | Pure (Succ y) = aux e1 x eq
  wpPartial1 {e1} {e2} () | Pure x       | eq         | Step Abort x₁
  wpPartial1 {e1} {e2} () | Step Abort x | eq         | ve2

  wpPartial2 : {e1 e2 : Expr} -> wpPartial ⟦_⟧ _⇓_ (Div e1 e2) -> wpPartial ⟦_⟧ _⇓_ e2
  wpPartial2 {e1} {e2} h with ⟦ e1 ⟧ | inspect ⟦_⟧ e1 | ⟦ e2 ⟧ | inspect ⟦_⟧ e2
  wpPartial2 {e1} {e2} h  | Pure x       | [[[ eqx ]]] | Pure y        | [[[ eqy ]]] = aux e2 y eqy
  wpPartial2 {e1} {e2} () | Pure x       | [[[ eq ]]]  | Step Abort x₁ | eq2
  wpPartial2 {_}  {_}  () | Step Abort x | eq1         | se2           | eq2

  complete (Val x) h = tt
  complete (Div e1 e2) h
    with ⟦ e1 ⟧ | inspect ⟦_⟧ e1 | ⟦ e2 ⟧ | inspect ⟦_⟧ e2
      | complete e1 (wpPartial1 {e1} {e2} h)
      | complete e2 (wpPartial2 {e1} {e2} h)
  complete (Div e1 e2) h | Pure x       | [[[ eqx ]]] | Pure Zero     | [[[ eqy ]]] | p1 | p2
    rewrite eqx | eqy = magic h
  complete (Div e1 e2) h | Pure x       | [[[ eqx ]]] | Pure (Succ y) | [[[ eqy ]]] | p1 | p2 = tt
  complete (Div e1 e2) h | Pure x       | eq1         | Step Abort x₁ | eq2         | p1 | ()
  complete (Div e1 e2) h | Step Abort x | eq1         | se2           | eq2         | () | p2

  {-
  --------------------------------------------------
  Refinement

  weakest precondition semantics on partial computations give rise
  to a refinement relation on Kleisli arrows of the form a -> Partial b

  can characterise this relation by proving:

  refinement : (f g : a -> Maybe b)
             -> (wpPartial f ⊑ wpPartial g) ↔ (∀ x -> (f x ≡ g x) ∨ (f x ≡ Nothing))

  use refinement to relate Kleisli morphisms,
  and to relate a program to a specification given by a pre- and post- condition

  --------------------------------------------------
  Example: Add (interpreter for stack machine)

  add top two elements; can fail fail if stack has too few elements

  below shows how to prove the definition meets its specification

  Define specification in terms of a pre/post condition.
  -}
  -- specification of a function of type (x : a) -> b x consists of:
  record Spec {l : Level} (a : Set) (b : a -> Set) : Set (suc l) where
    constructor [[_,_]]
    field
      pre   : a -> Set l              -- precondition on 'a'
      post  : (x : a) -> b x -> Set l -- postcondition relating inputs that satisfy this precondition
                                      -- and the corresponding outputs

  {-
  [ P , Q ] : specification consisting of precondition P and postcondition Q

  for non-dependent examples (e.g., type b does not depend on x : a)
  -}

  SpecK : {l : Level} -> Set -> Set -> Set (suc l)
  SpecK a b = Spec a (K b) -- K is constant function

  -- specification for addition function : describes the desired postcondition
  data Add : List Nat -> List Nat -> Set where
    AddStep : {x1 x2 : Nat} -> {xs : List Nat}
           -> Add (x1 :: x2 :: xs) ((x1 + x2) :: xs)

  addSpec : SpecK (List Nat) (List Nat)
  addSpec = [[ (\xs -> length xs > 1) , Add ]]

  {-
  How to relate this specification to an implementation?
  Note: 'wpPartial' assigns predicate transformer semantics to functions
  - but do not yet have a corresponding predicate transform semantics for specifications.
  wpSpec function does that:
  -}

  -- Given a specification, Spec a b
  -- computes the weakest precondition that satisfies an arbitrary postcondition P
  -- i.e., the spec’s precondition should hold and its postcondition must imply P.
  wpSpec : forall {l a} -> {b : a -> Set}
        -> Spec {l} a b
        -> (P : (x : a) -> b x -> Set l)
        -> (a -> Set l)
  wpSpec [[ pre , post ]] P = \x -> (pre x) ∧ (post x ⊆ P x)

  -- Can now formulate program

  pop : forall {a} -> List a -> Partial (a × List a)
  pop Nil       = abort
  pop (x :: xs) = return (x , xs)

  add : List Nat -> Partial (List Nat)
  add xs =
    pop xs >>= \{(x1 , xs) ->
    pop xs >>= \{(x2 , xs) ->
    return ((x1 + x2) :: xs)}}

  -- that refines the specification given by addSpec:
  correctnessAdd : wpSpec addSpec ⊑ wpPartial add
  correctnessAdd P Nil (() , _)
  correctnessAdd P (x :: Nil) (Data.Nat.s≤s () , _)
  correctnessAdd P (x :: y :: xs) (_ , H) = H (x + y :: xs) AddStep

  {-
  This example illustrates how to use the refinement relation
  - to relate a specification
    - given in terms of a pre- and postcondition,
  - to its implementation.

  Compared to the refinement calculus
  - have not yet described how to mix code and specifications (see Section 7)

  --------------------------------------------------
  Alternative semantics
  -}

  product : List Nat -> Nat
  product = foldr _*_ 1

  fastProduct : List Nat -> Partial Nat
  fastProduct Nil          = return 1
  fastProduct (Zero :: xs) = abort
  fastProduct (k :: xs)    = map (_*_ k) (fastProduct xs)

  defaultHandler : forall {a} -> a -> Partial a -> a
  defaultHandler _ (Pure x)       = x
  defaultHandler d (Step Abort _) = d

  wpDefault : forall {a b : Set}
           -> (d : b)
           -> (f : a -> Partial b)
           -> (P : a -> b -> Set)
           -> (a -> Set)
  wpDefault {a} {b} d f P = wp f defaultPT
    where
    defaultPT : (x : a) -> Partial b -> Set
    defaultPT x (Pure y)       = P x y
    defaultPT x (Step Abort _) = P x d

  soundness : forall {a b}
           -> (P : a -> b -> Set)
           -> (d : b)
           -> (c : a -> Partial b)
           -> forall x
           -> wpDefault d c P x
           -> P x (defaultHandler d (c x))
  soundness P d c x H with c x
  soundness P d c x H | Pure y       = H
  soundness P d c x H | Step Abort _ = H

  correctnessProduct : wp product ⊑ wpDefault 0 fastProduct
  correctnessProduct   P Nil H = H
  correctnessProduct   P (Zero :: xs) H = H
  correctnessProduct   P (Succ x :: xs) H
    with fastProduct xs | correctnessProduct (\xs v -> P (Succ x :: xs) _) xs H
  correctnessProduct   P (Succ x :: xs) H | Pure v       | IH                   = IH
  correctnessProduct   P (Succ x :: xs) H | Step Abort _ | IH rewrite *-zeroʳ x = IH

module State (s : Set) where
  open Free
  open Maybe using (SpecK; Spec; [[_,_]]; wpSpec)
  data C : Set where
    Get : C
    Put : s -> C

  R : C -> Set
  R Get     = s
  R (Put _) = ⊤

  State : forall {l} -> Set l -> Set l
  State = Free C R
  get : State s
  get = Step Get return

  put : s -> State ⊤
  put s = Step (Put s) (\_ -> return tt)

  run : {a : Set} -> State a -> s -> a × s
  run (Pure x)         s = (x , s)
  run (Step Get k)     s = run (k s) s
  run (Step (Put s) k) _ = run (k tt) s

  statePT : forall {l l'} -> {b : Set l} -> (b × s -> Set l') -> State b -> (s -> Set l')
  statePT P (Pure x)         = \s ->         P (x , s)
  statePT P (Step  Get    k) = \s -> statePT P (k  s) s
  statePT P (Step (Put s) k) = \_ -> statePT P (k tt) s

  statePT' : forall {l l'} -> {b : Set l}
          -> (s -> b × s -> Set l')
          -> State b
          -> (s -> Set l')
  statePT' P c i = statePT (P i) c i

  wpState : forall {l l' l''} -> {a : Set l} -> {b : Set l'}
         -> (a -> State b)
         -> (P : a × s -> b × s -> Set l'')
         -> (a × s -> Set l'')
  wpState f P (x , i) = wp f ((const \c -> statePT' (\j -> P (x , j)) c i)) x

  soundness : forall {a b : Set}
           -> (P : a × s -> b × s -> Set)
           -> (f : a -> State b)
           -> forall i x
           -> wpState f P (x , i)
           -> P (x , i) (run (f x) i)
  soundness {a} {b} P c i x H = lemma i (c x) H
    where
    lemma : (st : s) -> (statec : State b) -> (statePT (P (x , i)) statec st) -> P (x , i) (run statec st)
    lemma i (Pure y)         H = H
    lemma i (Step Get     k) H = lemma i (k  i) H
    lemma i (Step (Put s) k) H = lemma s (k tt) H

module Relabel where
  open Free
  open Maybe using (Spec; SpecK; [[_,_]]; wpSpec)
  open import Data.Nat public
    using
      (_+_; _>_; _*_
      )
    renaming
      ( ℕ to Nat
      ; zero to Zero
      ; suc to Succ
      )

  module StateNat = State Nat
  open StateNat

  data Tree (a : Set) : Set where
    Leaf  :      a           -> Tree a
    Node  : Tree a -> Tree a -> Tree a

  flatten : ∀ {a} -> Tree a -> List a
  flatten (Leaf x)   = [ x ]
  flatten (Node l r) = flatten l ++ flatten r

  size : ∀ {a} -> Tree a -> Nat
  size (Leaf x)   = 1
  size (Node l r) = size l + size r

  seq : Nat -> Nat -> List Nat
  seq i Zero     = Nil
  seq i (Succ n) = Cons i (seq (Succ i) n)

  relabelSpec : forall {a} -> SpecK (Tree a × Nat) (Tree Nat × Nat)
  relabelSpec = [[ K ⊤ , relabelPost ]]
    where
      relabelPost : forall {a} -> Tree a × Nat -> Tree Nat × Nat -> Set
      relabelPost (t , s) (t' , s') = (flatten t' == (seq (s) (size t))) ∧ (s + size t == s')

  fresh : State Nat
  fresh =  get >>= \n ->
           put (Succ n) >>
           return n

  relabel : forall {a} -> Tree a -> State (Tree Nat)
  relabel (Leaf x)   = map Leaf fresh
  relabel (Node l r) =
    relabel l >>= \l' ->
    relabel r >>= \r' ->
    return (Node l' r')

  correctnessRelabel : forall {a : Set} -> wpSpec ( relabelSpec {a}) ⊑ wpState relabel

  compositionality : forall {a b : Set} -> (c : State a) (f : a -> State b) ->
    ∀ i P -> statePT P (c >>= f) i == statePT (wpState f (const P)) c i

  compositionality (Pure x) f i P = refl
  compositionality (Step Get k) f i P = compositionality (k i) f i P
  compositionality (Step (Put x) k) f i P = compositionality (k tt) f x P

  correctnessRelabel = step2
    where
    open NaturalLemmas
    --   Simplify proofs of refining a specification,
    --   by first proving one side of the bind, then the second.
    --   This is essentially the first law of consequence,
    --   specialized to the effects of State and Spec.
    prove-bind : ∀ {a b} (mx : State a) (f : a -> State b) P i ->
      statePT (wpState f \_ -> P) mx i -> statePT P (mx >>= f) i
    prove-bind mx f P i x = coerce {zero} (sym (compositionality mx f i P)) x

    prove-bind-spec : ∀ {a b} (mx : State a) (f : a -> State b) spec ->
      ∀ P i -> (∀ Q -> Spec.pre spec i × (Spec.post spec i ⊆ Q) -> statePT Q mx i) ->
      Spec.pre spec i × (Spec.post spec i ⊆ wpState f (\_ -> P)) ->
      statePT P (mx >>= f) i
    prove-bind-spec mx f spec P i Hmx Hf = prove-bind mx f P i (Hmx (wpState f (\_ -> P)) Hf)

    --   Partially apply a specification.
    applySpec : ∀ {a b s} -> SpecK {zero} (a × s) (b × s) -> a -> SpecK s (b × s)
    applySpec [[ pre , post ]] x = [[ (\s -> pre (x , s)) , (\s -> post (x , s)) ]]

    --   Ingredients for proving the postcondition holds.
    --   By abstracting over the origin of the numbers,
    --   we can do induction on them nicely.
    append-seq : ∀ a b c -> seq a b ++ seq (a + b) c ≡ seq a (b + c)
    append-seq a Zero c = cong (\a' -> seq a' c) (sym (plus-zero a))
    append-seq a (Succ b) c = cong (Cons a) (trans
      (cong (\a+b -> seq (Succ a) b ++ seq a+b c) (+-succ a b))
      (append-seq (Succ a) b c))

    postcondition : ∀ s s' s'' sl fl sr fr ->
      Pair (fl ≡ seq s sl) (s + sl ≡ s') ->
      Pair (fr ≡ seq s' sr) (s' + sr ≡ s'') ->
      Pair (fl ++ fr ≡ seq s (sl + sr)) (s + (sl + sr) ≡ s'')
    postcondition s .(s + sl) .(s + sl + sr) sl .(seq s sl) sr .(seq (s + sl) sr)
      (refl , refl) (refl , refl) = append-seq s sl sr , +-assoc s sl sr

    --   We have to rewrite the formulation of step2 slightly to make it acceptable for the termination checker.

    step2' : ∀ {a} P (t : Tree a) s -> wpSpec relabelSpec P (t , s) -> statePT (P (t , s)) (relabel t) s
    step2' P (Leaf x) s (fst , snd) = snd (Leaf s , Succ s) (refl , plus-one s)
    step2' P (Node l r) s (fst , snd) = prove-bind-spec (relabel l) _ (applySpec relabelSpec l) _ _
      (\Q -> step2' (\_ -> Q) l s)
      (tt , \l',s' postL -> let l' = Pair.fst l',s' ; s' = Pair.snd l',s'
        in prove-bind-spec (relabel r) _ (applySpec relabelSpec r) _ _
          (\Q -> step2' (\_ -> Q) r s')
          (tt , \r',s'' postR -> let r' = Pair.fst r',s'' ; s'' = Pair.snd r',s''
            in snd (Node l' r' , s'') (postcondition s s' s'' (size l) (flatten l') (size r) (flatten r') postL postR)))
    step2 : wpSpec relabelSpec ⊑ wpState relabel
    step2 P (t , s) (fst , snd) = step2' P t s (fst , snd)

module Compositionality
  (C : Set) (R : C -> Set) (ptalgebra : (c : C) -> (R c -> Set) -> Set)
  where
  open Free
  open Maybe using (wpSpec; [[_,_]])

  postulate
    ext : {l l' : Level} {a : Set l} {b : Set l'} -> {f g : a -> b} ->
      ((x : a) -> f x ≡ g x) -> f ≡ g

  pt : {a : Set} -> Free C R a -> (a -> Set) -> Set
  pt (Pure   x) P = P x
  pt (Step c x) P = ptalgebra c (\r -> pt (x r) P)

  wpCR : {a : Set} {b : a -> Set} ->
      ((x : a) -> Free C R (b x)) -> ((x : a) -> b x -> Set) -> (a -> Set)
  wpCR f P x = pt (f x) (P x)

  compositionality : forall {a b : Set} -> (c : Free C R a) (f : a -> Free C R b) ->
    ∀ P -> pt (c >>= f) P ≡ pt c (wpCR f (const P))
  compositionality (Pure x) f P = refl
  compositionality (Step c x) f P =
    cong (\h -> ptalgebra c h) (ext (\r -> compositionality (x r) f P))

  _>=>_ : forall {l l' l''} -> {a : Set l} -> {b : Set l'} -> {c : Set l''} -> forall {C R} -> (a -> Free C R b) -> (b -> Free C R c) -> a -> Free C R c
  f >=> g = \x -> f x >>= g

  compositionality-left : forall {a b c : Set} -> (f1 f2 : a -> Free C R b) (g : b -> Free C R c) ->
    wpCR f1 ⊑ wpCR f2 ->
    wpCR (f1 >=> g) ⊑ wpCR (f2 >=> g)
  compositionality-left mx my f H P x y
    rewrite compositionality (mx x) f (P x)
    | compositionality (my x) f (P x) =
     H (\x y -> pt (f y) (P x)) x y

  compositionality-right : forall {a b c} -> (f : a -> Free C R b) (g1 g2 : b -> Free C R c) ->
    wpCR g1 ⊑ wpCR g2 ->
    wpCR (f >=> g1) ⊑ wpCR (f >=> g2)
  postulate
    monotonicity : forall {a} -> {P Q : a -> Set} -> P ⊆ Q -> (c : Free C R a) -> pt c P -> pt c Q
  compositionality-right mx f g H P x wp1
    rewrite compositionality (mx x) f (P x)
    | compositionality (mx x) g (P x) = monotonicity (H _) (mx x) wp1

  weakenPre          : {a : Set} -> {b : a -> Set} -> {P P' : a -> Set} -> {Q : (x : a) -> b x -> Set} -> (P ⊆ P') -> (wpSpec [[ P , Q ]] ⊑ wpSpec [[ P' , Q ]])


  strengthenPost     : {a : Set} -> {b : a -> Set} -> {P : a -> Set} -> {Q Q' : (x : a) -> b x -> Set} -> (forall (x : a) -> Q' x ⊆ Q x) -> (wpSpec [[ P , Q ]] ⊑ wpSpec [[ P , Q' ]])

  weakenPre H1 p H2 (pre , post) = (H1 H2 pre , post)

  strengthenPost H1 p H2 (pre , post) = (pre , \x y -> post x (H1 _ x y))

module Laws (s : Set) where
  open Free
  open Maybe using (SpecK; Spec; [[_,_]]; wpSpec)
  module StateS = State s
  open StateS

  postulate
    a : Set
    k0 : State a
    k1 : s -> State a
    k2 : s -> s -> State a
    x y : s

  _≃_ : forall {b : Set} -> State b -> State b -> Set₁
  t1 ≃ t2 = (wpState' t1 ⊑ wpState' t2) ∧ (wpState' t2 ⊑ wpState' t1)
    where
    wpState' : forall {b} -> State b -> (P : s -> b × s -> Set) -> (s -> Set)
    wpState' {b} t P s = wpState {a = ⊤} {b} (\_ -> t) (\{(tt , s') y -> P s' y}) (tt , s)

module Nondeterminism where

  open Free hiding (_⊆_)
  open Maybe using (wpSpec; SpecK; [[_,_]])
  data C : Set where
    Fail : C
    Choice : C

  R : C -> Set
  R Fail   = ⊥
  R Choice = Bool

  ND : Set -> Set
  ND = Free C R

  fail : forall {a} -> ND a
  fail = Step Fail (\())

  choice : forall {a} -> ND a -> ND a -> ND a
  choice c1 c2 = Step Choice (\b -> if b then c1 else c2)

  allPT : forall {a : Set} -> {b : a -> Set} -> (P : (x : a) -> b x -> Set) -> (x : a) -> ND (b x) -> Set
  allPT P _ (Pure x)        = P _ x
  allPT P _ (Step Fail k)   = ⊤
  allPT P _ (Step Choice k) = allPT P _ (k True) ∧ allPT P _ (k False)

  wpAll : forall {a   : Set} -> {b : a -> Set} -> ((x : a) -> ND (b x)) -> (P : (x : a) -> b x -> Set) -> (a -> Set)
  wpAll f P = wp f (allPT P)

  anyPT : forall {a : Set} -> {b : a -> Set} -> (P : (x : a) -> b x -> Set) -> (x : a) -> ND (b x) -> Set
  anyPT P _ (Pure x)        = P _ x
  anyPT P _ (Step Fail k)   = ⊥
  anyPT P _ (Step Choice k) = anyPT P _ (k True) ∨ anyPT P _ (k False)

  wpAny : forall {a   : Set} -> {b : a -> Set} -> ((x : a) -> ND (b x)) -> (P : (x : a) -> b x -> Set) -> (a -> Set)
  wpAny f P = wp f (anyPT P)

  run : forall {a} -> ND a -> List a
  run (Pure x)        = [ x ]
  run (Step Fail _)   = Nil
  run (Step Choice k) = run (k True) ++ run (k False)

  All : {a : Set} -> (a -> Set) -> List a -> Set
  All P Nil = ⊤
  All P (x :: xs) = P x ∧ All P xs

  All++ : {a : Set} (P : a -> Set) (xs ys : List a) ->
    All P xs -> All P ys -> All P (xs ++ ys)
  All++ P Nil ys H1 H2 = H2
  All++ P (x :: xs) ys (Px , H1) H2 = Px , All++ P xs ys H1 H2

  allSoundness : {a : Set} {b : a -> Set} (P : (x : a) -> b x -> Set) (x : a) (nd : ND (b x)) ->
    allPT P x nd -> All (P x) (run nd)
  allSoundness P x (Pure y) H = H , tt
  allSoundness P x (Step Fail _) H = tt
  allSoundness P x (Step Choice k) (H1 , H2) =
    All++ (P x) (run (k True)) (run (k False)) (allSoundness P x (k True) H1) (allSoundness P x (k False) H2)

  wpAllSoundness : forall {a} -> {b : a -> Set} -> (f : (x : a) -> ND (b x)) ->
    ∀ P x -> wpAll f P x -> All (P x) (run (f x))
  wpAllSoundness nd P x H = allSoundness P x (nd x) H
  data Elem {a : Set} (x : a) : ND a -> Set where
      Here   : Elem x (Pure x)
      Left   : forall {k} ->  Elem x (k True)  -> Elem x (Step Choice k)
      Right  : forall {k} ->  Elem x (k False) -> Elem x (Step Choice k)

  _⊆_ : forall {a} -> ND a -> ND a -> Set
  nd1 ⊆ nd2 = ∀ x -> Elem x nd1 -> Elem x nd2

  _<->_ : {l l' : Level} (a : Set l) (b : Set l') -> Set (l ⊔ l')
  a <-> b = Pair (a -> b) (b -> a)

  refineAll  : {a b : Set} {x : a} (f g : a -> ND b) -> (wpAll f  ⊑ wpAll g)  <-> ((x : a) -> g x  ⊆ f x)

  refineAny  : {a b : Set} {x : a} (f g : a -> ND b) -> (wpAny f  ⊑ wpAny g)  <-> ((x : a) -> f x  ⊆ g x)
  allP : ∀ {a b : Set} {x : a} P (S : ND b) -> allPT P x S <-> (∀ y -> Elem y S -> P x y)
  Pair.fst (allP P (Pure y)) H y Here = H
  Pair.fst (allP P (Step Choice k)) (H , _) y (Left i) = Pair.fst (allP P (k True)) H y i
  Pair.fst (allP P (Step Choice k)) (_ , H) y (Right i) = Pair.fst (allP P (k False)) H y i
  Pair.snd (allP P (Pure y)) H = H y Here
  Pair.snd (allP P (Step Fail k)) H = tt
  Pair.snd (allP P (Step Choice k)) H = (Pair.snd (allP P (k True)) λ y i -> H y (Left i)) , (Pair.snd (allP P (k False)) λ y i -> H y (Right i))

  anyP : ∀ {a b : Set} {x : a} P (S : ND b) -> anyPT P x S <-> Sigma b λ y -> Elem y S ∧ P x y
  Pair.fst (anyP P (Pure y)) H = y , (Here , H)
  Pair.fst (anyP P (Step Fail k)) ()
  Pair.fst (anyP P (Step Choice k)) (Inl H) with Pair.fst (anyP P (k True)) H
  Pair.fst (anyP P (Step Choice k)) (Inl H) | y , (i , IH) = y , (Left i , IH)
  Pair.fst (anyP P (Step Choice k)) (Inr H) with Pair.fst (anyP P (k False)) H
  Pair.fst (anyP P (Step Choice k)) (Inr H) | y , (i , IH) = y , (Right i , IH)
  Pair.snd (anyP P (Pure y)) (.y , (Here , H)) = H
  Pair.snd (anyP P (Step .Choice k)) (y , (Left i , H)) = Inl (Pair.snd (anyP P (k True)) (y , (i , H)))
  Pair.snd (anyP P (Step .Choice k)) (y , (Right i , H)) = Inr (Pair.snd (anyP P (k False)) (y , (i , H)))

  Pair.fst (refineAll f g) H x y i = Pair.fst (allP (λ _ y' -> Elem y' (f x)) (g x)) (H _ x (Pair.snd (allP _ (f x)) (λ _ -> id))) y i
  Pair.snd (refineAll f g) r P x H = Pair.snd (allP P (g x)) λ y i -> Pair.fst (allP P (f x)) H y (r x y i)
  Pair.fst (refineAny f g) H x y i with Pair.fst (anyP (λ _ y' -> y' == y) (g x)) (H _ x (Pair.snd (anyP _ (f x)) (y , (i , refl))))
  Pair.fst (refineAny f g) H x y i | .y , (i' , refl) = i'
  Pair.snd (refineAny f g) r P x H with Pair.fst (anyP P (f x)) H
  Pair.snd (refineAny f g) r P x H | y , (i , IH) = Pair.snd (anyP P (g x)) (y , ((r x y i) , IH))

  selectPost : forall {a} -> List a -> a × List a -> Set
  selectPost xs (y , ys) = Sigma (y ∈ xs) (\e -> delete xs e == ys)

  removeSpec : forall {a} -> SpecK (List a) (a × List a)
  removeSpec = [[ K ⊤ , selectPost ]]

  remove : forall {a} -> List a -> ND (a × List a)
  remove Nil       = fail
  remove (x :: xs) = choice  (return (x , xs)) (map (retain x) (remove xs))
      where
      retain : forall {a} -> a -> a × List a -> a × List a
      retain x (y , ys) = (y , (x :: ys))

  removeCorrect : forall {a} -> wpSpec {a = List a} {const (a × List a)} removeSpec ⊑ wpAll remove
  removeCorrect P Nil (tt , snd) = tt
  removeCorrect P (x :: xs) (tt , snd) =
    snd (x , xs) (∈Head , refl) ,
    mapPT P (x :: xs) xs (remove xs) _
      (removeCorrect _ xs (tt , (λ {(x' , xs') (i , H) -> snd (x' , (x :: xs')) (∈Tail i , cong (x ::_) H)})))
    where
    mapPT : ∀ {a b c : Set} P (x x' : a) (S : ND b) (f : b -> c) -> allPT (λ _ y -> P x (f y)) x' S -> allPT P x (map f S)
    mapPT P x x' (Pure y) f H = H
    mapPT P x x' (Step Fail k) f H = H
    mapPT P x x' (Step Choice k) f (fst , snd) = mapPT P x x' (k True) f fst , mapPT P x x' (k False) f snd

  trivialCorrect : forall {a} -> wpSpec {a = List a} {const (a × List a)} removeSpec ⊑ wpAll (const fail)
  trivialCorrect = \P xs H -> tt

  completeness : forall {a} -> (y : a) (xs ys : List a) -> selectPost xs (y , ys) -> Elem (y , ys) (remove xs)
  completeness y (y :: _) ys (∈Head , refl) = Left Here
  completeness y (x :: xs) .(x :: delete xs fst) (∈Tail fst , refl) = Right (inMap _ (remove xs) _ (completeness y _ _ (fst , refl)))
    where
    inMap : ∀ {a b : Set} (x : a) S (f : a -> b) -> Elem x S -> Elem (f x) (map f S)
    inMap x (Pure x) f Here = Here
    inMap x (Step Choice k) f (Left i) = Left (inMap x (k True) f i)
    inMap x (Step Choice k) f (Right i) = Right (inMap x (k False) f i)

module Recursion where
  open Free
  open import Data.Nat public
    using
      (_+_; _>_; _*_
      )
    renaming
      ( ℕ to Nat
      ; zero to Zero
      ; suc to Succ
      ; _∸_ to _-_
      )
  open NaturalLemmas
  open Maybe hiding (soundness)

  _~~>_ : (I : Set) (O : I -> Set) -> Set
  I ~~> O = (i : I) -> Free I O (O i)

  call : forall {I O} -> (i : I) -> Free I O (O i)
  call x = Step x Pure

  f91 : Nat ~~> K Nat
  f91 i with 100 lt i
  f91 i | yes  _ = return (i - 10)
  f91 i | no   _ = call (i + 11) >>= call

  f91Post : Nat -> Nat -> Set
  f91Post i o with 100 lt i
  f91Post i o | yes _ = o == i - 10
  f91Post i o | no _  = o == 91

  f91Spec : SpecK Nat Nat
  f91Spec = [[ K ⊤ , f91Post ]]

  invariant : forall {I} -> {O : I -> Set} -> (i : I) -> Spec I O -> Free I O (O i) -> Set
  invariant i [[ pre , post ]] (Pure x)   =  pre i -> post i x
  invariant i [[ pre , post ]] (Step j k) =  (pre i -> pre j)
                                              ∧ ∀ o -> post j o -> invariant i [[ pre , post ]] (k o)
  wpRec : forall {I} -> {O : I -> Set} -> Spec I O -> (f : I ~~> O) -> (P : (i : I) -> O i -> Set) -> (I -> Set)
  wpRec spec f P i = wpSpec spec P i ∧ invariant i spec (f i)

  f91Partial-correctness : wpSpec f91Spec ⊑ wpRec f91Spec f91
  f91Partial-correctness P i with 100 lt i
  f91Partial-correctness P i | yes p with 100 lt i
  f91Partial-correctness P i | yes p | yes _ = \H -> (tt , (\x eq -> Pair.snd H _ eq)) , (\x -> refl)
  f91Partial-correctness P i | yes p | no ¬p = magic (¬p p)
  f91Partial-correctness P i | no ¬p = \x -> (tt , (\x₁ x₂ -> Pair.snd x x₁ x₂)) ,
                                              ((\_ -> tt) , (\o x₁ -> (\x₂ -> tt) ,
                                              (\o₁ x₂ x₃ -> lemma i o _ ¬p x₁ x₂)))
    where
    open Data.Nat
    open import Data.Nat.Properties

    100-≮-91 : (i : Nat) -> ¬ (i + 10 ≤ i)
    100-≮-91 Zero ()
    100-≮-91 (Succ i) (s≤s pf) = 100-≮-91 i pf

    plus-minus : ∀ b c -> (b + c) - c == b
    plus-minus b c = trans (+-∸-assoc b (NaturalLemmas.≤-refl {c})) (trans (cong (b +_) (n∸n≡0 c)) (sym (plus-zero b)))

    plus-plus-minus : ∀ i -> i + 11 - 10 ≡ Succ i
    plus-plus-minus i = plus-minus (Succ i) 11

    between : ∀ a b -> ¬ (a < b) -> a < Succ b -> a ≡ b
    between Zero Zero ¬lt ltSucc = refl
    between Zero (Succ b) ¬lt ltSucc = magic (¬lt (s≤s z≤n))
    between (Succ a) Zero ¬lt (s≤s ())
    between (Succ a) (Succ b) ¬lt (s≤s ltSucc) = cong Succ (between a b (¬lt ∘ s≤s) ltSucc)

    lemma : ∀ i o o' -> ¬ (100 < i) ->
      f91Post (i + 11) o -> f91Post o o' -> f91Post i o'
    lemma i o o' i≤100 oPost o'Post with 100 lt i
    ... | yes p = magic (i≤100 p)
    ... | no ¬p with 100 lt o
    lemma i o .(o - 10) i≤100 oPost refl | no ¬p | yes p with 100 lt (i + 11)
    lemma i .(i + 11 - 10) .(i + 11 - 10 - 10) i≤100 refl refl | no ¬p | yes p | yes p₁ with between 100 i i≤100 (subst (\i' -> 100 < i') (plus-plus-minus i) p)
    lemma .100 .101 .91 i≤100 refl refl | no ¬p | yes p | yes p₁ | refl = refl
    lemma i .91 .81 i≤100 refl refl | no ¬p | yes p | no ¬p₁ = magic (100-≮-91 91 p)
    lemma i o o' i≤100 oPost o'Post | no ¬p | no ¬p₁ = o'Post

  petrol : forall {I O a} -> (f : I ~~> O) -> Free I O a -> Nat -> Partial a
  petrol f (Pure x)    n        = return x
  petrol f (Step _ _)  Zero     = abort
  petrol f (Step c k)  (Succ n) = petrol f (f c >>= k) n

  mayPT : forall {a} -> (a -> Set) -> (Partial a -> Set)
  mayPT P (Pure x)       = P x
  mayPT P (Step Abort _) = ⊤

  soundness : forall {I O} -> (f : I ~~> O) (spec : Spec I O) (P : (i : I) -> O i -> Set) ->
    (∀ i -> wpRec spec f P i) -> ∀ n i -> mayPT (P i) (petrol f (f i) n)
  soundness f spec P wpH n i = soundness' f spec P (f i) n wpH (wpH i)
    where
    invariant-compositionality : ∀ {I} {O : I -> Set} {i i'} spec
      (S : Free I O (O i)) (k : (O i) -> Free I O (O i')) ->
      invariant i spec S -> Spec.pre spec i -> (∀ o -> Spec.post spec i o -> invariant i' spec (k o)) ->
      invariant i' spec (S >>= k)
    invariant-compositionality spec (Pure x) k SH preH kH = kH x (SH preH)
    invariant-compositionality spec (Step c k') k (fst , snd) preH kH = (\_ -> fst preH) , \o postH -> invariant-compositionality spec (k' o) k (snd o postH) preH kH
    soundness' : ∀ {I} {O : I -> Set} {i}
      (f : (i : I) -> Free I O (O i)) (spec : Spec I O) (P : (i : I) -> O i -> Set)
      (S : Free I O (O i)) (n : Nat) ->
      (∀ i -> wpRec spec f P i) ->
      wpSpec spec P i ∧ invariant i spec S ->
      mayPT (P i) (petrol f S n)
    soundness' f spec P (Pure x) n wpH ((preH , postH) , invH) = postH x (invH preH)
    soundness' f spec P (Step c k) Zero wpH H = tt
    soundness' f spec P (Step c k) (Succ n) wpH (specH , (preH , postH)) = soundness' f spec P (f c >>= k) n wpH (specH , invariant-compositionality spec (f c) k (Pair.snd (wpH c)) (preH (Pair.fst specH)) postH)

module Mix (C : Set) (R : C -> Set) (ptalgebra : (c : C) -> (R c -> Set) -> Set) where
  open Free hiding (_>>=_)
  open Maybe using (SpecK; [[_,_]]; Spec; wpSpec)

  SpecVal : Set -> Set₁
  SpecVal a = SpecK ⊤ a

  data I (a : Set) : Set₁ where
    Done  : a -> I a
    Hole  : SpecVal a -> I a

  ptI : forall {a} -> I a -> (a -> Set) -> Set
  ptI (Done x)     P = P x
  ptI (Hole spec)  P = wpSpec spec (const P) tt

  M : Set -> Set₁
  M a = Free C R (I a)

  isExecutable : forall {a} -> M a -> Set
  isExecutable (Pure (Done _)) = ⊤
  isExecutable (Pure (Hole _)) = ⊥
  isExecutable (Step c k)      = ∀ r -> isExecutable (k r)

  pt : forall {l} -> {a : Set l} -> Free C R a -> (a -> Set) -> Set
  pt (Pure x) P   = P x
  pt (Step c x) P = ptalgebra c (\r -> pt (x r) P)

  wpCR : forall {l l'} -> {a : Set l} -> {b : a -> Set l'} -> ((x : a) -> Free C R (b x)) -> ((x : a) -> b x -> Set) -> (a -> Set)
  wpCR f P x = pt (f x) (P x)

  wpM : {a : Set} -> {b : a -> Set} -> ((x : a) -> M (b x)) -> ((x : a) -> b x -> Set) -> (a -> Set)
  wpM f P x = wpCR f (\x ix -> ptI ix (P x)) x

module StateExample where
  open Free hiding (_>>=_)
  open Maybe using (Spec; SpecK; [[_,_]]; wpSpec)
  open State Nat

  --   We have to redo the Mix section since our specifications incorporate the state
  SpecVal : ∀ {l} -> Set -> Set (suc l)
  SpecVal = SpecK Nat

  data I {l : Level} (a : Set) : Set (suc l) where
    Done  : a -> I a
    Hole  : SpecVal {l} (a × Nat) -> I a

  M : {l : Level} -> Set -> Set (suc l)
  M a = State (I a)

  ptI : forall {l a} -> I a -> (a × Nat -> Set l) -> Nat -> Set l
  ptI (Done x)     P t = P (x , t)
  ptI (Hole spec)  P t = wpSpec spec (\_ -> P) t

  wpM : forall {l l'} {a : Set l} {b : Set} -> (a -> M b) -> (a × Nat -> b × Nat -> Set l') -> (a × Nat -> Set l')
  wpM f P = wpState f (\i o -> ptI (Pair.fst o) (P i) (Pair.snd o))

  ptM : {a : Set} -> M a -> (Nat -> a × Nat -> Set) -> (Nat -> Set)
  ptM S post t = wpM (λ _ -> S) (λ _ -> (post t)) (⊤ , t)

  liftM : ∀ {l : Level} {a} -> M {l} a -> M {suc l} a
  liftM (Pure (Done x)) = Pure (Done x)
  liftM (Pure (Hole [[ pre , post ]])) = Pure (Hole [[ (λ x -> Lift _ (pre x)) , (λ i o -> Lift _ (post i o)) ]])
  liftM (Step c k) = Step c λ x -> liftM (k x)

  _>>=_ : forall {l a b} -> (M {l} a) -> (a -> M {l} b) -> M {suc l} b
  Pure (Done x)                >>= f = liftM (f x)
  Pure (Hole [[ pre , post ]]) >>= f =
    Pure (Hole [[ (λ n -> Lift _ (pre n)) ,
      (\i ynat -> ∀ x -> post i (x , i) -> ∀ P -> wpM f P (x , i) -> P (x , i) ynat
      ) ]] )
  (Step c k) >>= f        = Step c (\r -> k r >>= f)

  _>=>_ : forall {l : Level} {a b c : Set} -> (a -> M {l} b) -> (b -> M {l} c) -> a -> M {suc l} c
  (f >=> g) x = f x >>= g

  specV : {a : Set} -> SpecVal {zero} (a × Nat) -> M a
  specV spec = Pure (Hole spec)

  done   : forall {a} -> a -> M {zero} a
  get'   : M Nat
  put'   : Nat -> M ⊤
  done x = Pure (Done x)
  get' = Step Get done
  put' t = Step (Put t) done

  getPost : Nat -> Nat × Nat -> Set
  getPost t (x , t') = (t == x) ∧ (t == t')

  putPost : Nat -> Nat -> ⊤ × Nat -> Set
  putPost t _ (_ , t') = t == t'

  _▹_ : {a b : Set} -> (Q : a -> b -> Set) (P : a -> Set) -> b -> Set
  _▹_ {a} Q P = \y -> Sigma a (\x -> P x ∧ Q x y)
  _◃_ : {a b c : Set} -> (Q : a -> b -> Set) -> (SpecK a c) -> b -> c -> Set
  _◃_ Q [[ pre , post ]] = \y z -> ∀ x -> pre x ∧ Q x y -> post x z

  step : forall {b} -> (c : C) (spec : SpecVal {zero} (b × Nat)) -> SpecK {zero} (R c × Nat) (b × Nat)
  step Get      [[ pre , post ]] = [[ getPost ▹ pre , getPost ◃ [[ pre , post ]] ]]
  step (Put x)  [[ pre , post ]] = [[ (putPost x) ▹ pre , (putPost x) ◃ [[ pre , post ]] ]]

  intros : forall {a b} -> SpecK {zero} (a × Nat) (b × Nat) -> a -> SpecVal (b × Nat)
  intros [[ pre , post ]] x = [[ (\t -> pre (x , t)) , (\t -> post (x , t)) ]]

  data Derivation {b} (spec : SpecVal (b × Nat)) : Set₁ where
    Done : (x : b) -> wpSpec spec ⊑ ptM (done x) -> Derivation spec
    Step : (c : C) -> (∀ (r : R c) -> Derivation (intros (step c spec) r)) -> Derivation spec

  extract : forall {b} -> (spec : SpecVal (b × Nat)) -> Derivation spec -> State b
  extract _ (Done x _) = Pure x
  extract _ (Step c k) = Step c (\r -> extract _ (k r))

  DerivationFun : {a b : Set} (spec : SpecK (a × Nat) (b × Nat)) -> Set₁
  DerivationFun {a} {b} spec = (x : a) -> Derivation (intros spec x)

  stepMonotone : {a : Set} (c : C) (r : R c) {spec spec' : SpecVal (a × Nat)} ->
    wpSpec spec ⊑ wpSpec spec' ->
    wpSpec (intros (step c spec) r) ⊑ wpSpec (intros (step c spec') r)
  stepMonotone {a} Get r {spec} {spec'} H P .r ((.r , (fst₁ , (refl , refl))) , snd) = (r , (Pair.fst (H (\_ _ -> ⊤) r (fst₁ , (\x _ -> tt))) , (refl , refl))) , \x x₁ -> snd x (postLemma r x x₁)
    where
    postLemma : ∀ r
      (x : Pair a Nat) ->
      (∀ x₁ ->
      Pair (Spec.pre spec' x₁) (Pair (x₁ ≡ r) (x₁ ≡ r)) ->
      Spec.post spec' x₁ x) ->
      ∀ x₁ ->
      Pair (Spec.pre spec x₁) (Pair (x₁ ≡ r) (x₁ ≡ r)) ->
      Spec.post spec x₁ x
    postLemma r x x₂ .r (fst , (refl , refl)) = Pair.snd (H (Spec.post spec) r (fst , (\x₃ z -> z))) x (x₂ r ((Pair.fst (H (\_ _ -> ⊤) r (fst , (\x₃ _ -> tt)))) , (refl , refl)))
  stepMonotone {a} (Put t) r {spec} {spec'} H P .t ((fst , (fst₁ , refl)) , snd) = (fst , (Pair.fst (H (\_ _ -> ⊤) fst (fst₁ , (\x _ -> tt))) , refl)) , \x x₁ -> snd x (postLemma t x x₁)
    where
      postLemma : ∀ (t : Nat)
        (x : Pair a Nat) ->
        (∀ x₁ -> Pair (Spec.pre spec' x₁) (t ≡ t) -> Spec.post spec' x₁ x) ->
        ∀ x₁ ->
        Pair (Spec.pre spec x₁) (t ≡ t) -> Spec.post spec x₁ x
      postLemma t x x₁ x₂ (fst , refl) = Pair.snd (H (Spec.post spec) x₂ (fst , (\x₃ z -> z))) x (x₁ x₂ ((Pair.fst (H (\_ _ -> ⊤) x₂ (fst , (\x₃ _ -> tt)))) , refl))

  open import Data.Nat
  open import Data.Nat.Properties
  open NaturalLemmas hiding (≤-refl ; ≤-trans)

  data All {a : Set} (P : a -> Set) : List a -> Set where
    AllNil : All P Nil
    AllCons : ∀ {x xs} -> P x -> All P xs -> All P (x :: xs)

  unAllCons : ∀ {a P x} {xs : List a} ->
    All P (x :: xs) -> All P xs
  unAllCons (AllCons x₁ x₂) = x₂

  maxPre : List Nat × Nat -> Set
  maxPre (xs , i) = (i == 0) ∧ (¬ (xs == Nil))

  maxPost : List Nat × Nat -> Nat × Nat -> Set
  maxPost (xs , i) (o , _) = All (o ≥_) xs ∧ (o ∈ xs)

  maxSpec = [[ maxPre , maxPost ]]

  refineDerivation : forall {a : Set} -> {spec spec' : SpecVal (a × Nat)} -> wpSpec spec ⊑ wpSpec spec' -> Derivation spec' -> Derivation spec
  refineDerivation H (Done x Hx) = Done x (⊑-trans H Hx)
  refineDerivation H (Step c d) = Step c \r -> refineDerivation (stepMonotone c r H) (d r)

  maxPost' : List Nat × Nat -> Nat × Nat -> Set
  maxPost' (xs , i) (o , _) = All (o ≥_) (i :: xs) × (o ∈ (i :: xs))

  maxProof : ∀ (xs : List Nat) ->
    wpSpec (intros [[ maxPre , maxPost ]] xs) ⊑
    wpSpec (intros [[ K ⊤ , maxPost' ]] xs)
  maxProof xs P .0 ((refl , Hnil) , snd) = tt , \o H -> snd o (unAllCons (Pair.fst H) , lemma xs Hnil (Pair.fst o) H)
    where
    lemma : ∀ xs -> ¬ (xs == Nil) ->
      ∀ w -> Pair (All (\n -> n ≤ w) (0 :: xs)) (w ∈ (0 :: xs)) -> w ∈ xs
    lemma Nil Hnil w H = magic (Hnil refl)
    lemma (.0 :: xs) _ .0 (AllCons x₂ (AllCons z≤n fst) , ∈Head) = ∈Head
    lemma (x :: xs) _ w (_ , ∈Tail snd) = snd

  max'ProofNil : ∀ i ->
    wpSpec (intros (step Get (intros [[ K ⊤ , maxPost' ]] Nil)) i) ⊑ ptM (done i)
  max'ProofNil i P .i ((.i , (fst₁ , (refl , refl))) , snd) = snd (i , i) (lemma i)
    where
    lemma : ∀ i x ->
      Pair ⊤ (Pair (x ≡ i) (x ≡ i)) ->
      Pair (All (\n -> n ≤ i) (x :: Nil)) (i ∈ (x :: Nil))
    lemma i .i (fst , (refl , refl)) = (AllCons ≤-refl AllNil) , ∈Head

  max'Proof1 : ∀ x xs i ->
    Succ x ≤ i ->
    ∀ (P : Nat -> Nat × Nat -> Set) x₁ ->
    Pair (Sigma ℕ (\x₂ -> Pair ⊤ (Pair (x₂ ≡ i) (x₂ ≡ x₁))))
    (∀ x₂ ->
    (∀ x₃ ->
    Pair ⊤ (Pair (x₃ ≡ i) (x₃ ≡ x₁)) ->
    Pair (All (\n -> n ≤ Pair.fst x₂) (x₃ :: x :: xs))
    (Pair.fst x₂ ∈ (x₃ :: x :: xs))) ->
    P x₁ x₂) ->
    Pair ⊤
    (∀ x₂ ->
    Pair (All (\n -> n ≤ Pair.fst x₂) (x₁ :: xs))
    (Pair.fst x₂ ∈ (x₁ :: xs)) ->
    P x₁ x₂)
  max'Proof1 x xs i x<i P .i ((.i , (_ , (refl , refl))) , snd) = tt , \x₂ x₁ -> snd x₂ (lemma x₂ x₁)
    where
    lemma : ∀ (x₂ : Nat × Nat) ->
      Pair (All (\n -> n ≤ Pair.fst x₂) (i :: xs))
      (Pair.fst x₂ ∈ (i :: xs)) ->
      ∀ x₃ -> Pair ⊤ (Pair (x₃ ≡ i) (x₃ ≡ i)) ->
      Pair (All (\n -> n ≤ Pair.fst x₂) (x₃ :: x :: xs))
      (Pair.fst x₂ ∈ (x₃ :: x :: xs))
    lemma x₂ (AllCons x₁ fst , ∈Head) .i (_ , (refl , refl)) = (AllCons x₁ (AllCons (<⇒≤ x<i) fst)) , ∈Head
    lemma x₂ (AllCons x₁ fst , ∈Tail snd) _ (_ , (refl , refl)) = (AllCons x₁ (AllCons (≤-trans (<⇒≤ x<i) x₁) fst)) , ∈Tail (∈Tail snd)

  max'Proof2 : ∀ i x xs -> (Succ x ≤ i -> ⊥) ->
    ∀ (P : Nat -> Nat × Nat -> Set) x₁ -> Pair (Sigma ℕ (\x₂ -> Pair (Sigma ℕ (\x₃
    -> Pair ⊤ (Pair (x₃ ≡ i) (x₃ ≡ x₂)))) (x ≡ x₁))) (∀ x₂ -> (∀ x₃ -> Pair (Sigma
    ℕ (\x₄ -> Pair ⊤ (Pair (x₄ ≡ i) (x₄ ≡ x₃)))) (x ≡ x₁) -> ∀ x₄ -> Pair ⊤ (Pair
    (x₄ ≡ i) (x₄ ≡ x₃)) -> Pair (All (\n -> n ≤ Pair.fst x₂) (x₄ :: x :: xs))
    (Pair.fst x₂ ∈ (x₄ :: x :: xs))) -> P x₁ x₂) -> Pair ⊤ (∀ x₂ -> Pair (All (\n
    -> n ≤ Pair.fst x₂) (x₁ :: xs)) (Pair.fst x₂ ∈ (x₁ :: xs)) -> P x₁ x₂)
  max'Proof2 i x xs x≥i P .x ((.i , ((.i , (fst₂ , (refl , refl))) , refl)) , snd) = tt , \x₄ x₁ -> snd x₄ (lemma x₄ x₁)
    where
    lemma : ∀ (x₄ : Pair Nat Nat) -> Pair (All (\n -> n ≤ Pair.fst x₄) (x :: xs))
      (Pair.fst x₄ ∈ (x :: xs)) -> ∀ x₃ -> Pair (Sigma ℕ (\x₅ -> Pair ⊤ (Pair (x₅
      ≡ i) (x₅ ≡ x₃)))) (x ≡ x) -> ∀ x₅ -> Pair ⊤ (Pair (x₅ ≡ i) (x₅ ≡ x₃)) -> Pair
      (All (\n -> n ≤ Pair.fst x₄) (x₅ :: x :: xs)) (Pair.fst x₄ ∈ (x₅ :: x :: xs))
    lemma (_ , _) (AllCons x fst , ∈Head) _ ((_ , (_ , (refl , refl))) , refl) _ (_ , (refl , refl)) = (AllCons (≮⇒≥ x≥i) (AllCons x fst)) , (∈Tail ∈Head)
    lemma x₄ (AllCons x₁ fst , ∈Tail snd) _ ((_ , (_ , (refl , refl))) , refl) _ (_ , (refl , refl)) = (AllCons (≤-trans (≮⇒≥ x≥i) x₁) (AllCons x₁ fst)) , (∈Tail (∈Tail snd))

  maxSpec' = [[ K ⊤ , maxPost' ]]

  max' : (xs : List Nat) -> Derivation (intros maxSpec' xs)
  max' Nil       = Step Get \i ->
                     Done i (max'ProofNil i)
  max' (x :: xs) = Step Get \i ->
                     if' x <? i
                       then (\lt  -> refineDerivation (max'Proof1 x xs i lt) (max' xs))
                       else (\geq -> Step (Put x) (const (refineDerivation (max'Proof2 i x xs geq) (max' xs))))

  max : DerivationFun [[ maxPre , maxPost ]]
  max xs = refineDerivation (maxProof xs) (max' xs)
