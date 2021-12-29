module z-07 where

open import Data.Bool        hiding (if_then_else_ ; _≟_ ; _<_ ; _<?_)
open import Data.Empty
open import Data.Maybe
open import Data.Nat         renaming (_+_ to _+ℕ_ ; _<?_ to _<?ℕ_) hiding (_<_)
open import Data.Product
open import Data.Sum
open import Data.Unit
open import Function
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

------------------------------------------------------------------------------
-- program

-- 1.4.3 Safety : HC : do not use this.  Use the one defined in ch 7.
-- data SLProg : Set where -- S(mall)L(anguage)Prog(ram)
--   SLBool : Bool        → SLProg
--   SLNat  : ℕ           → SLProg
--   SLAdd  : SLProg      → SLProg   → SLProg
--   SLLt   : SLProg      → SLProg   → SLProg
--   SLIf   : SLProg      → SLProg   → SLProg   → SLProg

-- 7.1 Safety of a simple language : subject reduction and progress
data Value : Set where
  VNat  : ℕ    → Value
  VBool : Bool → Value

data Prog : Set where
  V             : Value → Prog
  _+_           : Prog  → Prog → Prog
  _<_           : Prog  → Prog → Prog
  if_then_else_ : Prog  → Prog → Prog → Prog

infix 50 _+_
infix 40 _<_
infix 30 if_then_else_

------------------------------------------------------------------------------
-- reduction

_<ℕ_ : ℕ → ℕ → Bool
l <ℕ r with l <?ℕ r
... | yes _ = true
... | no  _ = false

-- 1.4.3 one step of reduction
red : Prog → Maybe Prog
red (V (VBool _)) = nothing
red (V (VNat  _)) = nothing
red (V (VNat l) + V (VNat r)) = just (V (VNat (l +ℕ r)))
red (l + r) =
  case red l of λ where
    (just l') → just (l' + r)
    nothing   → case red r of λ where
                  (just r') → just (l + r')
                  nothing   → nothing
red (V (VNat l) < V (VNat r)) = just (V (VBool (l <ℕ r)))
red (l < r) =
  case red l of λ where
    (just l') → just (l' < r)
    nothing   → case red r of λ where
                  (just r') → just (l < r')
                  nothing   → nothing
red (if (V (VBool  true)) then t else _) = just t
red (if (V (VBool false)) then _ else f) = just f
red (if c             then t else f) =
  case red c of λ where
    (just c') → just (if c' then t else f)
    nothing   → nothing

_ : red (if (V (VBool true)) then (V (VNat 3) + V (VNat 4)) else (V (VNat 100)))
  ≡                    just (V (VNat 3) + (V (VNat 4)))
_ = refl

-- reduction relation
data _⇒_ : Prog → Prog → Set where
  ⇒-Add   : (m n : ℕ)
          → V (VNat  m) + V (VNat n)
          ⇒ V (VNat (m  +ℕ       n))
  ⇒-Add-l : {m m' : Prog} → m ⇒ m' → (n : Prog)
          → m  + n
          ⇒ m' + n
  ⇒-Add-r : {n n' : Prog} → (m : Prog) → n ⇒ n'
          → m + n
          ⇒ m + n'
  ⇒-Lt    : (m n : ℕ)
          → V (VNat   m) < V (VNat n)
          ⇒ V (VBool (m <ℕ        n))
  ⇒-Lt-l  : {m m' : Prog} → m ⇒ m' → (n : Prog)
          → m  < n
          ⇒ m' < n
  ⇒-Lt-r  : {n n' : Prog} → (m : Prog) → n ⇒ n'
          → m < n
          ⇒ m < n'
  ⇒-If    : {c c' : Prog} → c ⇒ c' → (t f : Prog)
          → if c  then t else f
          ⇒ if c' then t else f
  ⇒-If-t  : (t f : Prog)
          → if V (VBool true ) then t else f ⇒ t
  ⇒-If-f  : (t f : Prog)
          → if V (VBool false) then t else f ⇒ f

_ : (V (VNat 3) + V (VNat 4)) ⇒ V (VNat 7)
_ = ⇒-Add 3 4

------------------------------------------------------------------------------
-- types

data Type : Set where
  TNat TBool : Type

data TypeError : Set where
  typeError : Type -> Type -> TypeError

mutual
  -- infer type of program
  infer : Prog -> TypeError ⊎ Type
  infer = λ where
    (V (VBool _)) -> inj₂ TBool
    (V (VNat  _)) -> inj₂ TNat
    (p1 + p2) ->
      case check p1 TNat of λ where
        (inj₁ e) -> inj₁ e
        (inj₂ _) -> case check p2 TNat of λ where
                      (inj₁ e) -> inj₁ e
                      (inj₂ _) -> inj₂ TNat
    (p1 < p2) ->
      case check p1 TNat of λ where
        (inj₁ e) -> inj₁ e
        (inj₂ _) -> case check p2 TNat of λ where
                      (inj₁ e) -> inj₁ e
                      (inj₂ _) -> inj₂ TBool
    (if p then p1 else p2) ->
      case check p TBool of λ where
        (inj₁ e) -> inj₁ e
        (inj₂ _) ->
          case infer p1 of λ where
            (inj₁ e) -> inj₁ e
            (inj₂ t) ->
              case check p2 t of λ where
                (inj₁ e) -> inj₁ e
                (inj₂ _) -> inj₂ t

  check : Prog -> Type -> TypeError ⊎ Type
  check p t =
    case infer p of λ where
      (inj₁ e)  -> inj₁ e
      (inj₂ t') -> d t t'
   where
    d : Type -> Type -> TypeError ⊎ Type
    d TBool b@TBool = inj₂ b
    d TNat  n@TNat  = inj₂ n
    d TNat  TBool   = inj₁ (typeError TNat TBool)
    d TBool TNat    = inj₁ (typeError TBool TNat)

_ : infer (if (V (VNat 1)) then (V (VNat 2)) else (V (VNat 3))) ≡ inj₁ (typeError TBool TNat)
_ = refl
_ : infer (if (V (VBool true)) then (V (VNat 2)) else (V (VNat 3))) ≡ inj₂ TNat
_ = refl
_ : infer (if (V (VBool true)) then (V (VNat 2)) else (V (VBool false))) ≡ inj₁ (typeError TNat TBool)
_ = refl

-- typing relation
data ⊢_∷_ : Prog → Type → Set where
  ⊢-Nat  : (n : ℕ)
         → ⊢ V (VNat n)  ∷ TNat
  ⊢-Bool : (b : Bool)
         → ⊢ V (VBool b) ∷ TBool
  ⊢-Add  : {m n : Prog}
         → ⊢ m ∷ TNat → ⊢ n ∷ TNat
         → ⊢ m        +   n ∷ TNat
  ⊢-Lt   : {m n : Prog}
         → ⊢ m ∷ TNat → ⊢ n ∷ TNat
         → ⊢ m        <   n ∷ TBool
  ⊢-If   : {c t f : Prog} {A : Type}
         → ⊢ c ∷ TBool → ⊢ t ∷ A → ⊢ f ∷ A
         → ⊢ if c then t else f ∷ A

_ : ⊢ if V (VNat 3) < V (VNat 4) then V (VNat 3) else V (VNat 4) ∷ TNat
_ = ⊢-If (⊢-Lt (⊢-Nat 3) (⊢-Nat 4))
         (⊢-Nat 3)
         (⊢-Nat 4)

{-
Theorem 1.4.3.1 (Uniqueness of types)
Given a program p, if p is both of types A and A0 then A ≡ A0

In ⊢_∷_, the dependent pattern matching algorithm
- knows,
  - given the constructor of a program,
  - the possible types this program can have
- conversely
  - given a type
  - the possible program constructors which will give rise to this type)
therefore showing type uniqueness is almost immediate:

proof : by induction on p
- depending on the form of p, at most one rule applies.
- if p is of the form if p0 then p1 else p2, the only rule which allows typing p is

  ⊢ p0 : bool      ⊢ p1 : A       ⊢ p2 : A
  ----------------------------------------
       ⊢ if p0 then p1 else p2 : A

Since p1 and p2 admit at most one type A by induction hypothesis, p also does.
Other cases are similar.
-}

tuniq : {p : Prog} {A A' : Type}
      → ⊢ p ∷ A → ⊢ p ∷ A'
      → A ≡ A'
tuniq (⊢-Nat     n)    (⊢-Nat    .n)    = refl
tuniq (⊢-Bool    b)    (⊢-Bool   .b)    = refl
tuniq (⊢-Add  _  _)    (⊢-Add  _  _)    = refl
tuniq (⊢-Lt   _  _)    (⊢-Lt   _  _)    = refl
tuniq (⊢-If   _ lt lf) (⊢-If   _ rt rf) = tuniq lt rt -- 'tuniq lf rf' works too

{-
------------------------------------------------------------------------------
-- Safety Properties

-- SUBJECT REDUCTION

Theorem 1.4.3.2 (Subject reduction). REDUCTION PRESERVES TYPING:

Given programs p and p' such that p −→ p', if p has type A then p' also has type A.

Proof. By hypothesis, we have both a derivation of p −→ p' and ⊢ p : A.
We reason by induction on the former.
Suppose the last rule is

       p1 −→ p'1
       ---------
    p1 + p2 −→ p'1 + p2

The derivation of ⊢ p : A necessarily ends with

    ⊢ p1 : int    ⊢ p2 : int
  --------------------------
      ⊢ p1 + p2 : int

In particular, we have ⊢ p1 : int and thus,
by induction hypothesis, ⊢ p'1 : int is derivable.
We conclude using the derivation

         ⊢ p'1 : int     ⊢ p2 : int
        ---------------------------
             ⊢ p01 + p2 : int

Other cases are similar.
-}

sred : {p p' : Prog} {A : Type}
     → (p ⇒ p') → ⊢ p ∷ A
     → ⊢ p' ∷ A
sred (⇒-Add   m n) (⊢-Add _  _)     = ⊢-Nat (m +ℕ n)
sred (⇒-Add-l m _) (⊢-Add m' n')    = ⊢-Add (sred m m') n'
sred (⇒-Add-r _ n) (⊢-Add m' n')    = ⊢-Add m' (sred n n')
sred (⇒-Lt    m n) (⊢-Lt  _  _)     = ⊢-Bool (m <ℕ n)
sred (⇒-Lt-l  m _) (⊢-Lt  m' n')    = ⊢-Lt (sred m m') n'
sred (⇒-Lt-r  _ n) (⊢-Lt  m' n')    = ⊢-Lt m' (sred n n')
sred (⇒-If c _ _)  (⊢-If  c' t' f') = ⊢-If (sred c c') t' f'
sred (⇒-If-t _ _)  (⊢-If  _  t'  _) = t'
sred (⇒-If-f _ _)  (⊢-If  _ _   f') = f'

{-
-- PROGRESS

Theorem 1.4.3.3 (Progress). A PROGRAM IS EITHER A VALUE OR REDUCES.

Given a program p of type A, either p is a value or
there exists a program p' such that p −→ p'.

Proof. By induction on the derivation of ⊢ p : A.
Suppose that the last rule is

            ⊢ p1 : int    ⊢ p2 : int
            -----------------------
                ⊢ p1 + p2 : int

By induction hypothesis, the following cases can happen:
– p1 −→ p'1 : in this case, we have p1 + p2 −→ p'1 + p2 ,
– p2 −→ p'2 : in this case, we have p1 + p2 −→ p1 + p'2 ,
– p1 and p2 are values: in this case, they are necessarily integers
  and p1 + p2 reduces to their sum.

Other cases are similar.
-}

-- PROGRESS : a typable program is either a value or reduces to some other program.
-- Given a program p which admits a type A, the proof is performed on the derivation of ⊢ p : A

prgs : {p : Prog} {A : Type}
     → ⊢ p ∷ A
     → Σ Value (λ v → p ≡ V v) ⊎ Σ Prog (λ p' → p ⇒ p')
prgs (⊢-Nat  n)   = inj₁ ((VNat  n) , refl)
prgs (⊢-Bool b)   = inj₁ ((VBool b) , refl)

prgs (⊢-Add        tm _) with prgs tm
prgs (⊢-Add        _ tn) | inj₁ (v , e) with prgs tn
prgs (⊢-Add        _  _) | inj₁ (VNat m  , refl) | inj₁ (VNat  n , refl) =
  inj₂ (V (VNat (m +ℕ n)) , ⇒-Add m n )
prgs (⊢-Add        _ ()) | inj₁ (VNat m  , refl) | inj₁ (VBool _ , refl)
prgs (⊢-Add        () _) | inj₁ (VBool _ , refl) | inj₁ (_       , refl)
prgs (⊢-Add {pm}{_} _ _) | inj₁ (_       ,    _) | inj₂ (pn'     , n⇒pn') =
  inj₂ (pm  + pn'         , ⇒-Add-r pm n⇒pn')
prgs (⊢-Add {_}{pn} _ _) | inj₂ (pm' , m⇒pm') = -- XXXX
  inj₂ (pm' + pn          , ⇒-Add-l m⇒pm' pn)

prgs (⊢-Lt         tm _) with prgs tm
prgs (⊢-Lt         _ tn) | inj₁ (v , e) with prgs tn
prgs (⊢-Lt         _  _) | inj₁ (VNat m  , refl) | inj₁ (VNat  n , refl) =
  inj₂ (V (VBool (m <ℕ n)) , ⇒-Lt m n )
prgs (⊢-Lt         _ ()) | inj₁ (VNat m  , refl) | inj₁ (VBool _ , refl)
prgs (⊢-Lt         () _) | inj₁ (VBool _ , refl) | inj₁ (_       , refl)
prgs (⊢-Lt {pm}{_}  _ _) | inj₁ (_       ,    _) | inj₂ (pn'     , n⇒pn') =
  inj₂ (pm  < pn'          , ⇒-Lt-r pm n⇒pn')
prgs (⊢-Lt {_} {pn} _ _) | inj₂ (pm' , m⇒pm') = -- XXXX
  inj₂ (pm' < pn           , ⇒-Lt-l m⇒pm' pn)

prgs (⊢-If             b _ _) with prgs b
prgs (⊢-If            () _ _) | inj₁ (VNat  x     , refl)
prgs (⊢-If {_} {t} {f} _ _ _) | inj₁ (VBool true  , refl) = inj₂ (t , ⇒-If-t t f)
prgs (⊢-If {_} {t} {f} _ _ _) | inj₁ (VBool false , refl) = inj₂ (f , ⇒-If-f t f)
prgs (⊢-If {c} {t} {f} _ _ _) | inj₂ (c'          , c⇒c') = inj₂ ( if c' then t else f
                                                                 , ⇒-If c⇒c' t f)

{-
-- TODO how does XXXX above obviate YYYY below?

prg' : {p : Prog} {A : Type}
     → ⊢ p ∷ A
     → Σ Value (λ v → p ≡ V v) ⊎ Σ Prog (λ p' → p ⇒ p')
prg' (⊢-Nat  n)   = inj₁ ((VNat  n) , refl)
prg' (⊢-Bool b)   = inj₁ ((VBool b) , refl)
-- prg' (⊢-Add (⊢-Nat m) (⊢-Nat n)) = inj₂ (V (VNat (m +ℕ n)) , ⇒-Add m n)
prg' (⊢-Add l r)
  with prg' l | prg' r
prg' (⊢-Add _ ()) | inj₁ (VNat  _ , refl) | inj₁ (VBool _ , refl)
prg' (⊢-Add () _) | inj₁ (VBool _ , refl) | inj₁ (VNat  _ , refl)
prg' (⊢-Add _  _) | inj₁ (VNat  m , refl) | inj₁ (VNat  n , refl) =
  inj₂ (V (VNat (m +ℕ n)) , ⇒-Add m n )
prg' (⊢-Add {p} {_} _ _) | inj₁ (_ , _)   | inj₂ (q' , r) =
  inj₂ (p  + q'           , ⇒-Add-r p r)
prg' (⊢-Add {_} {q} _ _) | inj₂ (p' , r)  | inj₁ (_ , _) =
  inj₂ (p' + q            , ⇒-Add-l r q)
prg' (⊢-Add {p} {q} _ _) | inj₂ (lp , p⇒lp) | inj₂ (rp , q⇒rp) = -- YYYY
  inj₂ (lp + rp , {!!})
prg' (⊢-Lt x x₁) = {!!}
prg' (⊢-If x x₁ x₂) = {!!}
-}
{-

SAFETY

Theorem 1.4.3.4 (Safety). TYPABLE PROGRAMS NEVER ENCOUNTER ERRORS (e.g., never stuck)

A program p of type A is safe: either
– p reduces to a value v in finitely many steps           : p −→ p1 −→ p2 −→ · · · −→ pn −→ v
– or p loops: there is an infinite sequence of reductions : p −→ p1 −→ p2 −→ · · ·

Proof.
Consider a maximal sequence of reductions from p.

If this sequence is finite, by maximality, its last element p' is an irreducible program.

Since p is of type A and reduces to p',
by the subject reduction theorem 1.4.3.2, p' also has type A.

We can thus apply the progress theorem 1.4.3.3
and deduce that either p0 is a value or there exists p'' such that p' −→ p'' .

The second case is impossible
since it would contradict the maximality of the sequence of reductions.

---------
TODO
Exercise 7.1.0.1. Formalize type inference and show that
– it is correct: if a type is inferred for a program then the program actually admits this type
– it is complete: if a program is typable then type inference will return a type.
-}

-- correct : (t : Type)
--         → (p : Prog)
--         → infer p ≡ inj₂ t
--         → ⊢ p ∷ t

cc : {p : Prog} {A A' : Type}
  -> ⊢ p ∷ A
  -> infer p ≡ inj₂ A'
  -> A ≡ A'
cc {.(V (VNat  n))}              {.TNat}          (⊢-Nat  n) refl                  = refl
cc {.(V (VBool b))}              {.TBool}         (⊢-Bool b) refl                  = refl

cc {.(V (VNat _) + _)}           {.TNat}  {TNat}  (⊢-Add (⊢-Nat _) _) _            = refl
cc {.(V (VNat m) + _)}           {.TNat}  {TBool} (⊢-Add (⊢-Nat m) pA₁) ip         = {!!}
cc {.((_ + _) + _)}              {.TNat}  {TNat}  (⊢-Add (⊢-Add _ _) _) _          = refl
cc {.((_ + _) + _)}              {.TNat}  {TBool} (⊢-Add (⊢-Add pA pA₂) pA₁) ip    = {!!}
cc {.((if _ then _ else _) + _)} {.TNat}  {TNat}  (⊢-Add (⊢-If _ _ _) _) _         = refl
cc {.((if _ then _ else _) + _)} {.TNat}  {TBool} (⊢-Add (⊢-If pA pA₂ pA₃) pA₁) ip = {!!}

cc {.(V (VNat _) < _)}           {.TBool} {TBool} (⊢-Lt  (⊢-Nat _) _) _            = refl
cc {.(V (VNat n) < _)}           {.TBool} {TNat}  (⊢-Lt  (⊢-Nat n) pA₁) ip         = {!!}
cc {.(_ + _ < _)}                {.TBool} {TBool} (⊢-Lt  (⊢-Add _ _) _) _          = refl
cc {.(_ + _ < _)}                {.TBool} {TNat}  (⊢-Lt  (⊢-Add pA pA₂) pA₁) ip    = {!!}
cc {.((if _ then _ else _) < _)} {.TBool} {TBool} (⊢-Lt  (⊢-If _ _ _) _) _         = refl
cc {.((if _ then _ else _) < _)} {.TBool} {TNat}  (⊢-Lt  (⊢-If pA pA₂ pA₃) pA₁) ip = {!!}

cc {.(if _ then _ else _)}       {TBool}  {TBool} (⊢-If _ _ _ ) _                  = refl
cc {.(if _ then _ else _)}       {TBool}  {TNat}  (⊢-If pA pA1 pA2) ip             = {!!}
cc {.(if _ then _ else _)}       {TNat}   {TNat}  (⊢-If _ _ _) _                   = refl
cc {.(if _ then _ else _)}       {TNat}   {TBool} (⊢-If pA pA1 pA2) ip             = {!!}
