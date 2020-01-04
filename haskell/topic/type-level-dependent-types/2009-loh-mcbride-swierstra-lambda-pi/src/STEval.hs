module STEval where

------------------------------------------------------------------------------
import           STTypes
------------------------------------------------------------------------------
import           Control.Monad.Except
import           Data.List
import           Data.Maybe
import           Prelude                   hiding (print)
------------------------------------------------------------------------------

type NameEnv v     = [(Name, v)]
type Ctx       inf = [(Name, inf)]
type State   v inf = (Bool, String, NameEnv v, Ctx inf)

------------------------------------------------------------------------------
-- p 3 : (STLC) Evaluation : big-step eval rules

{-
- e ⇓ v means result of completely evaluating e is v
- evaluate everything as far as possible, including under lambda

examples:

(id :: α → α) y ⇓ y
(const :: (β → β) → α → β → β) id y ⇓ id
-}
{-
Substitution handled via env of values.
Bound vars represented as integers.
Env's i-th position corresponds to value of var i.
Add new element to Env whenever evaluating under a binder.
Lookup element when encountering a bound var.
-}
type Env     = [Value]

{-
inferable eval (eval ↑)
-}
iEval :: ITerm -> (NameEnv Value, Env) -> Value
{-
ignore annotation

e ⇓ v
----------
e :: τ ⇓ v
-}
iEval (Ann  e _)  d = cEval e d
{-
variables eval to themselves

------
x ⇓ x
-}
iEval (Free   x)  d = fromMaybe (vfree x) (lookup x (fst d))
iEval (Bound ii)  d = snd d !! ii
{-
application
-}
iEval (e1 :@: e2) d = vapp (iEval e1 d) (cEval e2 d)

vapp :: Value -> Value -> Value
{-
application

case left subterm evaluates to lambda abstraction
β-reduce
may produce new redexes, so evaluate result

e ⇓ λx → v    v[x ↦ e'] ⇓ v'
----------------------------
e e' ⇓ v'
-}
vapp (VLam f) v      = f v -- Beta-reduce
{-
application

case left subterm evaluates to neutral term
eval cannot proceed further
construct new neutral term from results of evaluating the two subterms
e ⇓ n    e' ⇓ v'
----------------
e e' ⇓ n v'

-- TODO : this looks like eval under lambda
e ⇓ v
---------------
λx → e ⇓ λx → v
-}
vapp (VNeutral n)  v = VNeutral (NApp n v)

{-
checkable eval (eval ↓)
-}
cEval :: CTerm -> (NameEnv Value, Env) -> Value
cEval (Inf  ii) d  = iEval ii d
-- create a Haskell fun and add the bound var x to Env while evaluating the body
cEval (Lam  e0) d0 = VLam (\x -> cEval e0 ((\(e, d) -> (e, x : d)) d0))

------------------------------------------------------------------------------
-- p4 : (STLC) Type System

{-
Γ(z) denotes information associated with identifier z by context Γ

definitions of contexts and their validity (i.e., "well-formed"):

               valid (Γ)               valid (Γ)   Γ ⊢ τ :: ∗
---------      -----------------       ----------------------
valid (ε)      valid (Γ, α :: ∗)       valid (Γ,  x :: τ )

- free vars in terms and types must occur in the context, e.g.,
  - for const to be of type (β → β) → α → β → β
  - context must contain at least: α :: ∗, β :: ∗, const :: (β → β) → α → β → β
  - α and β must be introduced before using them in the type of const
- multiple bindings for same var can occur, rightmost binding takes precedence
- the well-formedness and type rules assume all contexts are valid
-}
{-
check well-formedness of types (kind ↓)

TVAR, FUN explain when a type is well-formed
- i.e., when all its free vars appear in the context
-}
cKind :: Context -> Type -> Kind -> Either String ()
{-
  Γ(α) = ∗
---------- (TVAR)
Γ ⊢ α :: ∗
-}
cKind g (TFree x) Star = case lookup x g of
  Just (HasKind Star) -> pure ()
  Just z              -> Left ("cKind: not covered: " ++ show x ++ " : " ++ show z)
  Nothing             -> Left "cKind: unknown identifier"
{-
Γ ⊢ τ :: ∗   Γ ⊢ τ' :: ∗
------------------------- (FUN)
Γ ⊢ τ      →     τ' :: ∗
-}
cKind g (Fun k k') Star = do
  cKind g k  Star
  cKind g k' Star
{-
--------------------------------------------------
type rules

:: ↓ -- type is an input to type checking algorithm
:: ↑ -- type is produced by type checking algorithm

Γ ⊢ τ :: ∗   Γ ⊢ e :: ↓ τ
------------------------- (ANN)
Γ ⊢ (e :: τ ) :: ↑ τ

Γ(x) = τ
------------ (var)
Γ ⊢ x :: ↑ τ

Γ ⊢ e :: ↑ τ → τ'     Γ ⊢ e' :: ↓ τ
----------------------------------- (APP)
Γ ⊢ e e' :: ↑ τ'

Γ ⊢ e :: ↑ τ
------------ (CHK)
Γ ⊢ e :: ↓ τ

Γ, x :: τ ⊢ e :: ↓ τ
---------------------- (LAM)
Γ ⊢ λx → e :: ↓ τ → τ'

Figure 3 Type rules for λ →

- no type inference of types of lambda-bound vars
- only type checking
- for annotated terms, vars and applications the types can be determined

- inferable terms
  - ANN : check annotated terms against their type annotation, and then return the type
  - VAR : look up type in environment
  - APP : 1st postion must be of a function type;
          check arg against fun’s domain;
          return fun range as the result type

type-checking functions take an Int indicating the number of encountered.
On the initial call, it is 0.
-}
{-
infer term type (type ↑)
-}
iType0 :: Context -> ITerm -> Either String Type
iType0 = iType 0

iType :: Int -> Context -> ITerm -> Either String Type
iType ii g (Ann e ty) = do
  cKind g ty Star
  cType ii g e ty
  pure ty
iType  _ g (Free x) = case lookup x g of
  Just (HasType ty) -> pure ty
  Just z            -> Left ("iType: not covered: " ++ show z)
  Nothing           -> Left "unknown identifier"
iType ii g (e1 :@: e2) = do
  si <- iType ii g e1
  case si of
    Fun ty ty' -> do cType ii g e2 ty; pure ty'
    _          -> Left "illegal application"
iType _ _ x = Left ("iType: not covered: " ++ show x)

{-
check term type (type ↓)
-}
cType :: Int -> Context -> CTerm -> Type -> Either String ()
{-
The type eq check performed for an inferable term is syntactic equality on 'Type'.
The type checker does not perform unification.
-}
cType ii g (Inf e) ty           = do
  ty' <- iType ii g e
  unless (ty == ty') (Left "type mismatch")
{-
type rule for lambda abstraction
- add the bound var to the context while checking the body
- ii indicates the number of binders
- Local i is a fresh name associated with the bound var
- add Local i to the context Γ (i.e., 'g') when checking the body
- because a bound var is turned into a free var
  - perform corresponding substitution on the body
- the type checker will never encounter a bound var;
  - so the function type ↑ has no case for Bound.
-}
cType ii g (Lam e) (Fun ty ty') =
  cType  (ii + 1) ((Local ii, HasType ty) : g) (cSubst 0 (Free (Local ii)) e) ty'
cType  _ _ _ _                  = Left "type mismatch"
{-
substitution

Int arg indicates which var is to be substituted
-}
{-
inferable substitution (subst ↑)
-}
iSubst :: Int -> ITerm -> ITerm -> ITerm
iSubst ii r (Ann e ty)  = Ann (cSubst ii r e) ty
{-
Bound : check if the var encountered is the one to be substituted or not
-}
iSubst ii r (Bound j)   = if ii == j then r else Bound j
iSubst  _ _ (Free y)    = Free y
iSubst ii r (e1 :@: e2) = iSubst ii r e1 :@: cSubst ii r e2
{-
checkable substitution (subst ↓)
-}
cSubst :: Int -> ITerm -> CTerm -> CTerm
cSubst ii r (Inf e) = Inf (iSubst ii r e)
{-
Lam : increase i to indicate that var to substitute is underneath another binder
-}
cSubst ii r (Lam e) = Lam (cSubst (ii + 1) r e)

--------------------------------------------------
{-
quote takes Int indicating num binders traversed, initially 0
-}
quote0 :: Value -> CTerm
quote0 = quote 0

quote :: Int -> Value -> CTerm
{-
lambda abstraction
- generate fresh variable Quote i
- apply Haskell function f to the fresh variable
- resulting value is quoted at level i + 1
- Quote takes an Int arg to ensure newly created names do not clash
-}
quote ii (VLam f)     = Lam (quote (ii + 1) (f (vfree (Quote ii))))
{-
neutral term (application of free var to other values)
-}
quote ii (VNeutral n) = Inf (neutralQuote ii n)
{-
neutralQuote : to quote args
-}
neutralQuote :: Int -> Neutral -> ITerm
neutralQuote ii (NFree x)  = boundfree ii x
neutralQuote ii (NApp n v) = neutralQuote ii n :@: quote ii v
{-
boundfree
- checks if var at head of application is a Quote, therefore a bound var, or a free name
-}
boundfree :: Int -> Name -> ITerm
boundfree ii (Quote k) = Bound (ii - k - 1)
boundfree  _ x         = Free x

