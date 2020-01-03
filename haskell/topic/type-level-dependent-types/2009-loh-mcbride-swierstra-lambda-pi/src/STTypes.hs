module STTypes where

data Stmt i tinf
  = Let      String i        --  let x = t
  | Assume   [(String,tinf)] --  assume x :: t, assume x :: *
  | Eval     i
  | PutStrLn String          --  lhs2TeX hacking : enable printing "magic" string
  | Out      String          --  lhs2TeX hacking : enable print to files
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- p2 : Simply Typed Lambda Calculus (aka λ→)

{-
- terms explicitly typed
- no type inference
- only base types and functions
- functions are not polymorphic
- strongly normalizing: evaluation terminates for any term, independent of the evaluation strategy
-}

------------------------------------------------------------------------------
-- p3 : (STLC) Abstract syntax

{-
types

τ ::= α       base type
    | τ → τ'  function type
-}
data Type
     = TFree Name       -- type identifier
     | Fun   Type Type  -- function arrows (not polymorphic)
    deriving (Eq, Show)
{-
terms

e ::= e :: τ  annotated term
    | x       variable
    | e e'    application
    | λx → e  lambda abstraction

(Note: using numbers for local, and names for global is called a "locally nameless" representation)
-}
data ITerm -- Inferable (Term ↑)
     = Ann   CTerm Type -- explicit Annotation
     | Bound Int        -- variable : de Bruijn indice (i.e., no need to alpha reduce)
                        -- num binders between its binder and occurrence
     | Free  Name       -- variable : e.g., top level
     | ITerm :@: CTerm  -- application
    deriving (Eq, Show)

data CTerm -- Checkable (Term ↓)
     = Inf ITerm -- inferable embedded in a CTerm
     | Lam CTerm -- lambda abstraction -- no explicit vars because using de Bruijn
    deriving (Eq, Show)

data Name
     = Global String
     | Local  Int    -- when passing a binder in an algorithm,
                     -- temporarily convert a bound var into a free var
     | Quote  Int
    deriving (Eq, Show)
{-
values

v ::= n       neutral term
    | λx → v  lambda abstraction
-}
data Value
     = VNeutral Neutral
     | VLam     (Value -> Value) -- lambda abstraction (HOAS : rep funs as Haskell funs)
{-
Since using HOAS, define a quote function to take a Value back to a term.
- VLam takes a function as argument
- therefore cannot derive Eq and Show
- use quote (below) to get back at internal structure of a value (e.g., to display)
-}
-- Eq and Show are ONLY for testing.
instance Eq Value where
  (==) (VLam     _) (VLam     _) = True -- True for testing
  (==) (VLam     _)           _  = False
  (==)           _  (VLam     _) = False
  (==) (VNeutral l) (VNeutral r) = l == r
instance Show Value where
  show (VLam _)     = "VLam <fun>"
  show (VNeutral n) = "(VNeutral " ++ show n ++ ")"
{-
neutral : a variable applied to a (possibly empty) sequence of values

n ::= x     variable
    | n v   application
-}
data Neutral
     = NFree Name          -- free var
     | NApp  Neutral Value -- application of neutral term to a value
     deriving (Eq, Show)

-- create the value corresponding to a free var
vfree :: Name -> Value
vfree n = VNeutral (NFree n)

------------------------------------------------------------------------------
-- p4 : (STLC) Type System

--------------------------------------------------
{-
context and well-formedness

Γ ⊢ e :: t -- means e is type t in context Γ

context
- lists valid base types
  - α :: ∗ - indicates α is a base type
- associates identifiers with type information
  - x :: t - indicates x is a term of type t

Γ ::= ε          empty context
    | Γ, α :: ∗  adding a type identifier (base types)
    | Γ, x :: τ  adding a term identifier (type info)
-}
data Kind = Star
  deriving (Eq, Show)

data Info
  = HasKind  Kind
  | HasType  Type
  deriving (Eq, Show)

type Context = [(Name, Info)]
