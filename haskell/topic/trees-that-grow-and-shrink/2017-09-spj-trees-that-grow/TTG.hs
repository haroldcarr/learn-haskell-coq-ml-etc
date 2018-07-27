{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TTG where

import           Test.HUnit            (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util       as U (t)

------------------------------------------------------------------------------
{-
Trees That Grow
Shayan Najd, Simon Peyton Jones
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf

use type-level functions to enable (G)ADT extensibility
-}

------------------------------------------------------------------------------
type Var = String
data Typ = Int | Fun Typ Typ deriving Eq

-- p 5. not extensible
data Exp
  = Lit Integer
  | Var Var
  | Ann Exp Typ -- type Ann(otation)
  | Abs Var Exp
  | App Exp Exp

------------------------------------------------------------------------------
{- p 6
EXTENSIBLE

ξ is a type index to ExpX.
ξ is known as "extension descriptor"
- describes which extension is in use

Each constructor has "extra" field
- field enables extending a constructor with extra fields
- type of field determined by type-level function

"extra" 'ExpX' constructor
- has one field of type XExp ξ
- used to extend data type with new constructors
-}
data ExpX ξ
--      place for new fields
--          v
  = LitX (XLit ξ) Integer
  | VarX (XVar ξ) Var
  | AnnX (XAnn ξ) (ExpX ξ) Typ
  | AbsX (XAbs ξ) Var      (ExpX ξ)
  | AppX (XApp ξ) (ExpX ξ) (ExpX ξ)
  | ExpX (XExp ξ)                     -- place for new constructors

-- for specifying type
-- - of new fields
-- - of new constructors
type family XLit ξ
type family XVar ξ
type family XAnn ξ
type family XAbs ξ
type family XApp ξ
type family XExp ξ

------------------------------------------------------------------------------
{-
ExpUD : undecorated (UD) variant of ExpX

does not introduce any forms of extensions, so type-level mappings set to Void

the type instance declarations can be omitted
- without instances, 'XAnn UD' is irreducible: therefore an empty type just like Void
- but possible to accidentally add 'XAnn UD', so prevent by giving explicit instance
-}

data Void -- no inhabitants
void :: Void
void = error "Attempt to evaluate void"
absurd :: Void -> a
absurd m = case m of { }

data UD -- no inhabitants
type ExpUD            = ExpX UD
type instance XLit UD = Void
type instance XVar UD = Void
type instance XAnn UD = Void
type instance XAbs UD = Void
type instance XApp UD = Void
type instance XExp UD = Void

------------------------------------------------------------------------------
{- p 7
Pattern Synonyms for Convenience

program with ExpX
– pattern matching : must ignore         extra field
– constructing     : must supply void in extra field
-}

incLit  :: Exp -> Exp
incLit  (Lit i ) = Lit       (i + 1)
incLit  e        = e
incLitX :: ExpUD -> ExpUD
incLitX (LitX _ i) = LitX void (i + 1)
incLitX e        = e

-- use pattern synonyms

pattern LitUD :: Integer -> ExpUD
-- bidirectional
--      | for matching    | | for constructing        |
pattern LitUD i <- LitX _ i where LitUD i = LitX void i

incLitX' :: ExpUD -> ExpUD
incLitX' (LitUD i) = LitUD (i + 1)
incLitX' e =e

{-
note: term value ExpX void has no counterpart in Exp
- Haskell lacks an entirely uninhabited type
- therefore, hide ExpX constructor from client users
-}

------------------------------------------------------------------------------
{- p 8 NEW FIELD EXTENSION : e.g., hold TypeCheck (TC) info on App(lication)
-}

data TC
type ExpTC            = ExpX TC
type instance XLit TC = Void
type instance XVar TC = Void
type instance XAnn TC = Void
type instance XAbs TC = Void
type instance XApp TC = Typ -- enables extra field of this type
type instance XExp TC = Void

pattern LitTC :: Integer -> ExpTC
pattern LitTC i     <- LitX _ i where LitTC i = LitX void i
pattern VarTC :: Var -> ExpTC
pattern VarTC v     <- VarX _ v where VarTC v = VarX void v
pattern AnnTC :: ExpTC -> Typ -> ExpTC
pattern AnnTC e t   <- AnnX _ e t where AnnTC e t = AnnX void e t
pattern AbsTC :: Var-> ExpTC-> ExpTC
pattern AbsTC l r   <- AbsX _ l r where AbsTC l r = AbsX void l r
pattern AppTC :: Typ -> ExpTC -> ExpTC -> ExpTC
pattern AppTC a l m <- AppX a l m  where AppTC l m = AppX l m

{- p 9 FUNCTIONS ON EXTENDED DATA TYPES
e.g., type check
-}

check :: ExpTC -> [(Var,Typ)] -> Typ -> Bool
check (LitTC     _) _   Int       = True
check (VarTC     v) env c         = (== Just c) (lookup v env)
check (AnnTC   e t) env c         = t == c && check e env c
check (AbsTC   v e) env (Fun a b) = check e ((v,a):env) b
check (AppTC t f a) env c         = check f env (Fun t c) && check a env t
check _             _   _         = False -- GHC does not yet know when synonyms are exhaustive

ttc :: [Test]
ttc = U.t "ttc"
  (all
   (True==)
   [       check (LitTC   3)                                   []                  Int
   ,       check        (VarTC "x")                            [("x",Int)]         Int
   , not $ check        (VarTC "x")                            [("x",Fun Int Int)] Int
   , not $ check        (VarTC "x")                            []                  Int
   ,       check (AnnTC (VarTC "x") Int)                       [("x",Int)]         Int
   ,       check            (AbsTC "x" (VarTC "x"))            []                  (Fun Int Int)
   ,       check (AppTC Int (AbsTC "x" (VarTC "x")) (LitTC 3)) []                  Int
   ])
  True

------------------------------------------------------------------------------
{- p 8 NEW CONSTRUCTOR EXTENSION : e.g., hold TypeCheck (TC) info on App(lication)
e.g.,
- partial evaluation (PE) pass over trees: β-redices normalised away
- after reducing, PE stores value as node in tree
- stored in new contructor in non extensible version
  data Val = ...
  data Exp = ... | Val Val
- extensible version will define new constructor ValPE
-}

newtype Val           = Val ExpPE -- deriving Show -- not done just because need to do show for ExrPE

data PE
type ExpPE            = ExpX PE
type instance XLit PE = Void
type instance XVar PE = Void
type instance XAnn PE = Void
type instance XAbs PE = Void
type instance XApp PE = Void
type instance XExp PE = Val

-- represents new constructor introduced by extension
pattern ValPE :: Val -> ExpPE
pattern ValPE v = ExpX v

------------------------------------------------------------------------------
{- p 10 Generic Functions on Extensible Data Types

to define generic functions: use common structure of extensible data type

e.g., generic print

- print that ignores new field : works same for both ExpUD and ExpTC
- new constructor extensions
  - could ignore, or
  - pass funcetion to to handle new constructors (done below)
    - could have used type classes

ExpUD and ExpTC have no new constructors
- args passed to generic printE matches empty types
ExpPE
- pass print fun for new constructor 'Val'
-}

printT :: Typ -> String
printT Int            = "Int"
printT (Fun a b)      = "(" ++ printT a ++ ") -> " ++ printT b

printE :: (XExp ξ -> String) -> ExpX ξ -> String
printE _ (LitX _ i)   = show i
printE _ (VarX _ x)   = x
printE p (AnnX _ m a) = "(" ++ printE p m ++ ") :: (" ++ printT a ++ ")"
printE p (AbsX _ x n) = "λ" ++ x ++ "." ++ printE p n
printE p (AppX _ l m) = "(" ++ printE p l ++ ")(" ++ printE p m ++ ")"
printE p (ExpX ξ)     = p ξ

printEUD :: ExpUD -> String
printEUD = printE absurd
printETC :: ExpTC -> String
printETC = printE absurd
-- printEPE :: ExpPE -> String                             -- not done (see above about Show ExrPE)
-- printEPE = printE p where p v = "{{" ++ show v ++ "}}"

tpl,tpv,tpan,tpab,tpap::[Test]
tpl  = U.t "tpl"  (printE undefined (LitTC   3))                                   "3"
tpv  = U.t "tpv"  (printE undefined (VarTC "x"))                                   "x"
tpan = U.t "tpan" (printE undefined (AnnTC (VarTC "x") Int))                       "(x) :: (Int)"
tpab = U.t "tpab" (printE undefined (AbsTC "x" (VarTC "x")))                       "\955x.x"
tpap = U.t "tpap" (printE undefined (AppTC Int (AbsTC "x" (VarTC "x")) (LitTC 3))) "(\955x.x)(3)"

------------------------------------------------------------------------------
{- p 11 Type Classes for Extensible Data Types

To print field extensions, could pass fun args for all field extensions:

  printE :: (XLit ξ → String) → (XVar ξ → String) → (XAnn ξ → String)
          → (XAbs ξ → String) → (XApp ξ → String) → (XExp ξ → String)
          → ExpX ξ → String

Alternative via type classes.
-}

instance ( Show (XLit ξ), Show (XVar ξ), Show (XAnn ξ)
         , Show (XAbs ξ), Show (XApp ξ), Show (XExp ξ)) => Show (ExpX ξ) where
  show = undefined

{-
then no explicit args necessary.

Use ConstraintKinds can abstract over the constraint

type forall x . (φ :: ∗ -> Constraint) ξ
  = ( φ (XLit ξ), φ (XVar ξ), φ (XAnn ξ)
    , φ (XAbs ξ), φ (XApp ξ), φ (XExp ξ)
    )

then header of previous instance becomes

instance forall X Show ξ => Show (ExpX ξ) where
  show = ...

can use Haskell’s standalone deriving

deriving instance forall X . Show ξ => Show (ExpX ξ)
-}

------------------------------------------------------------------------------

test :: IO Counts
test  =
  runTestTT $ TestList $
  ttc ++
  tpl ++ tpv ++ tpan ++ tpab ++ tpap

