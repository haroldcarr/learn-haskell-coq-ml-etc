{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module X_1 where

import qualified Data.Map.Strict as Map
import           Data.Void

{-
Trees That Shrink
Vaibhav Sagar
https://vaibhavsagar.com/blog/2018/06/19/trees-that-shrink/

Problem solved by:
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/trees-that-grow.pdf
-}

-- | ADT of lambda calculus with de Bruijn indices
data Expr a
  = Lit a
  | Var Int
  | Abs (Expr a)
  | App (Expr a) (Expr a)
  deriving (Show)

-- add
-- - let bindings
-- - named variables (because de Bruijn indices hard to work with)

data Expr' a
  = Lit' a
  | Var' String
  | Abs' (Expr' a)
  | App' (Expr' a) (Expr' a)
  | Let' String (Expr' a) (Expr' a)
  deriving (Show)

{-
desugared let

    let <n> <x> <y> <=> (\n -> y) x

then simpler evaluator: no 'let' in evaluator

task: convert Expr' to intermediate rep where name and de Bruijn index coexist
options:
1 - define third data type
  - write indexing pass to convert Var String to Var (String, Int)
  - another pass that converts that to Expr a
2 - work in Expr'; do not index, throw errors when a Let is encountered after desugaring
3 - combine desugaring/indexing passes into one : THIS ONE BELOW

THE FOLLOWING IS BROKEN : FIXED : SEE "THIS WAS MISSING"
-}

desugarAndDeBruijn :: Map.Map String Int -> Expr' a -> Expr a
desugarAndDeBruijn env expr = case expr of
  Lit' a          -> Lit a
  Var' name       -> Var (env Map.! name)
  Abs' expr'      -> let env' = Map.map succ env in Abs (desugarAndDeBruijn env' expr')
  App' f x        -> App (desugarAndDeBruijn env f)    (desugarAndDeBruijn env x)
  Let' n v expr'  -> let env' = Map.insert n 0 env -- THIS WAS MISSING
                     in   desugarAndDeBruijn env' (App' (Abs' expr') v)

ex0' :: Expr' Int
ex0' = App' (Abs' (Var' "x")) (Lit' 3)
ex0 :: Expr Int
ex0 = desugarAndDeBruijn Map.empty ex0'

ex1' :: Expr' Int
ex1' = Let' "x" (Lit' 3) (Var' "x")
ex1 :: Expr Int
ex1 = desugarAndDeBruijn Map.empty ex1'

ex2' :: Expr' Int
ex2' = Let' "x" (Lit' 3)
        (Let' "f" (Abs' (Var' "x"))
         (App' (Var' "f") (Var' "x")))
ex2 :: Expr Int
ex2 = desugarAndDeBruijn Map.empty ex2'

ex3' :: Expr' Int
ex3' = Let' "x" (Lit' 3)
        (Let' "f" (Abs' (Var' "x"))
         (App' (Var' "f") (Var' "x")))
ex3 :: Expr Int
ex3 = desugarAndDeBruijn Map.empty ex3'

{-
better way : "Trees that Grow" by Shayan Najd and Simon Peyton Jones.

defining different data types for each use case is wrong
- all these types conceptually the same : just have different annotation/decoration
- define extensible base type instead

implement extensible data type with type families and pattern synonyms

can work with GADTs too

update
- add type parameter 'i'
- add field to constructors with a type that references 'i'
- add extra constructor (e.g, X*) to enable extending this type
- define type families for each of the new data types
-}

data ExpX i a
  = LitX (XLit i a) a
  | VarX (XVar i a)
  | AbsX (XAbs i a) (ExpX i a)
  | AppX (XApp i a) (ExpX i a) (ExpX i a)
  | ExpX (XExp i a)

type family XLit i a
type family XVar i a
type family XAbs i a
type family XApp i a
type family XExp i a

-- reconstruct original data type with no extensions using Data.Void

void :: Void
void = error "Attempt to evaluate void"

{-
ExpUD (UD for “undecorated”)
- Int for Var
- Void for all other extension points
- use pattern synonyms to make easier to work with
-}

data UD
type ExpUD a            = ExpX UD a
type instance XLit UD a = Void
type instance XVar UD a = Int
type instance XAbs UD a = Void
type instance XApp UD a = Void
type instance XExp UD a = Void

pattern LitUD :: a -> ExpUD a
pattern LitUD a   <- LitX _ a         where LitUD a = LitX void a
pattern VarUD :: Int -> ExpUD a
pattern VarUD i   <- VarX i           where VarUD i = VarX i
pattern AbsUD :: ExpUD a -> ExpUD a
pattern AbsUD a   <- AbsX _ a         where AbsUD a = AbsX void a
pattern AppUD :: ExpUD a -> ExpUD a -> ExpUD a
pattern AppUD f a <- AppX _ f a       where AppUD f a = AppX void f a

{-
define intermediate data type (e.g., like Expr')
- uses named variables and de Bruijn indices
- TODO: can now have named parameters to lambdas
  - not possible with initial approach
-}

data Ann
type ExpAnn a            = ExpX Ann a
type instance XLit Ann a = Void
type instance XVar Ann a = (String, Int)
type instance XAbs Ann a = String
type instance XApp Ann a = Void
type instance XExp Ann a = Void

pattern LitAnn :: a -> ExpAnn a
pattern LitAnn a   <- LitX _ a        where LitAnn a = LitX void a
pattern VarAnn :: String -> Int -> ExpAnn a
pattern VarAnn s i <- VarX (s,i)      where VarAnn s i = VarX (s, i)
pattern AbsAnn :: String -> ExpAnn a -> ExpAnn a
pattern AbsAnn s a <- AbsX s a        where AbsAnn s a = AbsX s a
pattern AppAnn :: ExpAnn a -> ExpAnn a -> ExpAnn a
pattern AppAnn f a <- AppX _ f a      where AppAnn f a = AppX void f a

{-
add let bindings
- represent let <name> = <expr> in <expr> as (<name>, <expr>, <expr>)
- use named variables and parameters in this representation
-}

data Let
type ExpLet a            = ExpX Let a
type instance XLit Let a = Void
type instance XVar Let a = String
type instance XAbs Let a = String
type instance XApp Let a = Void
type instance XExp Let a = (String, ExpLet a, ExpLet a)

pattern LitLet :: a -> ExpLet a
pattern LitLet a     <- LitX _ a      where LitLet a = LitX void a
pattern VarLet :: String -> ExpLet a
pattern VarLet s     <- VarX s        where VarLet s = VarX s
pattern AbsLet :: String -> ExpLet a -> ExpLet a
pattern AbsLet s a   <- AbsX s a      where AbsLet s a = AbsX s a
pattern AppLet :: ExpLet a -> ExpLet a -> ExpLet a
pattern AppLet f a   <- AppX _ f a    where AppLet f a = AppX void f a
pattern LetLet :: forall i a a1 b c
                . XExp i a ~ (a1, b, c)
               => a1 -> b -> c -> ExpX i a
pattern LetLet n v e <- ExpX (n,v,e)

-- instead of writing single pass, can now write smaller ones

-- see X_3 for functions (e.g., eval) that operate on this data

