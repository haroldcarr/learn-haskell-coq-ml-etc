{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Fischoff.Thump2 where

import Language.LBNF.Compiletime
import Language.LBNF (lbnf, bnfc)

-- built in support parentheses:

bnfc [lbnf|
Ap.  Expr ::= Expr Expr;
Lam. Expr ::= "\\" Ident "->" Expr;
Var. Expr ::= Ident;
_.   Expr ::= "(" Expr ")" ; -- This is what was added
|]
{-
Now it works:

> :set -XQuasiQuotes
> [Fischoff.Thump2.expr| (\x -> x) y |]
Ap (Lam (Ident "x") (Var (Ident "x"))) (Var (Ident "y"))

warning when compiling:

Warning: 8shift/reduce conflicts

because grammar is ambiguous

See Thump3 for fix.
-}
