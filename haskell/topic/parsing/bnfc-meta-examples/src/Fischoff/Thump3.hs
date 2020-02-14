{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Fischoff.Thump3 where

import Language.LBNF.Compiletime
import Language.LBNF (lbnf, bnfc)

-- fix ambiguous grammar by making the precedence explicit and providing “coercions”

bnfc [lbnf|
Lam. Expr  ::= "\\" Ident "->" Expr1;
Ap.  Expr1 ::= Expr1 Expr2;
Var. Expr2 ::= Ident;
coercions Expr 2 ; -- Shortcut for _.  Expr ::= "(" Expr ")" ;
|]
{-
> :set -XQuasiQuotes
> [Fischoff.Thump3.expr| (\x -> x) y |]

Can add antiquotes to the quasiquoter.
Quasiquoter includes a pattern quasiquoter.

Downsides : difficult to customize the lexer
-}
