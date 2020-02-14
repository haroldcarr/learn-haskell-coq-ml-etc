{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Fischoff.Thump where

import Language.LBNF.Compiletime
import Language.LBNF (lbnf, bnfc)

{-
https://medium.com/@jonathangfischoff/prototyping-with-bnfc-meta-82861dbb990b
Prototyping With BNFC-meta
Jonathan Fischoff
Jun 21, 2017
-}

bnfc [lbnf|
Ap.  Expr ::= Expr Expr;
Lam. Expr ::= "\\" Ident "->" Expr;
Var. Expr ::= Ident;
|]
{-
bnfc is Template Haskell function and it creates the types:

data Expr = Ap Expr Expr
          | Lam Ident Expr
          | Var Ident -- Ident is a built in that is a newtype around String.

and also a parser 'pExpr' and lexer 'myLexer':

> Fischoff.Thump.pExpr $ Fischoff.Thump.myLexer "\\x -> x"
Ok (Lam (Ident "x") (Var (Ident "x")))

For debugging, easier to use
> :set -XQuasiQuotes
> [Fischoff.Thump.expr| \x -> x |]
Lam (Ident "x") (Var (Ident "x"))

pretty printer:

> printTree [Fischoff.Thump.expr| \x -> x |]
"\\ x -> x"

try a more complicated expression:

> [Fischoff.Thump.expr| (\x -> x) y |]
<interactive>:9:7: error:
  • syntax error at line 1 due to lexer error
  • In the quasi-quotation: [expr| (\x -> x) y |]

We need handle parentheses.

See Thump2.hs
-}
