{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module HC.Def where

import           Language.LBNF

bnfc [lbnf|
Ap.  Expr    ::= Expr Expr;
VL . Expr    ::= "\\" Ident [Ident] "->" Expr;
Ref. Expr    ::= Ident;
[] . [Ident] ::= ;
(:). [Ident] ::= Ident [Ident];
     |]
{-
HC.Def.pExpr $ HC.Def.myLexer "\\x     -> x"
HC.Def.pExpr $ HC.Def.myLexer "\\x y   -> x"
HC.Def.pExpr $ HC.Def.myLexer "\\x y z -> x"
HC.Def.pExpr $ HC.Def.myLexer "\\x y z -> \\a b c -> x"
HC.Def.pExpr $ HC.Def.myLexer "\\x y z -> z y x"
HC.Def.pExpr $ HC.Def.myLexer "\\ -> x"

:set -XQuasiQuotes
import Language.LBNF.Compiletime
printTree [HC.Def.expr|\x y z -> z y x |]
-}



