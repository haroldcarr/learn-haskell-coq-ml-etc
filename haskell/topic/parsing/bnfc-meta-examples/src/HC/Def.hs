{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module HC.Def where

import           Data.List
import           Data.Maybe
import           Language.LBNF

bnfc [lbnf|
Ap.  Expr    ::= Expr Expr; -- TODO left to right associate
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
printTree        [HC.Def.expr|\x y z -> z y x |]
deBruijnIndex [] [HC.Def.expr|\x y z -> z y x |]
-}

data ITerm -- Inferable (Term ↑)
  = Bound Int
  | ITerm :@: CTerm  -- application
  deriving (Eq, Show)

data CTerm -- Checkable (Term ↓)
  = Inf ITerm
  | Lam CTerm
  deriving (Eq, Show)

deBruijnIndex :: [Ident] -> Expr -> CTerm
deBruijnIndex env = \case
  -- Ap  l r           -> (iterm l) :@: (deBruijnIndex env r)
  VL  i []     body -> Lam (deBruijnIndex (i:env)          body)
  VL  i (x:xs) body -> Lam (deBruijnIndex (i:env) (VL x xs body))
  Ref i             -> maybe (error ("deBruijnIndex: " <> show i))
                             (Inf . Bound) (elemIndex i env)
 where
  iterm :: Expr -> ITerm
  iterm l = case deBruijnIndex env l of
    Inf it -> it
    _      -> error ("iterm: " <> show l)
