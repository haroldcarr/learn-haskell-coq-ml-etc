{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module HC.ParseAndPrint where

import           Data.List
import           Language.LBNF

bnfc [lbnf|

-- Statement
GLet   .   GStm      ::= "let" Ident "=" GITerm         ;
GEval  .   GStm      ::= GITerm                         ;

-- Type
GTFree .   GType     ::= Ident                          ;
GFun   .   GType     ::= GType "->" GType               ;

-- ITerm
GAnn   .   GITerm    ::= "ann" GCTerm "::" GType        ;
GVar   .   GITerm    ::= Ident                          ;
GAp    .   GITerm    ::= "(" GITerm GCTerm ")"          ; -- hack to force left associativity by parens

-- CTerm
GInf   .   GCTerm    ::= GITerm                         ;
GLam   .   GCTerm    ::= "\\" Ident [Ident] "->" GCTerm ;

[]     .   [Ident]   ::=                                ;
(:)    .   [Ident]   ::= Ident  [Ident]                 ;
     |]

data Typ
  = TFree Name    -- type identifier
  | Fun   Typ Typ -- function arrows (not polymorphic)
  deriving (Eq, Show)

data ITerm -- Inferable (Term ↑)
  = Ann   CTerm Typ -- explicit Annotation
  | Bound Int        -- variable : de Bruijn indice (i.e., no need to alpha reduce)
                     -- num binders between its binder and occurrence
  | Free  Name       -- variable : e.g., top level
  | ITerm :@: CTerm  -- application
  deriving (Eq, Show)

data CTerm -- Checkable (Term ↓)
  = Inf ITerm
  | Lam CTerm
  deriving (Eq, Show)

data Name
  = Global String
  | Local  Int    -- when passing a binder in an algorithm,
                  -- temporarily convert a bound var into a free var
  | Quote  Int
  deriving (Eq, Show)

gtypeToTyp :: GType -> Typ
gtypeToTyp  = \case
  GTFree i   -> TFree (Global (show i))
  GFun   l r -> Fun (gtypeToTyp l) (gtypeToTyp r)

deBruijnI :: [Ident] -> GITerm -> ITerm
deBruijnI env = \case
  GAnn cterm typ   -> Ann (deBruijnC env cterm) (gtypeToTyp typ)
  GVar i@(Ident n) -> case elemIndex i env of
                        Nothing -> Free (Global n)
                        Just ix -> Bound ix
  GAp f a          -> deBruijnI env f :@: deBruijnC env a

deBruijnC :: [Ident] -> GCTerm -> CTerm
deBruijnC env = \case
  GInf iterm         -> Inf (deBruijnI env iterm)
  GLam i    []  body -> Lam (deBruijnC (i:env) body)
  GLam i (x:xs) body -> Lam (deBruijnC (i:env) (GLam x xs body))
