{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- http://www.dcc.fc.up.pt/~pbv/aulas/linguagens/progs/Parser.hs

module Parser where

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language
import qualified Text.Parsec.Token    as P


-- abstract syntax for language terms
data Term = Var Ident               -- variables
          | Lambda Ident Term       -- abstraction
          | App Term Term           -- application
          | Const Int               -- constants
          | Term :+ Term            -- arithmetic operators
          | Term :- Term
          | Term :* Term
          | IfZero Term Term Term   -- conditional
          | IfThenElse Term Term Term   -- conditional
          | Let Ident Term Term     -- local definition
          | Fix Term                -- fixed-point operator
            deriving (Eq, Show)

-- indentifiers are just strings
type Ident = String

-- setup a Haskell-like lexer using the Parsec tokenizer
lexer = haskell

reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
natural    = P.natural lexer
parens     = P.parens lexer
whiteSpace = P.whiteSpace lexer


{- This is the start non-terminal: parses a term
   followed by whitespace and end-of-file

   Examples (at the GHCi prompt):
   > parseTest "let x=1 in 2*(x+1)"
   Let "x" (Const 1) (Const 2 :* (Var "x" :+ Const 1))
   > parseTest "\\x y->x+y"
   Lambda "x" (Lambda "y" (Var "x" :+ Var "y")
   > parseTest "x*2)"
   parse error at (line 1, column 4):
   unexpected ")"
   expecting digit, identifier, natural, "(", "*", operator or end of input

   The "parseTest" function  is exported by the Parsec library;
   see also the documentation on "parse" and "parseFromFile".
-}
start = do { e<-term; whiteSpace; eof; return e }

-- the rest of the grammar follows

-- terms of the Fun language
term = do { reserved "let"  -- let/in construct
          ; x<-identifier
          ; reservedOp "="
          ; e1<-term
          ; reserved "in"
          ; e2<-term
          ; return (Let x e1 e2)
          }
       -- if/then/else construct
       <|> do { reserved "if"
              ; e1 <- term
              ; reserved "then"
              ; e2 <- term
              ; reserved "else"
              ; e3 <- term
              ; return (IfThenElse e1 e2 e3)
              }
       -- fixpoint operator
       <|> do { reserved "fix"
              ; e <- term
              ; return (Fix e)
              }
       -- lambda-abstraction
       -- multiple identifiers are expanded into curried form
       <|> do { reservedOp "\\"
              ; xs <- many1 identifier
              ; reservedOp "->"
              ; e <- term
              ; return (foldr Lambda e xs)
              }
       -- otherwise: an infix expression
       <|> expr


-- an expression using infix arithmetic and comparision operators
-- uses the Parsec expression parser builder
expr = buildExpressionParser table applExpr
    where table = [ [ binary "*" (:*) AssocLeft ]
                  , [ binary "+" (:+) AssocLeft
                    , binary "-" (:-) AssocLeft
                    ]
                  ]
          --     s op assoc
          binary s op = Infix (do {reservedOp s; return op})

-- an application (e0 e1 e2 .. en)
-- where ei are self-delimited expressions
applExpr = do { es<-many1 delimExpr
              ; return (foldl1 App es)
              }

-- self-delimited expressions:
-- identifiers, constants or parentesised terms
delimExpr = do { x<-identifier; return (Var x) }
             <|> do { n<-natural; return (Const (fromInteger n)) }
             <|> parens term

par x = let (Right r) = runP start (3::Int) "foo" x in r

-- par "\\x y->x+y"

-- end of file ---
