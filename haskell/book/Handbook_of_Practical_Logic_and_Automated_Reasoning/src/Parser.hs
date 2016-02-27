{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- http://www.dcc.fc.up.pt/~pbv/aulas/linguagens/progs/Parser.hs

module Parser where

import qualified Text.Parsec          as P (eof, many1, runP, (<|>))
import qualified Text.Parsec.Expr     as P (Assoc (AssocLeft), Operator (Infix),
                                            buildExpressionParser)
import qualified Text.Parsec.Language as P (haskell)
import qualified Text.Parsec.Token    as P (identifier, natural, parens,
                                            reserved, reservedOp, whiteSpace)

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
lexer = P.haskell

reserved   = P.reserved lexer
reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
natural    = P.natural lexer
parens     = P.parens lexer
whiteSpace = P.whiteSpace lexer

test = [ p "\\x y->x+y"
       , p "let x=1 in 2*(x+1)" -- Let "x" (Const 1) (Const 2 :* (Var "x" :+ Const 1))
       , p "\\x y->x+y"         -- Lambda "x" (Lambda "y" (Var "x" :+ Var "y")
       ]

testE = p "x*2)"                -- parse error at (line 1, column 4):    unexpected ")"    expecting digit, identifier, natural, "(", "*", operator or end of input

{- parses a term, followed by whitespace and end-of-file -}
start = do
    e <- term
    whiteSpace
    P.eof
    return e

term =
    do  -- let/in construct
        reserved "let"
        x <- identifier
        reservedOp "="
        e1 <- term
        reserved "in"
        e2 <- term
        return (Let x e1 e2)
    P.<|> do -- if/then/else construct
        reserved "if"
        e1 <- term
        reserved "then"
        e2 <- term
        reserved "else"
        e3 <- term
        return (IfThenElse e1 e2 e3)
    P.<|> do -- fixpoint operator
        reserved "fix"
        e <- term
        return (Fix e)
    P.<|> do -- lambda-abstraction : multiple identifiers expanded into curried form
        reservedOp "\\"
        xs <- P.many1 identifier
        reservedOp "->"
        e <- term
        return (foldr Lambda e xs)
    P.<|> expr -- otherwise: an infix expression

-- using infix arithmetic and comparision operators
expr = P.buildExpressionParser table applExpr
    where table = [ [ binary "*" (:*) P.AssocLeft ]
                  , [ binary "+" (:+) P.AssocLeft
                    , binary "-" (:-) P.AssocLeft
                    ]
                  ]
          --     s op assoc
          binary s op = P.Infix (do { reservedOp s; return op })

-- application (e0 e1 e2 .. en)
applExpr = do
    es <- P.many1 delimExpr
    return (foldl1 App es)

-- self-delimited expressions: identifiers, constants or parentesised terms
delimExpr =
    do
        x <- identifier
        return (Var x)
    P.<|> do
        n <- natural
        return (Const (fromInteger n))
    P.<|> parens term

p = P.runP start (3::Int) "foo"

ps x = case p x of
    (Right r) -> show r
    (Left  l) -> show l

pr x = let (Right r) = p x in r

-- end of file ---
