{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Parser where

import qualified Text.Parsec          as P (eof, runP, (<?>), (<|>))
import qualified Text.Parsec.Expr     as P (Assoc (AssocLeft),
                                            Operator (Infix, Prefix),
                                            buildExpressionParser)
import qualified Text.Parsec.Language as P (haskell)
import qualified Text.Parsec.Token    as P (identifier, parens, reservedOp,
                                            whiteSpace)

-- 2.1

data Formula a =
    F
  | T
  | Atom  a
  | Not   (Formula a)
  | And   (Formula a) (Formula a)
  | Or    (Formula a) (Formula a)
  | Impl  (Formula a) (Formula a)
  | Iff   (Formula a) (Formula a)
  | Forall String     (Formula a)
  | Exists String     (Formula a)
  deriving (Eq, Show)

-- Haskell lexer via Parsec tokenizer
lexer = P.haskell

reservedOp = P.reservedOp lexer
identifier = P.identifier lexer
parens     = P.parens     lexer
whiteSpace = P.whiteSpace lexer

test = [ p "p ^ q"
       , p "p v q ^ z"
       ]

testE = p "x*2)"

p = P.runP start (3::Int) "foo"

pr x = let (Right r) = p x in r

{- parses a term, followed by whitespace and end-of-file -}
start = do
    e <- expr
    whiteSpace
    P.eof
    return e

expr = P.buildExpressionParser table term P.<?> "expression"

term =  parens expr P.<|> atom P.<?> "simple expression"

atom = do
    i <- identifier
    return (Atom i)

table = [ [ prefix "~"   Not ]
        , [ binary "^"   And  P.AssocLeft ]
        , [ binary "v"   Or   P.AssocLeft ]
        , [ binary "-->" Impl P.AssocLeft ]
        , [ binary "<->" Iff  P.AssocLeft ]
        ]

--      name fun assoc
binary  name fun       = P.Infix  (do { reservedOp name; return fun })
prefix  name fun       = P.Prefix (do { reservedOp name; return fun })

-- end of file ---
