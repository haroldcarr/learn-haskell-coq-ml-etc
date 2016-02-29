{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}

module X_2_1_Prop_Syntax where

import qualified Test.HUnit                   as U (Counts, Test (TestList),
                                                    runTestTT)
import qualified Test.HUnit.Util              as U (t)
import qualified Text.Parsec                  as P (alphaNum, eof, letter,
                                                    oneOf, runP, (<?>), (<|>))
import qualified Text.Parsec.Expr             as P (Assoc (AssocRight),
                                                    Operator (Infix, Prefix),
                                                    buildExpressionParser)
import qualified Text.Parsec.Token            as P (GenLanguageDef (LanguageDef),
                                                    caseSensitive, commentEnd,
                                                    commentLine, commentStart,
                                                    identLetter, identStart,
                                                    identifier, makeTokenParser,
                                                    nestedComments, opLetter,
                                                    opStart, parens, reserved,
                                                    reservedNames, reservedOp,
                                                    reservedOpNames, whiteSpace)
import qualified Text.PrettyPrint.ANSI.Leijen as PP (text, (<>))

-- 2.1

data Formula a =
    F
  | T
  | Atom           a
  | Not   (Formula a)
  | And   (Formula a) (Formula a)
  | Or    (Formula a) (Formula a)
  | Impl  (Formula a) (Formula a)
  | Iff   (Formula a) (Formula a)
  | Forall String     (Formula a)
  | Exists String     (Formula a)
  deriving (Eq, Functor, Foldable, Show, Traversable)

------------------------------------------------------------------------------
-- Parser

langDef = P.LanguageDef
    { P.commentStart    = "{-"
    , P.commentEnd      = "-}"
    , P.commentLine     = "--"
    , P.nestedComments  = True
    , P.identStart      = P.letter
    , P.identLetter     = P.alphaNum
    , P.opStart         = P.oneOf "~^v-<"
    , P.opLetter        = P.oneOf ">"
    , P.reservedNames   = ["T", "F", "v", "Forall", "Exists"]
    , P.reservedOpNames = ["~","^","v","->","<->"]
    , P.caseSensitive   = True
    }

lexer        = P.makeTokenParser langDef
identifier   = P.identifier lexer
parens       = P.parens     lexer
reserved     = P.reserved   lexer
reservedOp   = P.reservedOp lexer
whiteSpace   = P.whiteSpace lexer

-- parser that return Either
p' = P.runP start (3::Int) "foo"

-- parser that expects success
pr x = let (Right r) = p' x in r

{- parses a term, followed by whitespace and end-of-file -}
start = do
    e <- formula
    whiteSpace
    P.eof
    return e

formula = P.buildExpressionParser table operatorOrAtom P.<?> "formula"

operatorOrAtom =
          parens formula
    P.<|> boolean
    P.<|> atom
    P.<?> "operatorOrAtom"

boolean =
          (reserved "T" >> return T)
    P.<|> (reserved "F" >> return F)

atom = do
    a <- identifier
    return (Atom a)

table = [ [ prefix "~"   Not ]
        , [ binary "^"   And  P.AssocRight ]
        , [ binary "v"   Or   P.AssocRight ]
        , [ binary "->"  Impl P.AssocRight ] -- TODO "-->"
        , [ binary "<->" Iff  P.AssocRight ]
        ]

-- TMP
-- also see: http://www.mega-nerd.com/erikd/Blog/CodeHacking/Haskell/parsec_expression_parsing.html
pt x = P.runP (reservedOp x) (3::Int) "foo"

--      name fun assoc
binary  name fun       = P.Infix  (reservedOp name >> return fun)
prefix  name fun       = P.Prefix (reservedOp name >> return fun)

tp0 = U.t "tp0"
      (pr "~(~T ^ ~F)")
      (Not (And (Not T) (Not F)))

tp1 = U.t "tp1"
      (pr "p ^ q")
      (And (Atom "p") (Atom "q"))

tp2 = U.t "tp2"
      (pr "p v q ^ z")
      (Or (Atom "p") (And (Atom "q") (Atom "z")))

tp3 = U.t "tp3"
      (pr "p v q -> r")
      (Impl (Or (Atom "p") (Atom "q")) (Atom "r"))

tp4 = U.t "tp4"
      (pr "p <-> r")
      (Iff (Atom "p") (Atom "r"))

------------------------------------------------------------------------------
-- pretty printer

data ComingFrom = A | O

showFormula f0 = case f0 of
    T           -> PP.text "T"
    F           -> PP.text "F"
    (Atom    a) -> PP.text a
    (Not     f) -> PP.text "~" PP.<> showFormula f
    (And f1 f2) -> se f1 A PP.<> PP.text " ^ " PP.<> se f2 A
    (Or  f1 f2) -> se f1 O PP.<> PP.text " v " PP.<> se f2 O
  where
    se f from = case (f, from) of
        (a@(Atom  _),_) -> showFormula a
        (n@(Not   _),_) -> showFormula n
        (a@(And _ _),A) -> showFormula a
        (o@(Or  _ _),O) -> showFormula o
        (f'         ,_) -> PP.text "(" PP.<> showFormula f' PP.<> PP.text ")"

pp = show . showFormula

ts1 = U.t "tShowFormula"
    (show (showFormula (pr "(p v q v r) ^ (p v q v  r ^ x  v y)")))
                           "(p v q v r) ^ (p v q v (r ^ x) v y)"

------------------------------------------------------------------------------
-- syntax operations

onAtomsContents :: Applicative f => (a -> f b) -> Formula a -> f (Formula b)
onAtomsContents = traverse

toa1 = U.t "toa1"
       (let r = pr "p v q" in (r, onAtomsContents (const "z") r))
       ( Or (Atom "p") (Atom "q")
       ,[Or (Atom 'z') (Atom 'z')])

-- TODO : use library function
onAtoms :: (a -> Formula a) -> Formula a -> Formula a
onAtoms f fs = case fs of
    (Atom   a  ) -> f a
    (Not    p  ) -> Not    (onAtoms f p)
    (Or     p q) -> Or     (onAtoms f p) (onAtoms f q)
    (And    p q) -> And    (onAtoms f p) (onAtoms f q)
    (Impl   p q) -> Impl   (onAtoms f p) (onAtoms f q)
    (Iff    p q) -> Iff    (onAtoms f p) (onAtoms f q)
    (Forall x p) -> Forall x             (onAtoms f p)
    (Exists x p) -> Exists x             (onAtoms f p)
    _            -> fs

------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tp0 ++ tp1 ++ tp2 ++ tp3 ++ tp4 ++
    ts1 ++
    toa1

-- end of file ---
