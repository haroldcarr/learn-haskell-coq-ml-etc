{-# LANGUAGE OverloadedStrings #-}

module Lib where

------------------------------------------------------------------------------
import           Data.Char                   (isAlpha, isDigit)
import           Data.SCargot
import           Data.SCargot.Language.Basic
import           Data.SCargot.Repr
import qualified Data.Text                   as T
import           Text.Parsec                 (alphaNum, char, many1, satisfy,
                                              (<|>))
import           Text.Parsec.Text            (Parser)
------------------------------------------------------------------------------

aa :: Either String [RichSExpr T.Text]
aa = decode (asRich basicParser) "(a b)"

bb :: Either String [RichSExpr T.Text]
bb = decode (asRich basicParser)
    "(ProposalMsg (B 40a05a2 (BD 1 2 (Prop TX/2) (QC (VD (BI 22ab8de 1 1) (BI 8fd092d 1 0)) (LIWS (LI (BI 0000000 0 0) a4698ed) (a2 a3 a4))))))"

cc :: Either String [RichSExpr T.Text]
cc = decode (asRich basicParser)
    "(ProposalMsg (B 40a05a2 (BD 1 2 (Prop \"TX/2\") (QC (VD (BI 22ab8de 1 1) (BI 8fd092d 1 0)) (LIWS (LI (BI 0000000 0 0) a4698ed) (a2 a3 a4))))))"

------------------------------------------------------------------------------

data Atom
  = Atom T.Text
  | Str  T.Text
  deriving (Eq, Show)

pAtom :: Parser Atom
pAtom =  Str  . T.pack
         <$> (do _ <- char '"'
                 s <- many1 (satisfy (\a -> isDigit a || isAlpha a|| a == '/'))
                 _ <- char '"'
                 pure s)
     <|> Atom . T.pack
         <$> many1 alphaNum

sAtom :: Atom -> T.Text
sAtom (Str  t) = t
sAtom (Atom t) = t

myParser :: SExprParser Atom (SExpr Atom)
myParser = mkParser pAtom

myPrinter :: SExprPrinter Atom (SExpr Atom)
myPrinter = flatPrint sAtom

x :: Either String [RichSExpr Atom]
x = decode (asRich myParser) "x"
y :: Either String [RichSExpr Atom]
y = decode (asRich myParser) "\"x\""

z :: Either String [RichSExpr Atom]
z = decode (asRich myParser)
    "(ProposalMsg (B 40a05a2 (BD 1 2 (Prop \"TX/2\") (QC (VD (BI 22ab8de 1 1) (BI 8fd092d 1 0)) (LIWS (LI (BI 0000000 0 0) a4698ed) (a2 a3 a4))))))"

z' :: Either String [SExpr Atom]
z' = decode myParser
    "(ProposalMsg (B 40a05a2 (BD 1 2 (Prop \"TX/2\") (QC (VD (BI 22ab8de 1 1) (BI 8fd092d 1 0)) (LIWS (LI (BI 0000000 0 0) a4698ed) (a2 a3 a4))))))"
