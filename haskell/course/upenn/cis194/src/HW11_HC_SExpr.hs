{-
Created       : 2014 Jun 19 (Thu) 17:22:43 by Harold Carr.
Last Modified : 2014 Jun 21 (Sat) 19:46:02 by Harold Carr.
-}

module HW11_HC_SExpr where

import           HW10_HC_AParser

import           Control.Applicative
import           Data.Char           (isUpper)

import qualified Test.HUnit          as T
import qualified Test.HUnit.Util     as U

------------------------------------------------------------------------------
-- Examples from lecture

-- data Employee = Emp { name :: String, phone :: String } deriving (Eq, Show)

-- nondeterministic arithmetic
(.+), (.*) :: [Integer] -> [Integer] -> [Integer]
(.+) = liftA2 (+)    -- addition lifted to some Applicative context
(.*) = liftA2 (*)    -- same for multiplication

lec :: T.Test
lec = T.TestList
    [
      -- example of non-deterministic list applicative
      U.teq "l0" (Emp <$> ["A", "B"] <*> ["1", "2"])            [Emp "A" "1",Emp "A" "2",Emp "B" "1",Emp "B" "2"]

      -- (either 4 or 5) times 2, plus either 6 or 1
    , U.teq "l1" (([4,5] .* pure 2) .+ [6,1])                   [14,9,16,11::Integer]

      -- possibly-failing arithmetic
--    , U.teq "l2" ((Just (3::Integer) .+ Just 5)  .* Just 8)   []
--    , U.teq "l3" ((Just 3 .+ Nothing) .* Just 8)              Nothing
    ]

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = fmap (++) (fmap (\x -> [x]) p) <*> (fmap (\x -> [x]) p)

ex1' :: T.Test
ex1' = T.TestList
    [
      U.teq "e10" (runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH") (Just ("ABC","dEfgH"))
    , U.teq "e11" (runParser (oneOrMore  (satisfy isUpper)) "ABCdEfgH") (Just ("ABC","dEfgH"))
    , U.teq "e12" (runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh") (Just ("","abcdeFGh"))
    , U.teq "e13" (runParser (oneOrMore  (satisfy isUpper)) "abcdeFGh") Nothing
    ]

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

ex2'' :: T.Test
ex2'' = T.TestList
    [
    ]

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

ex3' :: T.Test
ex3' = T.TestList
    [
    ]

------------------------------------------------------------------------------

hw11 :: IO T.Counts
hw11 = do
    T.runTestTT lec
    T.runTestTT ex1'
    T.runTestTT ex2''
    T.runTestTT ex3'
