{-
Created       : 2014 Jun 19 (Thu) 10:59:09 by Harold Carr.
Last Modified : 2014 Jun 19 (Thu) 14:31:47 by Harold Carr.
-}

module HW10_HC_AParser where

import           Control.Applicative

import           Data.Char
import           Data.List           (unfoldr)

import qualified Test.HUnit          as T
import qualified Test.HUnit.Util     as U

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

ex0 :: T.Test
ex0 = T.TestList
    [
      U.teq "e00" (runParser (satisfy isUpper) "ABC")      (Just ('A',"BC"))
    , U.teq "e01" (runParser (satisfy isUpper) "abc")      Nothing
    , U.teq "e02" (runParser (char 'x')        "xyz")      (Just ('x',"yz"))

    , U.teq "e03" (runParser posInt             "10ab20")  (Just (10,"ab20"))
    , U.teq "e03" (runParser posInt               "ab20")  Nothing
    , U.teq "e03" (runParser posInt                 "20")  (Just (20,""))
    ]

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

------------------------------------------------------------------------------
-- Exercise 1

instance Functor Parser where
    fmap f (Parser p) = Parser (\s -> case p s of
                                          Nothing       -> Nothing
                                          Just (r,rest) -> Just (f r, rest))

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "e10" (runParser (fmap (toUpper) (satisfy isLower))
                             "abc")
                  (Just ('A', "bc"))
    , U.teq "e11" (runParser (fmap (*2)      posInt)
                             "20")
                  (Just (40, ""))
    ]

------------------------------------------------------------------------------
-- Exercise 2

instance Applicative Parser where
    pure    = undefined
    Parser l <*> Parser r = Parser (\s -> case l s of
                                              Nothing -> Nothing
                                              Just (a,rest) -> case r rest of
                                                                   Nothing -> Nothing
                                                                   Just (a',rest') -> Just (a a', rest'))


-- for test

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Eq, Show)

parseName  :: Parser Name
parseName = Parser $ pp isAlpha

parsePhone :: Parser String
parsePhone = Parser $ pp isDigit

parseEmployee :: Parser Employee
parseEmployee = Emp <$> parseName <*> parsePhone

-- runParser (runParser (Emp <$> parseName) "Harold") "23"

pp :: (Char -> Bool) -> String -> Maybe (String, String)
pp f s0 = pp' s0 []
  where
    pp' s acc = case (runParser (satisfy f)) s of
           Nothing -> if null acc then Nothing else Just (reverse acc, s) -- TODO avoid reverse
           (Just (c,rest)) -> pp' rest (c:acc)

ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "e20" (runParser parseName     "Harold8016824058etc") (Just ("Harold", "8016824058etc"))
    , U.teq "e21" (runParser parsePhone    "Harold8016824058etc") Nothing
    , U.teq "e22" (runParser parsePhone    "8016824058Harold000") (Just ("8016824058", "Harold000"))
    , U.teq "e23" (runParser parseEmployee "Harold8016824058etc") (Just (Emp "Harold" "8016824058", "etc"))
    ]

------------------------------------------------------------------------------
-- Exercise 3 -- TODO

ex3 :: T.Test
ex3 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 4 -- TODO

ex4 :: T.Test
ex4 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 5 -- TODO

ex5 :: T.Test
ex5 = T.TestList
    [
    ]

------------------------------------------------------------------------------

hw10 :: IO T.Counts
hw10 = do
    T.runTestTT ex0
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3
    T.runTestTT ex4
    T.runTestTT ex5

-- End of file.
