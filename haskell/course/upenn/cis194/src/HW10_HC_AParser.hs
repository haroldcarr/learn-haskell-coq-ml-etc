{-
Created       : 2014 Jun 19 (Thu) 10:59:09 by Harold Carr.
Last Modified : 2014 Jun 19 (Thu) 13:38:16 by Harold Carr.
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
    _ <*> _ = undefined

-- for test

type Name = String
data Employee = Emp { name :: Name, phone :: String }

parseName  :: Parser Name
parseName = Parser (\s -> undefined)

parsePhone :: Parser String
parsePhone = undefined

pp :: (Char -> Bool) -> String -> String
pp f = unfoldr (\b -> case b of
                          []     -> Nothing
                          (x:xs) -> if f x then Just (x, xs) else Nothing)

ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "e20" (pp isAlpha "Harold8016824058") "Harold"
    , U.teq "e20" (pp isDigit "Harold8016824058") ""
    , U.teq "e20" (pp isDigit "8016824058Harold") "8016824058"
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
