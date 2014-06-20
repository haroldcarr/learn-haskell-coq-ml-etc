{-
Created       : 2014 Jun 19 (Thu) 10:59:09 by Harold Carr.
Last Modified : 2014 Jun 19 (Thu) 17:07:24 by Harold Carr.
-}

module HW10_HC_AParser where

import           Control.Applicative

import           Data.Char
import           Data.List           (unfoldr)
import           Data.Maybe          (fromJust)

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
    fmap f (Parser p) = Parser (\s -> p s >>= \(r,rest) -> return (f r, rest))

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "e10" (runParser (fmap (toUpper) (satisfy isLower))
                             "abc")
                  (Just ('A', "bc"))
    , U.teq "e11" (runParser (fmap (*2)      posInt)
                             "20")
                  (Just (40, ""))
    , U.teq "e12" (runParser (     (*3) <$>  posInt)
                             "20")
                  (Just (60, ""))
    ]

------------------------------------------------------------------------------
-- Exercise 2

instance Applicative Parser where
    pure                  = undefined -- TODO
    Parser l <*> Parser r = Parser (\s -> l s >>= \(a,rest) -> r rest >>= \(a',rest') -> return (a a', rest'))

-- for test

type Name = String
data Employee = Emp { name :: Name, phone :: String } deriving (Eq, Show)

parseName  :: Parser Name
parseName = Parser $ pp isAlpha

parsePhone :: Parser String
parsePhone = Parser $ pp isDigit

parseEmployee :: Parser Employee
parseEmployee = Emp <$> parseName <*> parsePhone

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

ex2' :: [T.Test]
ex2' = U.tt "trc"
      [ runParser parseEmployee "H8e"
      , runParser (Emp <$> parseName <*> parsePhone) "H8e"
      , runParser (Parser (\s -> let (r,rest) = fromJust $ (runParser parseName) s in Just (Emp r,rest)) <*> parsePhone) "H8e"
      , runParser (Parser (\s' -> (\s -> let (r,rest) = fromJust $ (runParser parseName) s in Just (Emp r,rest)) s'
                                  >>= \(a,rest) -> (runParser parsePhone) rest
                                                   >>= \(a',rest') -> return (a a', rest'))) "H8e"

      ,                   (\s -> let (r,rest) = fromJust $ (runParser parseName) s in Just (Emp r,rest)) "H8e"
                                  >>= \(a,rest) -> (runParser parsePhone) rest
                                                   >>= \(a',rest') -> return (a a', rest')

      ,                          let (r,rest) = fromJust $ (runParser parseName) "H8e" in Just (Emp r,rest)
                                  >>= \(a,rest) -> (runParser parsePhone) rest
                                                   >>= \(a',rest') -> return (a a', rest')

      ,                                                                                   Just (Emp "H","8e")
                                  >>= \(a,rest) -> (runParser parsePhone) rest
                                                   >>= \(a',rest') -> return (a a', rest')

      ,                                            (runParser parsePhone) "8e"
                                                   >>= \(a',rest') -> return (Emp "H" a', rest')

      ,                                                               Just   (Emp "H" "8", "e")

      ]
      (Just (Emp "H" "8", "e"))

------------------------------------------------------------------------------
-- Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> (satisfy ('a'==)) <*> (satisfy ('b'==))

abParser_ :: Parser ()
abParser_ = (\(_,_) -> ()) <$> abParser

-- an explicit one just to see how it works
intPairP :: Parser [Integer]
intPairP = Parser (\s -> let (x,r1) = fromJust $ runParser posInt s
                             (_,r2) = fromJust $ runParser (satisfy (' '==)) r1
                             (y,r3) = fromJust $ runParser posInt r2
                         in Just ([x,y], r3))

-- the real deal using applicatives
intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> (satisfy (' '==)) <*> posInt

ex3 :: T.Test
ex3 = T.TestList
    [
      U.teq "e300" (runParser abParser  "abcdef")   (Just (('a','b'),"cdef"))
    , U.teq "e301" (runParser abParser  "aebcdf")   Nothing
    , U.teq "e310" (runParser abParser_ "abcdef")   (Just (()       ,"cdef"))
    , U.teq "e311" (runParser abParser_ "aebcdf")   Nothing
    , U.teq "e320" (runParser intPairP  "12 34")    (Just ([12,34],""))
    , U.teq "e321" (runParser intPair   "12 34")    (Just ([12,34],""))
    ]

------------------------------------------------------------------------------
-- Exercise 4

instance Alternative Parser where
    empty                     = Parser (\_ -> Nothing)
    (Parser l) <|> (Parser r) = Parser (\s -> case l s of
                                                  Nothing -> r s
                                                  result  -> result)

ex4 :: T.Test
ex4 = T.TestList
    [
      U.teq "e40" (runParser ((satisfy isAlpha) <|> (satisfy isDigit)) "a1b2")   (Just ('a',"1b2"))
    , U.teq "e41" (runParser ((satisfy isDigit) <|> (satisfy isAlpha)) "a1b2")   (Just ('a',"1b2"))
    ]

------------------------------------------------------------------------------
-- Exercise 5

-- parses either an integer value or an uppercase character
intOrUppercase :: Parser ()
intOrUppercase = u <$> posInt <|> u <$> satisfy isUpper
    where u _ = ()

ex5 :: T.Test
ex5 = T.TestList
    [
      U.teq "e50" (runParser intOrUppercase "XYZ")  (Just ((), "YZ"))
    , U.teq "e51" (runParser intOrUppercase "foo")  Nothing
    , U.teq "e51" (runParser intOrUppercase "10X")  (Just ((), "X"))
    ]

------------------------------------------------------------------------------

hw10 :: IO T.Counts
hw10 = do
    T.runTestTT ex0
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT $ T.TestList $ ex2'
    T.runTestTT ex3
    T.runTestTT ex4
    T.runTestTT ex5

-- End of file.
