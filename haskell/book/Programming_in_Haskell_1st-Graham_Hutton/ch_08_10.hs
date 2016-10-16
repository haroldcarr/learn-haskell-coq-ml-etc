{-
Created       : 2015 Apr 20 (Mon) 18:27:09 by Harold Carr.
Last Modified : 2015 Apr 22 (Wed) 12:00:47 by Harold Carr.
-}

module Ch_08_10 where

-- see also: https://www.cs.nott.ac.uk/~gmh/pearl.pdf

import           Data.Char       (isAlpha, isAlphaNum, isDigit, isLower,
                                  isSpace, isUpper)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 8.10 Exercises

newtype Parser a = Parser (String -> [(a, String)])

failure :: Parser a
failure = Parser (\_ -> [])

item :: Parser Char
item = Parser (\inp -> case inp of
                           []     -> []
                           (x:xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) inp = p inp

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = Parser (\inp -> case parse p inp of
                                  []        -> []
                                  [(v,out)] -> parse (f v) out)
    return v = Parser (\inp -> [(v,inp)])

e00 :: [Test]
e00 = U.t "e00"
     (parse (return 'c') "abc")
     [('c',"abc")]

e01 :: [Test]
e01 = U.t "e01"
     (parse failure "abc")
     ([] :: [(Int, String)])

e02a :: [Test]
e02a = U.t "e02a"
     (parse item "\n")
     [('\n',"")]

e02b :: [Test]
e02b = U.t "e02b"
     (parse item "")
     ([] :: [(Char, String)])

e03 :: [Test]
e03 = U.t "e03"
     (parse item "abc")
     ([('a',"bc")] :: [(Char, String)])

pc2d2r3 :: Parser (Char, Char)
pc2d2r3 = do
    x <- item
    item
    y <- item
    return (x, y)

e04 :: [Test]
e04 = U.t "e04"
     (parse pc2d2r3 "abcdef")
     [(('a','c'),"def")]

(+++) :: Parser a -> Parser a -> Parser a
(Parser p) +++ (Parser q) =
    Parser (\inp -> case p inp of
                        []        -> parse (Parser q) inp
                        [(v,out)] -> [(v,out)])

e05 :: [Test]
e05 = U.t "e05"
     (parse (item +++ return 'd') "abc")
     [('a',"bc")]

e06 :: [Test]
e06 = U.t "e06"
     (parse (failure +++ return 'd') "abc")
     [('d',"abc")]

e07 :: [Test]
e07 = U.t "e07"
     (parse (failure +++ failure) "abc")
     ([] :: [(Char, String)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

e08 :: [Test]
e08 = U.t "e08"
     (parse (sat (=='a')) "abc")
     [('a', "bc")]

digit, lower, upper, letter, alphanum :: Parser Char
digit    = sat isDigit
lower    = sat isLower
upper    = sat isUpper
letter   = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x   = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- allall :: Parser (Char, Char)
allall = do
    d <- digit
    l <- lower
    u <- upper
    a <- alphanum
    c <- char 'c'
    s <- string "carr"
    return (d,l,u,a,c,s)

e09 :: [Test]
e09 = U.t "e09"
     (parse allall "1aA9ccarr")
     [(('1','a','A','9','c',"carr"), "")]

many,many1 :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 p = do v  <- p
             vs <- many p
             return (v:vs)

e10 :: [Test]
e10 = U.t "e10"
     (parse (many digit) "123abc")
     [("123", "abc")]

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- many1 digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

e11 :: [Test]
e11 = U.t "e11"
     (parse ident "isSpace def")
     [("isSpace", " def")]

e12 :: [Test]
e12 = U.t "e12"
     (parse nat "123 abc")
     [(123, " abc")]

e13 :: [Test]
e13 = U.t "e13"
     (parse space "    abc")
     [((), "abc")]

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

pListOfNats :: Parser [Int]
pListOfNats = do symbol "["
                 n <- natural
                 ns <- many (do symbol ","
                                natural)
                 symbol "]"
                 return (n:ns)

e14 :: [Test]
e14 = U.t "e14"
     (parse pListOfNats "[1,2,3]")
     [([1,2,3], "")]

e15 :: [Test]
e15 = U.t "e15"
     (parse pListOfNats "[1,2,]")
     []

expr :: Parser Int
expr = do t <- term
          do s <- symbol "+" +++ symbol "-"
             e <- expr
             return ((if s == "+" then (+) else (-)) t  e)
             +++ return t

term :: Parser Int
term = do f <- factor
          do s <- symbol "*" +++ symbol "/"
             t <- term
             return (if s == "*" then f * t else f `div` t)
             +++ return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
            +++ natural

eval :: String -> Int
eval xs = case parse expr xs of
              [(n,[])]  -> n
              [(_,out)] -> error ("unused input: " ++ out)
              []        -> error ("invalid input: " ++ xs)

e16 = U.t "e16"
     (eval "2*3+4")
     10

e16a = U.t "e16a"
     (eval "2*3-4")
     2

e16b = U.t "e16b"
     (eval "(2*30)/15")
     4

e17 = U.t "e17"
     (eval "2*(3+4)")
     14

e18 = U.t "e18"
     (eval "2 * (3 + 4)")
     14

e19 = U.e "e19"
     (eval "2*3--4")
     "unused input: --4"

------------------------------------------------------------------------------
-- 1

int :: Parser Int
int = do symbol "-"
         n <- nat
         return (-n)
         +++ nat

e1a :: [Test]
e1a = U.t "e1a"
     (parse int "-11ab")
     [((-11),"ab")]

e1b :: [Test]
e1b = U.t "e1a"
     (parse int "45ab")
     [(45,"ab")]

------------------------------------------------------------------------------
-- 2

consumeLine :: Parser ()
consumeLine = do char '\n'
                 return ()
                 +++ do item
                        consumeLine

comment :: Parser ()
comment = do
    symbol "--"
    consumeLine
    return ()

e2 :: [Test]
e2 = U.t "e2"
     (parse comment "-- a line 12 34\nab")
     [((),"ab")]

------------------------------------------------------------------------------
-- 3 did not do

------------------------------------------------------------------------------
-- 4 did not do

------------------------------------------------------------------------------
-- 5 did not do

------------------------------------------------------------------------------
-- 6

-- see expr and term definitions and e16a and e16b

------------------------------------------------------------------------------
-- 7

-- TODO
{-
see p. 344 in (uses 'primary' idea suggest by Hutton, but in Prolog):
   https://books.google.com/books?id=Skdlivv48uYC&pg=PA344&lpg=PA344&dq=arithmetic+expression+grammar+with+exponentiation&source=bl&ots=kyeIaWHTl9&sig=F0_xrzOHm_lUIXaWQKqJJ6JF2QU&hl=en&sa=X&ei=1ec2VczEOc32yQSZ44DABw&ved=0CCwQ6AEwBDgK#v=onepage&q=arithmetic%20expression%20grammar%20with%20exponentiation&f=false

see p. T-23 in (uses 'primary'):
    https://books.google.com/books?id=dYYCjriMyvwC&pg=SL20-PA23&lpg=SL20-PA23&dq=arithmetic+expression+grammar+with+exponentiation&source=bl&ots=PfEcrfKVFZ&sig=0IQ9aMFVBOZrXDFkWQvTHOpjCMI&hl=en&sa=X&ei=1ec2VczEOc32yQSZ44DABw&ved=0CC8Q6AEwBTgK#v=onepage&q=arithmetic%20expression%20grammar%20with%20exponentiation&f=false
see exercise 11.5.2 in
   http://infolab.stanford.edu/~ullman/focs/ch11.pdf
see 10. in
   http://math.purduecal.edu/~rlkraft/cs31600-2012/chapter03/syntax-examples.html
-}

------------------------------------------------------------------------------
-- 8

-- TODO
------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e00 ++ e01 ++ e02a ++ e02b ++
                               e03 ++ e04 ++ e05 ++ e06 ++ e07 ++
                               e08 ++ e09 ++ e10 ++ e11 ++ e12 ++
                               e13 ++ e14 ++ e15 ++
                               e16 ++ e16a ++ e16b ++
                               e17 ++ e18 ++ e19 ++
                               e1a ++ e1b ++
                               e2

-- End of file.


