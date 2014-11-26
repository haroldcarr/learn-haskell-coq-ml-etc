{-
Created       : 2014 Nov 25 (Tue) 18:20:43 by Harold Carr.
Last Modified : 2014 Nov 25 (Tue) 21:29:27 by Harold Carr.
-}

import           Parsing

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 7

------------------------------------------------------------------------------
-- EXERCISE 0

e0 :: [Test]
e0 = U.t "e0"
     (parse item "hello")
     [('h',"ello")]

------------------------------------------------------------------------------
-- EXERCISE 1

{-
:t  (return 1 +++ return 2)
;=> (return 1 +++ return 2) :: Num a => Parser a
-}

e1 :: [Test]
e1 = U.t "e1"
     (parse (return (1::Int) +++ return 2) "2")
     [(1,"2")]

------------------------------------------------------------------------------
-- EXERCISE 2

e2 :: [Test]
e2 = U.t "e2"
     (parse (return (1::Int)) "hello")
     [(1,"hello")]

------------------------------------------------------------------------------
-- EXERCISE 3

e3 :: [Test]
e3 = U.t "e3"
     (parse (item +++ return 'a') "hello")
     [('h',"ello")]


------------------------------------------------------------------------------
-- EXERCISE 4

e4 :: [Test]
e4 = U.t "e4"
     (parse (return (2::Int) >>= \_ -> item) "hello")
     [('h',"ello")]

------------------------------------------------------------------------------
-- EXERCISE 5

e5t :: [Test]
e5t = U.t "e5t"
     (parse (char 'a' +++ return 'b') "abc")
     [('a',"bc")]

e5f :: [Test]
e5f = U.t "e5f"
     (parse (char 'a' +++ return 'b') "1aca")
     [('b',"1aca")]

------------------------------------------------------------------------------
-- EXERCISE 6

e6p :: [Test]
e6p = U.t "e6p"
     (parse int "01234")
     [(1234,"")]

e6n :: [Test]
e6n = U.t "e6n"
     (parse int "-0001234")
     [(-1234,"")]

------------------------------------------------------------------------------
-- EXERCISE 7

e7 :: [Test]
e7 = U.t "e7"
     (parse comment "-- foo bar\nbaz")
     [((),"baz")]

------------------------------------------------------------------------------
-- EXERCISE 8

e8 :: [Test]
e8 = U.t "e8"
     (parse expr "34 - 3 - 1")
     [(30,"")]

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e1 ++ e2 ++ e3 ++ e4 ++ e5t ++ e5f ++
                               e6p ++ e6n ++ e7 ++ e8

-- End of file.


