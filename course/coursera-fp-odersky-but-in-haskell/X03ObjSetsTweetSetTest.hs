{-
Created       : 2013 Oct 01 (Tue) 14:46:30 by carr.
Last Modified : 2013 Oct 01 (Tue) 15:15:12 by carr.
-}

module X03ObjSetsTweetSetTest where

import Test.HUnit
import AssertError
import X03ObjSetsTweetSet

set1  = Empty
set2  = incl set1 (Tweet "a" "a body" 20)
set3  = incl set2 (Tweet "b" "b body" 20)
c     =           (Tweet "c" "c body"  7)
d     =           (Tweet "d" "d body"  9)
set4c = incl set3 c
set4d = incl set3 d
set5  = incl set4c d


seven  =    NonEmpty (Tweet "7" "7" 7) Empty Empty
three  =  incl seven (Tweet "3" "3" 3)
one    =  incl three (Tweet "1" "1" 1)
five   =  incl one   (Tweet "5" "5" 5)
nine   =  incl five  (Tweet "9" "9" 9)
eight  =  incl nine  (Tweet "8" "8" 8)

trends = descendingByRetweet set5
getUser     (Tweet user _ _       ) = user
getRetweets (Tweet _    _ retweets) = retweets

tests = TestList
    [TestCase $ assertEqual "filter': on empty set"     0     (size (filter' (\x -> (getUser x) == "a") set1))
    ,TestCase $ assertEqual "filter': a on set5"        1     (size (filter' (\x -> (getUser x) == "a") set5))
    ,TestCase $ assertEqual "filter': 20 on set5"       2     (size (filter' (\x -> (getRetweets x) == 20) set5))
    ,TestCase $ assertEqual "union: set4c and set4d"    4     (size (union set4c set4d))
    ,TestCase $ assertEqual "union: with empty set (1)" 4     (size (union set5 set1))
    ,TestCase $ assertEqual "union: with empty set (2)" 4     (size (union set1 set5))
    ,TestCase $ assertEqual "descending: set5"          False (isEmptyTL trends)
    ,TestCase $ assertEqual "descending: ..."           True  (((getUser (headTL trends)) == "a") || ((getUser (headTL trends)) == "b"))
    ]

main = do
    runTestTT tests

-- End of file.
