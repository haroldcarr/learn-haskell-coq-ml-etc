{-
Created       : 2013 Oct 01 (Tue) 14:46:30 by carr.
Last Modified : 2013 Oct 02 (Wed) 09:51:20 by carr.
-}

module X03ObjSetsTweetSetTest where

import AssertError
import Control.Monad
import Test.HUnit
import X03ObjSetsTweetData
import X03ObjSetsTweetReader
import X03ObjSetsTweetSet

set1  = Empty
set2  = incl set1  (Tweet "a" "a body" 20)
set3  = incl set2  (Tweet "b" "b body" 20)
c     =            (Tweet "c" "c body"  7)
d     =            (Tweet "d" "d body"  9)
set4c = incl set3  c
set4d = incl set3  d
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
    ,TestCase $ assertEqual "descending: set5"          False (null trends)
    ,TestCase $ assertEqual "descending: ..."           True  (((getUser (head trends)) == "a") || ((getUser (head trends)) == "b"))
    ]

------------------------------------------------------------------------------

googleKeywords = ["android", "Android", "galaxy", "Galaxy", "nexus", "Nexus"]
appleKeywords  = ["ios", "iOS", "iphone", "iPhone", "ipad", "iPad"]

main = do
    runTestTT tests
    --
    gizmodoTweets     <- parseTweets gizmodoJSON
    techcrunchTweets  <- parseTweets techcrunchJSON
    engadgetTweets    <- parseTweets engadgetJSON
    amazondealsTweets <- parseTweets amazondealsJSON
    cnetTweets        <- parseTweets cnetJSON
    gadgetlabTweets   <- parseTweets gadgetlabJSON
    mashableTweets    <- parseTweets mashableJSON

    let all = allTweets [gizmodoTweets, techcrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets,  mashableTweets]
    print (size all)
    runTestTT $ TestList[TestCase $ assertEqual "size all"         695 (size all)]
    -- foreach all  (\x -> putStrLn (show x))

    let googleTweets = collectByKeywords all googleKeywords Empty
    print (size googleTweets)
    runTestTT $ TestList[TestCase $ assertEqual "size googleTweets" 38 (size googleTweets)]

    let appleTweets  = collectByKeywords all appleKeywords Empty
    print (size appleTweets)
    runTestTT $ TestList[TestCase $ assertEqual "size appleTweets" 150 (size appleTweets)]

    let trending = descendingByRetweet $ googleTweets `union ` appleTweets
    print (length trending)
    runTestTT $ TestList[TestCase $ assertEqual "size trending"    179 (length trending)] -- TODO : why is this not 150 + 38 (maybe because of identical text but different users)

    -- TODO : ensure that selected Tweets in trending are as expected

    forM_ trending (\x -> putStrLn (show x))



-- End of file.
