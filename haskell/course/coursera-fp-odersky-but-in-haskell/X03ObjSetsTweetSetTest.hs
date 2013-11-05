{-
Created       : 2013 Oct 01 (Tue) 14:46:30 by carr.
Last Modified : 2013 Nov 04 (Mon) 21:15:03 by carr.
-}

module X03ObjSetsTweetSetTest where

import Control.Monad
import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util
import X03ObjSetsTweetData
import X03ObjSetsTweetReader
import X03ObjSetsTweetSet

set1  = Empty
set2  = incl set1  (Tweet "a" "a body" 20)
set3  = incl set2  (Tweet "b" "b body" 20)
c     =             Tweet "c" "c body"  7
d     =             Tweet "d" "d body"  9
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
    [teq "filter': on empty set"     (size (filter' (\x -> getUser     x == "a") set1))                 0
    ,teq "filter': a on set5"        (size (filter' (\x -> getUser     x == "a") set5))                 1
    ,teq "filter': 20 on set5"       (size (filter' (\x -> getRetweets x ==  20) set5))                 2
    ,teq "union: set4c and set4d"    (size (set4c `union` set4d))                                       4
    ,teq "union: with empty set (1)" (size (set5  `union` set1))                                        4
    ,teq "union: with empty set (2)" (size (set1  `union` set5))                                        4
    ,teq "descending: set5"          (null trends)                                                      False
    ,teq "descending: ..."           ((getUser (head trends) == "a") || (getUser (head trends) == "b")) True
    ]

------------------------------------------------------------------------------

googleKeywords = ["android", "Android", "galaxy", "Galaxy", "nexus", "Nexus"]
appleKeywords  = ["ios", "iOS", "iphone", "iPhone", "ipad", "iPad"]

main = do
    runTestTT tests

    gizmodoTweets     <- parseTweets gizmodoJSON
    techcrunchTweets  <- parseTweets techcrunchJSON
    engadgetTweets    <- parseTweets engadgetJSON
    amazondealsTweets <- parseTweets amazondealsJSON
    cnetTweets        <- parseTweets cnetJSON
    gadgetlabTweets   <- parseTweets gadgetlabJSON
    mashableTweets    <- parseTweets mashableJSON

    let all = allTweets [gizmodoTweets, techcrunchTweets, engadgetTweets, amazondealsTweets, cnetTweets, gadgetlabTweets,  mashableTweets]
    runTestTT $ TestList[teq "size all"          (size all)          695]
    -- foreach all  (\x -> putStrLn (show x))

    let googleTweets = collectByKeywords all googleKeywords
    runTestTT $ TestList[teq "size googleTweets" (size googleTweets)  38]

    let appleTweets  = collectByKeywords all appleKeywords
    runTestTT $ TestList[teq "size appleTweets"  (size appleTweets)  150]

    let trending = descendingByRetweet $ googleTweets `union ` appleTweets
    runTestTT $ TestList[teq "size trending"     (length trending)   179] -- TODO : why is this not 150 + 38 (maybe because of identical text but different users)

    -- TODO : ensure that selected Tweets in trending are as expected

    -- forM_ trending print

-- End of file.
