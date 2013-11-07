{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Nov 06 (Wed) 18:23:05 by carr.
-}

import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util
import FP01RecFun

tests = TestList
    [
    -- BALANCE
     teq "balance: '(if (zero? x) max (/ 1 x))' is balanced" (balance "(if (zero? x) max (/ 1 x))") True
    ,teq "balance: 'I told him ...' is balanced"             (balance "I told him (that it's not (yet) done).\n(But he wasn't listening)")
                                                                                                    True
    ,teq "balance: ':-)' is unbalanced"                      (balance ":-)")                        False
    ,teq "balance: counting is not enough"                   (balance "())(")                       False

    -- COUNT CHANGE
    ,teq "countChange: example given in instructions" (countChange   4                    [1,2])    3
    ,teq "countChange: sorted CHF"                    (countChange 300 [5,10,20,50,100,200,500]) 1022
    ,teq "countChange: no pennies"                    (countChange 301 [5,10,20,50,100,200,500])    0
    ,teq "countChange: unsorted CHF"                  (countChange 300 [500,5,50,100,20,200,10]) 1022

    -- PASCAL
    ,teq "pascal: col=0,row=2"                   (pascal   0   2)   1
    ,teq "pascal: col=1,row=2"                   (pascal   1   2)   2
    ,teq "pascal: col=1,row=3"                   (pascal   1   3)   3
    ,teq "pascal: col=3,row=10"                  (pascal   3  10) 120
    ,ter "pascal: col=-1,row=0 throws exception" (pascal (-1)  0)
                                                 "IllegalArgumentException: not a legal position: c:-1, r:0"
    ,ter "pascal: col=0,row=-1 throws exception" (pascal   0 (-1))
                                                 "IllegalArgumentException: not a legal position: c:0, r:-1"
    ,ter "pascal: col=4,row=3 throws exception"  (pascal   4   3)
                                                 "IllegalArgumentException: not a legal position: c:4, r:3"
    ]

main = runTestTT tests

-- End of file.
