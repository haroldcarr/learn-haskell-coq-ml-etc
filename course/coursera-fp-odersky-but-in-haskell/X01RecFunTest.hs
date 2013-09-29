{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Sep 29 (Sun) 09:03:11 by carr.
-}

import Test.HUnit
import AssertError
import X01RecFun

tests = TestList
    [
    -- BALANCE
     TestCase $ assertEqual "balance: '(if (zero? x) max (/ 1 x))' is balanced" True  (balance "(if (zero? x) max (/ 1 x))")
    ,TestCase $ assertEqual "balance: 'I told him ...' is balanced"             True  (balance "I told him (that it's not (yet) done).\n(But he wasn't listening)")
    ,TestCase $ assertEqual "balance: ':-)' is unbalanced"                      False (balance ":-)")
    ,TestCase $ assertEqual "balance: counting is not enough"                   False (balance "())(")

    -- COUNT CHANGE
    ,TestCase $ assertEqual "countChange: example given in instructions"    3 (countChange   4                    [1,2])
    ,TestCase $ assertEqual "countChange: sorted CHF"                    1022 (countChange 300 [5,10,20,50,100,200,500])
    ,TestCase $ assertEqual "countChange: no pennies"                       0 (countChange 301 [5,10,20,50,100,200,500])
    ,TestCase $ assertEqual "countChange: unsorted CHF"                  1022 (countChange 300 [500,5,50,100,20,200,10])

    -- PASCAL
    ,TestCase $ assertEqual "pascal: col=0,row=2"    1 (pascal   0   2)
    ,TestCase $ assertEqual "pascal: col=1,row=2"    2 (pascal   1   2)
    ,TestCase $ assertEqual "pascal: col=1,row=3"    3 (pascal   1   3)
    ,TestCase $ assertEqual "pascal: col=3,row=10" 120 (pascal   3  10)
    ,TestCase $ assertError "pascal: col=-1,row=0 throws exception"
                            "IllegalArgumentException: not a legal position: c:-1, r:0"
                                                       (pascal (-1)  0)
    ,TestCase $ assertError "pascal: col=0,row=-1 throws exception"
                            "IllegalArgumentException: not a legal position: c:0, r:-1"
                                                       (pascal   0 (-1))
    ,TestCase $ assertError "pascal: col=4,row=3 throws exception"
                            "IllegalArgumentException: not a legal position: c:4, r:3"
                                                       (pascal   4   3)
    ]

main = runTestTT tests

-- End of file.
