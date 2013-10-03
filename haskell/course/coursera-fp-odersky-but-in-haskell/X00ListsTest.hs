{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Sep 29 (Sun) 09:03:04 by carr.
-}

import Test.HUnit
import AssertError
import X00Lists

tests = TestList
    [TestCase $ assertEqual "sum' empty"                   0  (sum' [])
    ,TestCase $ assertEqual "sum' pos"                     3  (sum' [1,2,0])
    ,TestCase $ assertEqual "sum' neg"                   (-6) (sum' [-1,-2,-3])
    ,TestCase $ assertEqual "sum' neg/pos"                 0  (sum' [-1,1,-2,2,-3,3,-4,4])
    ,TestCase $ assertError "max' empty" "NoSuchElement"      (max' [])
    ,TestCase $ assertEqual "max' pos"                     7  (max' [3, 7, 2])
    ,TestCase $ assertEqual "max' neg"                   (-1) (max' [-1,-2,-3])
    ,TestCase $ assertEqual "max' neg/pos"                 4  (max' [-1,1,-2,2,-3,3,-4,4])
    ]

main = runTestTT tests

-- End of file.
