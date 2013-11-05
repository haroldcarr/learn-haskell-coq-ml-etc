{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Nov 04 (Mon) 21:14:08 by carr.
-}

import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util
import X00Lists

tests = TestList
    [teq "sum' empty"   (sum' [])                      0
    ,teq "sum' pos"     (sum' [1,2,0])                 3
    ,teq "sum' neg"     (sum' [-1,-2,-3])              (-6)
    ,teq "sum' neg/pos" (sum' [-1,1,-2,2,-3,3,-4,4])   0
    ,ter "max' empty"   (max' [])                      "NoSuchElement"
    ,teq "max' pos"     (max' [3, 7, 2])               7
    ,teq "max' neg"     (max' [-1,-2,-3])              (-1)
    ,teq "max' neg/pos" (max' [-1,1,-2,2,-3,3,-4,4])   4
    ]

main = runTestTT tests

-- End of file.
