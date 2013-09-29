{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Sep 29 (Sun) 09:02:57 by carr.
-}

module AssertError where

import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.HUnit.Base  ((~?=), Test(TestCase, TestList))
import Test.HUnit.Text (runTestTT)
import Test.HUnit.Tools (assertRaises) -- cabal install testpack

-- http://stackoverflow.com/questions/13350164/how-do-i-test-for-an-error-in-haskell
instance Eq ErrorCall where
    x == y = (show x) == (show y)
assertError msg ex f =
    assertRaises msg (ErrorCall ex) $ evaluate f

-- End of file.

