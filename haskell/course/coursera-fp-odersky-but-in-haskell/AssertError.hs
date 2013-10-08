{-
Created       : 2013 Sep 28 (Sat) 09:01:51 by carr.
Last Modified : 2013 Oct 07 (Mon) 18:57:49 by carr.
-}

module AssertError where

import Control.Exception (ErrorCall(ErrorCall), evaluate)
import Test.HUnit
import Test.HUnit.Base  ((~?=), Test(TestCase, TestList))
import Test.HUnit.Text (runTestTT)
import Test.HUnit.Tools (assertRaises) -- cabal install testpack

-- http://stackoverflow.com/questions/13350164/how-do-i-test-for-an-error-in-haskell
instance Eq ErrorCall where
    x == y = show x == show y
assertError msg ex f =
    assertRaises msg (ErrorCall ex) $ evaluate f


teq :: (Eq a, Show a) => String -> a -> a      -> Test
teq testName actual expected = TestCase $ assertEqual testName expected actual

ter ::        Show a  => String -> a -> String -> Test
ter testName actual expected = TestCase $ assertError testName expected actual

-- End of file.

