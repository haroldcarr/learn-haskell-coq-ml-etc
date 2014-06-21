{-
Created       : 2014 Jun 04 (Wed) 08:19:48 by Harold Carr.
Last Modified : 2014 Jun 20 (Fri) 21:59:43 by Harold Carr.
-}

module Main where

import           HW01_HC
import           HW02_HC_LogAnalysis
import           HW03_HC_Golf
import           HW04_HC
import           HW05_HC
import           HW06_HC
import           HW07_HC_JoinList
import           HW08_HC
import           HW10_HC_AParser
import           HW11_HC_SExpr
import           Test.HUnit.Base

main :: IO Counts
main = do
    hw01
    hw02
    hw03
    hw04
    hw05
    hw06
    hw07
    hw08
    -- there is no homework 9
    hw10
    hw11

-- End of file.
