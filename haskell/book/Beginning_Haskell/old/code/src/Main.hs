{-
Created       : 2014 Jun 29 (Sun) 19:55:45 by Harold Carr.
Last Modified : 2014 Jul 05 (Sat) 07:34:11 by Harold Carr.
-}

module Main where

import           C02
import           C03
import           C04
import           C05
import           Test.HUnit.Base

main :: IO Counts
main = do
    _ <- c02
    _ <- c03
    _ <- c04
    c05

-- End of file.
