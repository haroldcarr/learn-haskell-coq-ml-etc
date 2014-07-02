{-
Created       : 2014 Jun 29 (Sun) 19:55:45 by Harold Carr.
Last Modified : 2014 Jul 02 (Wed) 04:40:20 by Harold Carr.
-}

module Main where

import           C02
import           C03
import           C04
import           Test.HUnit.Base

main :: IO Counts
main = do
    _ <- c02
    _ <- c03
    c04

-- End of file.
