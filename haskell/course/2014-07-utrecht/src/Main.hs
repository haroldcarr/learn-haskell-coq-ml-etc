{-
Created       : 2014 Jul 09 (Wed) 12:49:34 by Harold Carr.
Last Modified : 2014 Jul 09 (Wed) 13:26:18 by Harold Carr.
-}

module Main where

import           Assignment1
import           Assignment3
import           Assignment4
import           MasterMind
import           Stereograms
import           Validation

import           Test.HUnit  as T

main :: IO T.Counts
main = do
    a1
    a3
    a4
    mm
    sg
    v

-- End of file.
