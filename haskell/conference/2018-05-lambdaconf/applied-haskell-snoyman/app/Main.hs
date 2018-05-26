{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import System.Environment

import AH

main = do
  a <- getArgs
  case a of
    ["a","1"] -> average1
    ["a","2"] -> average2
    ["a","3"] -> average3
    ["a","4"] -> average4
    ["a","5"] -> average5
    ["s","1"] -> runsum1
    ["s","2"] -> runsum2
    ["s","3"] -> runsum3
    ["s","4"] -> runsum4
    _     -> error "***************** bad news ******************"
