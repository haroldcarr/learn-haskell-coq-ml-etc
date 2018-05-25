{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Text.IO      as T
import           TextShow
------------------------------------------------------------------------------
import           Ex
import           XX

main :: IO ()
main = do
  let x = showt (f (Example 1 2))
  evaluate x
  T.putStrLn "trace output will appear before this line"
  T.putStrLn x
