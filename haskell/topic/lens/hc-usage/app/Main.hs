module Main where

import Lib

main :: IO ()
main = do
  hf     <- rHasFoo
  hfhb   <- rHasFooHasBar
  hfhbhx <- rHasFooHasBarHasX
  hy     <- rHasY
  hy'    <- rHasY'
  hy''   <- rHasY''
  print hf
  print hfhb
  print hfhbhx
  print hy
  print hy'
  print hy''
