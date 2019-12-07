module Main where

import Lib

main :: IO ()
main = do
  rr  <- r
  rr' <- r'
  print rr
  print rr'
