{-# LANGUAGE NoImplicitPrelude #-}

-- | Demonstrates how to write a test case that tests this module.
module Util
  ( plus2
  )
where

import           RIO

plus2 :: Int -> Int
plus2 = (+ 2)
