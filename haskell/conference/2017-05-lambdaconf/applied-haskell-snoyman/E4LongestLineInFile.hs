#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module E4LongestLineInFile where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BSC8

e4 = main

main :: IO ()
main = do
  let nameIn  = "E4LongestLineInFile.hs"
  contents <- B.readFile nameIn
  print (maximum (fmap B.length (BSC8.lines contents)))
