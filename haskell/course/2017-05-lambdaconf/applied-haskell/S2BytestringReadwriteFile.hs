#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S2BytestringReadwriteFile where

import qualified Data.ByteString       as B
import           Data.ByteString.Char8 as BSC8 (putStrLn)
import           Data.Monoid           ((<>))
import           Data.Word8            (_space)

s2 = main

main :: IO ()
main = do
  let fp = "/tmp/JUNK/somefile.txt"
  B.writeFile fp $ "Hello " <> "World!"
  contents <- B.readFile fp   -- this is lazy - so if something goes wrong : LEAK
  BSC8.putStrLn $ B.takeWhile (/= _space) contents

