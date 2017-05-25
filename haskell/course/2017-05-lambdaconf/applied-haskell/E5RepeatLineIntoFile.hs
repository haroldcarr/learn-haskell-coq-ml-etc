#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
module E5RepeatLineIntoFile where

import           Control.Monad         (replicateM_)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BSC8
import           Data.Monoid           ((<>))
import           System.IO

e5 = main

main :: IO ()
main = do
  let nameOut = "/tmp/JUNK/E5RepeatLineIntoFile"
      line    = BSC8.pack (['A' .. 'Z'] <> ['\n'])
  withBinaryFile nameOut WriteMode $ \hOut ->
    replicateM_ 1000 $ B.hPutStr hOut line
