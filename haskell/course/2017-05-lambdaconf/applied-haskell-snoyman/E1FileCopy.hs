#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module E1FileCopy where

import           Control.Monad   (unless)
import qualified Data.ByteString as B
import           Data.Function   (fix)
import           System.IO

e1 = main

main :: IO ()
main = do
  let nameIn  = "E1FileCopy.hs"
  let nameOut = "/tmp/JUNK/JUNK"
  withBinaryFile nameIn ReadMode $ \hIn ->
    withBinaryFile nameOut WriteMode $ \hOut ->
      fix $ \loop -> do
        bs <- B.hGetSome hIn 4096
        unless (B.null bs) $ do
          B.hPut hOut bs
          loop
