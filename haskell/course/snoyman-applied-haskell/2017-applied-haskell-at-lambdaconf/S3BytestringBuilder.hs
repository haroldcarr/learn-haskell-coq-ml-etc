#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S3BytestringBuilder where

import qualified Data.ByteString.Builder as BB
import           Data.Monoid             ((<>))
import           System.IO               (stdout)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

s3 = main

main = BB.hPutBuilder stdout $ foldr
  (\i rest -> BB.intDec i <> "\n" <> rest)
  mempty
  (take 5 fibs)
