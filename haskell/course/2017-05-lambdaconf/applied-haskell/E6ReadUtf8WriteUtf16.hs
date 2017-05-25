#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module E6ReadUtf8WriteUtf16 where

import qualified Data.ByteString    as B
import           Data.Text.Encoding as TE

e6 = main

main :: IO ()
main = do
  let nameIn   = "E6ReadUtf8WriteUtf16.hs"
  let nameOut  = "/tmp/JUNK/E6ReadUtf8WriteUtf16"
  contents    <- B.readFile nameIn
  let utf8In   = TE.decodeUtf8 contents
  let utf16Out = TE.encodeUtf16LE utf8In
  B.writeFile nameOut utf16Out
