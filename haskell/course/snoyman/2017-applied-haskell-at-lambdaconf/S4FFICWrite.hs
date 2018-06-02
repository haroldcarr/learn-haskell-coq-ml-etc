#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module S4FFICWrite where

import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Data.Monoid            ((<>))
import qualified Data.Text.Encoding     as TE
import qualified Data.Text.IO           as TIO
import           Foreign.C.Types        (CChar)
import           Foreign.Ptr            (Ptr)

foreign import ccall "write"
    c_write :: Int -> Ptr CChar -> Int -> IO ()

s4 = main

main :: IO ()
main = do
    TIO.putStrLn "What is your name?"
    name <- TIO.getLine
    let msg = "Hello, " <> name <> "\n"
        bs = TE.encodeUtf8 msg
    unsafeUseAsCStringLen bs $ \(ptr, len) ->
        c_write stdoutFD ptr len
  where
    stdoutFD = 1

