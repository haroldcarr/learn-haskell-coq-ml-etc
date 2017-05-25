#!/usr/bin/env stack
-- stack --resolver lts-8.12 --install-ghc runghc --package typed-process

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}
{-# LANGUAGE OverloadedStrings #-}

module S7TypedProcess where

import           Control.Concurrent.STM     (atomically)
import qualified Data.ByteString.Lazy.Char8 as L8
import           System.IO                  (hClose, hPutStr)
import           System.Process.Typed       (byteStringOutput, createPipe,
                                             getStdin, getStdout, proc,
                                             readProcess_, runProcess,
                                             runProcess_, setStdin, setStdout,
                                             withProcess_)
s7 = main

main :: IO ()
main = do
    -- Run a process, print its exit code
    runProcess "true"  >>= print
    runProcess "false" >>= print

    -- Check that the exit code is a success
    runProcess_ "true"
    -- This will throw an exception: runProcess_ "false"

    -- Capture output and error
    (dateOut, dateErr) <- readProcess_ "date"
    print (dateOut, dateErr)

    -- Use shell commands
    (dateOut2, dateErr2) <- readProcess_ "date >&2"
    print (dateOut2, dateErr2)

    -- Interact with a process
    let catConfig = setStdin createPipe
                  $ setStdout byteStringOutput
                  $ proc "cat" ["/etc/hosts", "-", "/etc/group"]
    withProcess_ catConfig $ \p -> do
        hPutStr (getStdin p) "\n\nHELLO\n"
        hPutStr (getStdin p) "WORLD\n\n\n"
        hClose (getStdin p)

        atomically (getStdout p) >>= L8.putStr
