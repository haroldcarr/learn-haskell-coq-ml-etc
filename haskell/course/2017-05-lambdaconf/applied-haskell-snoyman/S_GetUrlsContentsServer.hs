#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

{-# LANGUAGE OverloadedStrings #-}

module S_GetUrlsContentsServer where

import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

main :: IO ()
main = run 3000 $ logStdoutDev
                $ \_req send -> send $ responseBuilder status200 [] "Hello World"
