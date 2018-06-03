{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Run (run) where

import qualified RIO.Text as T
import qualified Text.Show.Pretty as SP
import Import
import qualified System.Etc as Etc

run :: Etc.Config -> RIO App ()
run x = do
  logInfo "We're inside the application!"
  logInfo (display (T.pack (SP.ppShow x)))
