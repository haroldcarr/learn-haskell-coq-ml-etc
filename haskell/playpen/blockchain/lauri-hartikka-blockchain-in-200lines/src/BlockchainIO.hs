{-# LANGUAGE OverloadedStrings #-}

-- https://medium.com/@lhartikk/a-blockchain-in-200-lines-of-code-963cc1cc0e54#.ufqde5iq6

module BlockchainIO where

import           Data.Text
import           Data.Thyme.Time

getDate :: IO Text
getDate = do
  now    <- getCurrentTime
  myzone <- getCurrentTimeZone
  let x   = show (utcToZonedTime myzone now :: ZonedTime)
  return (pack x)
