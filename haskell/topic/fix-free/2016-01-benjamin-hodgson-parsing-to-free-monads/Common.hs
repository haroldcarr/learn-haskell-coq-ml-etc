{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString.Char8            as BSC

parseFully :: Result r -> Either String (String, r)
parseFully r0 = case handlePartial r0 of
  Fail u ctxs msg -> Left  (BSC.unpack u ++ " " ++ show ctxs ++ " " ++ msg)
  Done u r        -> Right (BSC.unpack u, r)
  _               -> error "impossible"
 where
  handlePartial r = case r of
    Partial f  -> f ""       -- tell the parser there is no more input
    failOrDone -> failOrDone

