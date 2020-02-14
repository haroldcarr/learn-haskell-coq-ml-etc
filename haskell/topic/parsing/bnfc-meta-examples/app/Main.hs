module Main where

import qualified JavaLight.UseJll as JL
import qualified Typesafe.Example as TS

main :: IO ()
main = do
  JL.main
  TS.main

