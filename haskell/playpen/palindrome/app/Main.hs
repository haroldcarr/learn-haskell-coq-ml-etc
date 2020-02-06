module Main where

import qualified Lib
import System.Environment

main :: IO ()
main = do
  av <- getArgs
  case av of
    ("w":_) -> Lib.mainWords
    ("s":_) -> Lib.mainSentences
    _       -> error (concat av)
