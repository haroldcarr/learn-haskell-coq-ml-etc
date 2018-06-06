module Main where

import System.IO as SIO
------------------------------------------------------------------------------
import X01_LedgerDirect
import X02_LedgerWithPool

main :: IO ()
main = loop
 where
  loop = do
    putStrLn "1: direct"
    putStrLn "2: with pool"
    putStr "> "
    SIO.hFlush SIO.stdout
    l <- getLine
    case l of
      "1" -> runLedgerDirect
      "2" -> runLedgerWithPool
      _   -> loop
