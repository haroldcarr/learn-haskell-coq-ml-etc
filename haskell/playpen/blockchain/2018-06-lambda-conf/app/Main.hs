module Main where

import X01_LockedLedger
import X02_CASLedger

main :: IO ()
main = loop
 where
  loop = do
    putStrLn "1: runDirect"
    putStrLn "2: runPooled"
    l <- getLine
    case l of
      "1" -> runDirectLedger
      "2" -> runPoolLedger
      _   -> loop
