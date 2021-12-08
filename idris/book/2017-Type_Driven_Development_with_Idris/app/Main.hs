module Main where

import qualified MAlonzo.Code.P139_read_vect                       as P139
import qualified MAlonzo.Code.P236Z45Z257Z45ZpredicatesZ45Zhangman as P236
------------------------------------------------------------------------------
import           System.Environment

main :: IO ()
main = do
  av <- getArgs
  case av of
    ["P139"] -> P139.main
    ["P236"] -> P236.main
    xs       -> error (concat ("unknown: " : xs))
