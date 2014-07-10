{-
Created       : 2014 Jul 09 (Wed) 13:18:07 by Harold Carr.
Last Modified : 2014 Jul 10 (Thu) 00:50:24 by Harold Carr.
-}

module Validation where

-- This contains the "Validating Credit Card Numbers" functions (exact same thing in Brent Yorgey's course).
import           HW01_HC

import           Data.List       (intercalate, isPrefixOf)
import           Data.List.Split (chunksOf)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Reading and Showing Credit Card Numbers

-- Ex. 5.

readCC :: String -> Integer
readCC = read . filter (/= ' ')

-- Ex. 6.

showCC :: Integer -> String
showCC i =
    let ds = show i
        zs = replicate (16 - (length ds)) '0'
    in intercalate " " (chunksOf 4 (zs ++ ds))

------------------------------------------------------------------------------
-- Identifying Credit Card Type

rawData :: [String]
rawData = [ "34 15 American Express"
          , "37 15 American Express"
          , "560221 16 Bankcard"
          , "6011 16 Discover Card"
          , "65 16 Discover Card"
          , "51 16 Master Card"
          , "52 16 Master Card"
          , "4 13 Visa"
          , "4 16 Visa"
          , "417500 16 Visa Electron"
          ]

-- lookupIssuer :: Integer -> String
lookupIssuer cardNum = lu (show cardNum) rawData
  where
    lu _ []      = "Unknown"
    lu cn (x:xs) = if check cn (words x) then x else lu cn xs
    check cn (prefix:n:_) = isPrefixOf prefix cn && length cn == read n

------------------------------------------------------------------------------

t0 :: T.Test
t0 = T.TestList
    [
      U.teq "t000" (readCC "4012 8888 8888 1881") 4012888888881881
    , U.teq "t001" (showCC 4012888888881881) "4012 8888 8888 1881"
    , U.teq "t002" (lookupIssuer (4012888888881881::Int)) "4 16 Visa"
    ]

v :: IO T.Counts
v = T.runTestTT t0

-- End of file.
