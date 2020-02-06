module Lib where

import Control.Arrow
import Data.Char
import Data.List

main :: (String -> Bool) -> IO ()
main test = interact $ lines >>> solve test >>> reverse >>> unlines

solve :: (String -> Bool) -> [String] -> [String]
solve test = foldl' (\answer word -> if test word then word:answer else answer) []

mainWords :: IO ()
mainWords  = main isPalindrome

mainSentences :: IO ()
mainSentences = main (isPalindrome . filter (not . isSpace))

isPalindrome :: String -> Bool
isPalindrome word = lower word == reverse (lower word)

lower :: String -> String
lower  = map toLower

