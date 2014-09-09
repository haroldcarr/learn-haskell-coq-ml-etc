{-
Created       : 2014 Sep 09 (Tue) 08:35:39 by Harold Carr.
Last Modified : 2014 Sep 09 (Tue) 08:35:54 by Harold Carr.

http://c2.com/cgi/wiki?FizzBuzzTest
-}

import           Control.Applicative
import           Data.Maybe

fizzbuzz = [ x | i <- [ 1 .. 100 ], let x = fromJust (both i <|> fizz i <|> buzz i <|> Just (show i))]
  where
    fb n s i | i `mod` n == 0 = Just s
             | otherwise      = Nothing
    fizz     = fb 3 "fizz"
    buzz     = fb 5 "buzz"
    both   i = if isJust (fizz i) && isJust (buzz i) then Just "fizzbuzz" else Nothing

-- End of file.
