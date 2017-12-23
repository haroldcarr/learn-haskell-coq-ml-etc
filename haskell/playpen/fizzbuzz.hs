{-
Created       : 2014 Sep 09 (Tue) 08:35:39 by Harold Carr.
Last Modified : 2014 Sep 09 (Tue) 09:11:59 by Harold Carr.

http://c2.com/cgi/wiki?FizzBuzzTest
-}

import           Control.Applicative
import           Data.Maybe

fizzbuzz1 = [ fromJust (both i <|> fizz i <|> buzz i <|> Just (show i)) | i <- [ 1 .. 100 ]]
  where
    fb n s i | i `mod` n == 0 = Just s
             | otherwise      = Nothing
    fizz     = fb  3 "fizz"
    buzz     = fb  5 "buzz"
    both     = fb 15 "fizzbuzz"

fizzbuzz2 = [ doit i | i <- [ 1 .. 100 ] ]
  where
    fb n s i | i `mod` n == 0 = Just s
             | otherwise      = Nothing
    fizz     = fb 3 "fizz"
    buzz     = fb 5 "buzz"
    doit   i = case ((fizz i), (buzz i)) of
                   (Just f, Just b) -> f ++ b
                   (Just f,      _) -> f
                   (_     , Just b) -> b
                   _                -> show i

{-
See also:
http://www.haskell.org/haskellwiki/Haskell_Quiz/FizzBuzz/Solution_Acontorer
http://freshbrewedcode.com/calvinbottoms/2012/02/25/fizzbuzz-in-haskell/
http://themonadreader.files.wordpress.com/2014/04/fizzbuzz.pdf
http://dmytrish.wordpress.com/2013/10/10/haskell-fizzbuzz/
https://www.hackerrank.com/challenges/fizzbuzz
-}
-- End of file.
