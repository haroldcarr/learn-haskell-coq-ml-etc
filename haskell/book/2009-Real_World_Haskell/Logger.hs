{-
Created       : 2013 Nov 22 (Fri) 20:26:36 by carr.
Last Modified : 2013 Nov 22 (Fri) 20:59:48 by carr.
-}


module Logger
(
Logger
, Log
, runLogger
, record
) where

import Control.Monad
import Test.HUnit
import Test.HUnit.Util

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log) } deriving (Eq, Show)

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m
                  n      = k a
                  (b, x) = execLogger n
              in Logger (b, w ++ x)

globToRegex :: String -> Logger String
globToRegex cs =
    globToRegex' cs >>= \ds ->
    return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('?':cs) =
    record "any"    >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)
globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)
globToRegex' ('[':_) =
    fail "unterminated character class"
globToRegex' (c:cs) =
    liftM2 (++) (escape c) (globToRegex' cs)

charClass (']':cs) =
    (']':) `liftM` globToRegex' cs
charClass (c:cs) =
    (c:) `liftM` charClass cs

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where
    regexChars = "\\+()^$.{}]|"

tl1 = t "tl1"
      (runLogger (return True :: Logger Bool))
      (True,[])

tl2 = t "tl2"
      (runLogger (record "hi mom!" >> return 3.1337))
      (3.1337,["hi mom!"])

tl3 = t "tl3"
      (runLogger ((return "foo" :: Logger String) >>= \s -> return (length s)))
      (3,[])

tl4 = t "tl4"
      (runLogger (globToRegex "[a-z]?"))
      ("^[a-z].$",["character class","any"])

main = do
    runTestTT $ TestList $ tl1 ++ tl2 ++ tl3 ++ tl4

-- End of file.
