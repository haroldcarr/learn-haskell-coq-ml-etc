module Lib where

{-
https://danielbmarkham.com/fun-with-an-interview-question/
1
TODO:
- fix quadratic use of ++
- make output (String, Integer) instead of (Char, Integer)
-}
xx :: String -> ([(Char, Integer)], Maybe (Char, Integer))
xx = foldl go ([], Nothing)
 where
  go (acc, x) c = case x of
    Nothing               -> (acc        , Just (c, 1))
    Just (c',i) | c' == c -> (acc        , Just (c, i + 1))
    Just ci               -> (acc ++ [ci], Just (c, 1))

yy :: ([(Char, Integer)], Maybe (Char, Integer)) -> [(Char, Integer)]
yy (partialAnswer, lastAnswer) = case lastAnswer of
  Nothing -> partialAnswer
  Just l  -> partialAnswer ++ [l]
{-
xx "aaaabbbcca"
yy (xx "aaaabbbcca")

xx "aaaabbbcc"
yy (xx "aaaabbbcc")

xx "abcabdaabbccd"
yy (xx "abcabdaabbccd")
-}
