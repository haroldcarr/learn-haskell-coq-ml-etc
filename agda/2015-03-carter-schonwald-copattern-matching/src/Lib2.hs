{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}

module Lib2 where

-- https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/d117nbc?utm_source=share&utm_medium=web2x&context=3
-- https://gist.github.com/ChristopherKing42/bb61e60c488b22b119ad

data Stream a res where
  Head :: Stream a a
  Tail :: Stream a (CoData (Stream a))

newtype CoData con = CoData {unCoData :: forall r. con r -> r}

head :: CoData (Stream r) -> r
head (CoData f) = f Head

tail :: CoData (Stream a) -> CoData (Stream a)
tail (CoData f) = f Tail

appendList :: [a] -> CoData (Stream a) -> CoData (Stream a)
appendList    []  s = s
appendList (x:xs) s = CoData $ \case
  Head -> x
  Tail -> appendList xs s

cycle :: [a] -> CoData (Stream a)
cycle xs = let r = appendList xs r in r

toList :: CoData (Stream a) -> [a]
toList (CoData f) = f Head : toList (f Tail)

numFrom :: Num a => a -> CoData (Stream a)
numFrom a = CoData $ \case
  Head -> a
  Tail -> numFrom (a + 1)
