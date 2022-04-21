{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

{-
https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/
-}

-- a   : param list
-- res : result type
data StreamTag a res where
  HeadTag :: StreamTag a         a
  TailTag :: StreamTag a (Stream a)

newtype Stream a = Stream  { unStream  :: forall res . StreamTag a res -> res }

headStream:: Stream a -> a
headStream (Stream f) = f HeadTag

tailStream :: Stream a -> Stream a
tailStream (Stream f) = f TailTag

zipWithStream :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithStream f sa sb = Stream $ \case
  HeadTag ->               f (headStream sa) (headStream sb)
  TailTag -> zipWithStream f (tailStream sa) (tailStream sb)

numFrom :: Num a => a -> Stream a
numFrom a = Stream $ \case
  HeadTag ->          a
  TailTag -> numFrom (a + 1)

------------------------------------------------------------------------------

-- StreamModel usually written as "Stream" in haskell,
-- though is not ideal for a variety of reasons.
-- StreamModel type is only included for simplicity.
data StreamModel a = StreamModel { head :: a , tail :: StreamModel a}

-- from Stream (the coinductive definition)
-- to StreamModel (the usual stream definition in haskell)
stream2StreamModel :: Stream a -> StreamModel a
stream2StreamModel s = StreamModel (headStream s) (stream2StreamModel s)



