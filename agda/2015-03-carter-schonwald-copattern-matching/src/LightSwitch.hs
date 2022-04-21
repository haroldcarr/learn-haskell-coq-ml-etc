{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module LightSwitch where

-- https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/d117b1k?utm_source=share&utm_medium=web2x&context=3
-- https://gist.github.com/cartazio/a7f8c7adf2c6c68adcfb

newtype CoData con = CoData { unCoData :: forall r. con r -> r }

data Light r where
  IsLit      :: Light Bool           -- head
  SwitchFlip :: Light (CoData Light) -- tail

alternates :: Bool -> CoData Light
alternates b = CoData $ \case
  IsLit      -> b
  SwitchFlip -> alternates (not b)

isLit :: CoData Light -> Bool
isLit (CoData f) = f IsLit

switchFlip :: CoData Light -> CoData Light
switchFlip (CoData f) = f SwitchFlip

currentState :: Bool
currentState = isLit (switchFlip (switchFlip (switchFlip (alternates True))))
