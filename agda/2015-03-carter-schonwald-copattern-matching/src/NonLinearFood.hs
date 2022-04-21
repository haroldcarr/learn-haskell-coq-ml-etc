{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module NonLinearFood where

-- https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/d117b1k?utm_source=share&utm_medium=web2x&context=3
-- https://gist.github.com/cartazio/3873e805ad288e52ece7

------------------------------------------------------------------------------

newtype CoData con = CoData { unCoData :: forall r. con r -> r }

data Full where
  Eaten :: Full

data Food r where
  Have :: Food (CoData Food)
  Eat  :: Food Full

biscuit :: CoData Food
biscuit  = CoData $ \case
  Have -> biscuit
  Eat  -> Eaten

eat  :: CoData Food -> Full
eat  (CoData f) = f Eat

have :: CoData Food -> CoData Food
have (CoData f) = f Have

endOfDinner :: Full
endOfDinner  = eat (have (have (have biscuit)))

------------------------------------------------------------------------------

data Gone where
  Done :: Gone
  deriving (Eq, Show)

data Edible r where
  TakeABite :: Int -> Edible (CoData Edible)
  FinishIt  ::        Edible Gone
  WhatsLeft ::        Edible Int

mkEdible :: Int -> CoData Edible
mkEdible available = CoData $ \case
  TakeABite howBig -> mkEdible (max 0 (available - howBig))
  FinishIt         -> Done
  WhatsLeft        -> available

takeABite :: Int -> CoData Edible -> CoData Edible
takeABite i (CoData f) = f (TakeABite i)

finishIt  :: CoData Edible -> Gone
finishIt    (CoData f) = f FinishIt

whatsLeft :: CoData Edible -> Int
whatsLeft   (CoData f) = f WhatsLeft

eatItAll :: Gone
eatItAll  = finishIt (takeABite 25 (takeABite 25 (takeABite 25 (mkEdible 100))))

------------------------------------------------------------------------------


