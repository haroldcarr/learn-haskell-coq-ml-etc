module NonLinearFood where

-- https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/d117b1k?utm_source=share&utm_medium=web2x&context=3
-- https://gist.github.com/cartazio/3873e805ad288e52ece7

data Full : Set where
  Eaten : Full

record Food : Set where
  coinductive
  field
    have : Food
    eat  : Full
open Food

biscuit : Food
have biscuit = biscuit
eat  biscuit = Eaten

endOfDinner : Full
endOfDinner = eat (have (have (have biscuit)))
