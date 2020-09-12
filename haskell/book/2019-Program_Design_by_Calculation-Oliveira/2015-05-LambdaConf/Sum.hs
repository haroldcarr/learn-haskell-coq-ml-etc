module Sum where

import           Control.Arrow
import           Either

sum :: (a -> c) -> (b -> d) -> Either a b -> Either c d
sum f g  = Either.either (Left . f) (Right . g)

s0,s1,s2 :: Either Int Int
s0 = (    (*11) `Sum.sum`      (+1))   (Left  9)
s1 = (arr (*11)  +++       arr (+1))   (Left  9)
s2 = (arr (*11)  +++       arr (+1))   (Right 9)

-- (Left 99)

-- End

