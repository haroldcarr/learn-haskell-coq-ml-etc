module SumFunctor where

import           Sum

sumFunctorLeft, sumFunctorRight :: (e -> c) -> (a -> e) -> (f -> d) -> (b -> f) -> Either a b -> Either c d
sumFunctorLeft  g h i j = Sum.sum (g . h) (i . j)
sumFunctorRight g h i j = (Sum.sum g i) . (Sum.sum h j)

-- End
