module SumAbsorption where

import           Either
import           Sum

sumAbsorptionLeft, sumAbsorptionRight :: (d -> c) -> (e -> c) -> (a -> d) -> (b -> e) -> Either a b -> c

sumAbsorptionLeft  g h i j = (Either.either g h) . (Sum.sum i j)

sumAbsorptionRight g h i j = Either.either (g . i) (h . j)

-- End

