module SumFusion where

import           Either

sumFusionLeft, sumFusionRight :: (c -> d) -> (a -> c) -> (b -> c) -> Either a b -> d
sumFusionLeft  f g h = f . (Either.either g h)
sumFusionRight f g h = Either.either (f . g) (f . h)

-- End
