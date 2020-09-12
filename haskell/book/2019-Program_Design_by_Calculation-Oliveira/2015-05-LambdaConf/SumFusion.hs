module SumFusion where

import           Either

sumFusionLeft, sumFusionRight :: (c -> d) -> (a -> c) -> (b -> c) -> Either a b -> d
sumFusionLeft  f g h = f . Either.either g h
sumFusionRight f g h = Either.either (f . g) (f . h)

sfl,sfl',sfr,sfr' :: Int
sfl  = sumFusionLeft  ((*10) . read)   (show)                  (id)  $  Left   3
sfl' = sumFusionLeft  ((*10) . read)  ((show)::Int -> String)  (id)  $  Right "3"
sfr  = sumFusionRight ((*10) . read)   (show)                  (id)  $  Left   3
sfr' = sumFusionRight ((*10) . read)  ((show)::Int -> String)  (id)  $  Right "3"

-- End
