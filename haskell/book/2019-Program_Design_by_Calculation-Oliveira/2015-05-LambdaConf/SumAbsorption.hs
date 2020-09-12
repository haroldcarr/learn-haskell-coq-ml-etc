module SumAbsorption where

import           Either
import           Sum

sumAbsorptionLeft, sumAbsorptionRight :: (d -> c) -> (e -> c) -> (a -> d) -> (b -> e) -> Either a b -> c

sumAbsorptionLeft  g h i j = Either.either g h . Sum.sum i j

sumAbsorptionRight g h i j = Either.either (g . i) (h . j)

sal0,sal1,sar0,sar1 :: String
sal0 = sumAbsorptionLeft  (++" g::d->c ") (++ " h:e->c ") (++" i::a->d ") (++" j::b->e ")   (Left  "foo")
sal1 = sumAbsorptionLeft  (++" g::d->c ") (++ " h:e->c ") (++" i::a->d ") (++" j::b->e ")   (Right "foo")
sar0 = sumAbsorptionRight (++" g::d->c ") (++ " h:e->c ") (++" i::a->d ") (++" j::b->e ")   (Left  "foo")
sar1 = sumAbsorptionRight (++" g::d->c ") (++ " h:e->c ") (++" i::a->d ") (++" j::b->e ")   (Right "foo")

-- End

