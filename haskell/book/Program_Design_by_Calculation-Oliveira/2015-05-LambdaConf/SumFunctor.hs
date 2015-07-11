module SumFunctor where

import           Sum

sumFunctorLeft, sumFunctorRight :: (e -> c) -> (a -> e) -> (f -> d) -> (b -> f) -> Either a b -> Either c d
sumFunctorLeft  g h i j = Sum.sum (g . h) (i . j)
sumFunctorRight g h i j = Sum.sum g i . Sum.sum h j

el0,el1,er0,er1 :: Either String String
el0 = sumFunctorLeft  (++" g::e->c ") (++" h::a->e ") (++" i::f->d ") (++" j::b->f ")    (Left  "foo")
el1 = sumFunctorLeft  (++" g::e->c ") (++" h::a->e ") (++" i::f->d ") (++" j::b->f ")    (Right "foo")
er0 = sumFunctorRight (++" g::e->c ") (++" h::a->e ") (++" i::f->d ") (++" j::b->f ")    (Left  "foo")
er1 = sumFunctorRight (++" g::e->c ") (++" h::a->e ") (++" i::f->d ") (++" j::b->f ")    (Right "foo")

-- End
