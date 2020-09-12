module ProductFunctor where

import           Product

productFunctorLeft  :: (e -> a) -> (c -> e) -> (f -> b) -> (d -> f) -> (c, d) -> (a, b)
productFunctorLeft  g h i j = Product.product (g . h) (i . j)

productFunctorRight :: (e -> a) -> (c -> e) -> (f -> b) -> (d -> f) -> (c, d) -> (a, b)
productFunctorRight g h i j = Product.product g i . Product.product h j

e0,e1 :: (Int, Double)
e0 = productFunctorLeft  (+2) (+4) (+6.0) (+8.0)       (1,100.0)
e1 = productFunctorRight (+2) (+4) (+6.0) (+8.0)       (1,100.0)

-- (7,114.0)

-- End
