module ProductAndProjections where

import           Product

e0 = fst . (Product.product show show) $       (3, 4)
e1 = show                              $ fst   (3, 4) -- optimized via 2.26

-- "3"

-- End
