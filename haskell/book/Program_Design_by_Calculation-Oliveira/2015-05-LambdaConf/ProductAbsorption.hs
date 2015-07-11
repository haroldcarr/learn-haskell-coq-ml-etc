module ProductAbsorption where

import           Product
import           Products

-- left
pal         :: (d -> a) -> (e -> b) -> (c -> d) -> (c -> e) -> c -> (a, b)
pal i j g h = Product.product i j . pair g h

-- right
par         :: (d -> a) -> (e -> b) -> (c -> d) -> (c -> e) -> c -> (a, b)
par i j g h = pair (i . g) (j . h)

pal0,par0 :: (String,Int)
pal0 = pal show read (*2) show     4
par0 = par show read (*2) show     4

-- ("8",4)

-- End
