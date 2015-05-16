module ProductAbsorption where

import           Product
import           Products

-- non-optimized version
pcp                        :: (d -> a) -> (e -> b) -> (c -> d) -> (c -> e) -> c -> (a, b)
pcp                i j g h = Product.product i j . pair g h

-- optimized version via 2.20
productComposePair         :: (d -> a) -> (e -> b) -> (c -> d) -> (c -> e) -> c -> (a, b)
productComposePair i j g h = pair (i . g) (j . h)

pcp0,pcp1 :: (String,Int)
pcp0 = pcp                show read (*2) show   4
pcp1 = productComposePair show read (*2) show   4

-- ("8",4)

-- End
