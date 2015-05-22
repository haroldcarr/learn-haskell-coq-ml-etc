module ProductFusion where

import           Data.Char (digitToInt)
import           Products

productFusionLeft,productFusionRight :: (c -> a) -> (c -> b) -> (d -> c) -> d -> (a, b)
productFusionLeft  g h f = pair g h . f
productFusionRight g h f = pair (g . f) (h . f)

pf = [
       productFusionLeft  (*2)        show   digitToInt   '3'
     , pair  (*2)                     show . digitToInt $ '3'
     , pair  (*2)                     show                 3
     ,      ((*2)                3,   show                 3)

     , productFusionRight (*2)        show digitToInt     '3'
     , pair ((*2) . digitToInt)      (show . digitToInt)  '3'
         , (((*2) . digitToInt) '3', (show . digitToInt)  '3')
         , (((*2)                3), (show                 3))
     ]

--  (6,"3")

-- End
