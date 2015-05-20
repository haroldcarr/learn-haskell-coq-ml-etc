module Product where

import           Control.Arrow
import           Products

product :: (c -> a) -> (d -> b) -> (c,d) -> (a,b)
product f g = pair (f . fst) (g . snd)

pr0, pr1 :: (Integer, String) -> (Integer, String)

pr0 =     (*2) `Product.product`     (++"bar")
pr1 = arr (*2) ***               arr (++"bar")

pr0' = pr0 (2,"foo")
pr1' = pr1 (2,"foo")

-- End
