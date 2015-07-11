module Product where

import           Control.Arrow
import           Products

product :: (c -> a) -> (d -> b) -> (c,d) -> (a,b)
product f g = pair (f . fst) (g . snd)

pr0,pr1 :: (Integer, String) -> (String, Integer)
pr0 =     (show . (*2)) `Product.product`     (read . (++"1"))
pr1 = arr (show . (*2)) ***               arr (read . (++"1"))

pr0' = pr0 (2,"6")
pr1' = pr1 (2,"6")

-- End
