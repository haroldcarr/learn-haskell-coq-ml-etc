module Products where

import           Control.Arrow
import           Isomorphisms

-- cartesian product of types
pair :: (c -> a) -> (c -> b) -> c -> (a, b)
pair f g c = (f c, g c)

split = pair -- aka

p0,p1 :: (Enum c, Ord c, Show c) => c -> (Seven, String)
p0 =     transform `pair`     show
p1 = arr transform  &&&   arr show

p0' = p0 Sunday
p1' = p1 Sunday

-- End
