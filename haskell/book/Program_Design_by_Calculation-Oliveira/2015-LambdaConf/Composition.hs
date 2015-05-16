module Composition where

c :: Num a => a -> a
c = (+10) . (*99)

-- End
