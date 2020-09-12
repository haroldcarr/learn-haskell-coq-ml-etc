module Either where

import           Control.Arrow

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  a) = f a
either _ g (Right b) = g b

e0,e1,e2,e3 :: Integer
e0 = ((*11)     `Either.either`     (+1))   (Left  9)
e1 = ((*11)     `Either.either`     (+1))   (Right 1)
e2 = (arr (*11)  |||            arr (+1))   (Left  9)
e3 = (arr (*11)  |||            arr (+1))   (Right 1)

-- => 99

-- End

