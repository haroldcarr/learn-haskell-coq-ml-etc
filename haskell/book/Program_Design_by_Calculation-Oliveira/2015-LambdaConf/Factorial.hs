module Factorial where

f :: Integer -> Integer
f 0 = 1
f n = f (n - 1) * n

-- End
