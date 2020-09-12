module SumCancellation where

import           Either

sc0, sc0', sc1, sc1' :: Int

sc0  = (Either.either (+10) (*10) . Left)  10
sc0' =                (+10)                10

--  20

sc1  = (Either.either (+10) (*10) . Right) 10
sc1' =                      (*10)          10

-- 100

-- End

