module SumReflexion where

import           Either

sr1  = Either.either Left Right    (Left   10)
sr1' = id                          (Left   10)

-- (Left 10)

sr2  = Either.either Left Right    (Right 100)
sr2' = id                          (Right 100)

-- (Right 100)

-- End
