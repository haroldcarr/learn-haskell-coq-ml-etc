module ProductFusion where

import           Data.Char (digitToInt)
import           Products

pf0 = pair (*2) show . digitToInt                  $ '3'

pf1 = pair ((*2) . digitToInt) (show . digitToInt)  '3'

--  (6,"3")

-- End
