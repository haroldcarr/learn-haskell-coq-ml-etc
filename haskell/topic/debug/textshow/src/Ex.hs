module Ex where

import           Data.Semigroup
import qualified TextShow             as TS

data Example = Example Int Int

instance TS.TextShow Example where
    showb (Example i1 i2) = TS.showb i1 <> TS.showbSpace <> TS.showb i2

{-
import TextShow
showt (Example 2 3)
-- "2 3"
:t showt (Example 2 3)
-- showt (Example 2 3) :: text-1.2.3.0:Data.Text.Internal.Text
-}

