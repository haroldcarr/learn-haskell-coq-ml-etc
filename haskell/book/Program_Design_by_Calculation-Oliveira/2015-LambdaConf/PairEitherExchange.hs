module PairEitherExchange where

import           Either
import           Products

peExchangeLeft, peExchangeRight ::
   (a -> b) -> (a -> d) -> (c -> b) -> (c -> d)
   -> Either a c
   -> (b,  d)

peExchangeLeft  f g h k = Either.either (pair          f g) (pair          h k)
peExchangeRight f g h k = pair          (Either.either f h) (Either.either g k)

pel0,pel1,per0,per1 :: (String, String)
pel0 = peExchangeLeft  (++" f::a->b ") (++" g::a->d ") (++" h::c->b ") (++" k::c->d ") (Left "foo")
pel1 = peExchangeLeft  (++" f::a->b ") (++" g::a->d ") (++" h::c->b ") (++" k::c->d ") (Right "foo")
per0 = peExchangeRight (++" f::a->b ") (++" g::a->d ") (++" h::c->b ") (++" k::c->d ") (Left "foo")
per1 = peExchangeRight (++" f::a->b ") (++" g::a->d ") (++" h::c->b ") (++" k::c->d ") (Right "foo")

-- End
