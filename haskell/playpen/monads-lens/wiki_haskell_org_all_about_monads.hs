{-# LANGUAGE OverloadedStrings #-}
{-
:set -XOverloadedStrings
-}

import           Control.Monad (MonadPlus (..))
import           Data.Text

{-
Created       : 2015 Aug 15 (Sat) 09:41:08 by Harold Carr.
Last Modified : 2015 Aug 15 (Sat) 13:15:38 by Harold Carr.

https://wiki.haskell.org/All_About_Monads
http://web.archive.org/web/20061211101052/http://www.nomaware.com/monads/html/index.html

Monads
- sequential computations
- monad determines how combined computations form a new computation
- frees programmer coding the combination manually

Monads : useful for structuring functional programs.
- Modularity
  - computations composed from other computations
  - separate combination strategy from computations
- Flexibility
  - programs more adaptable than programs written without monads.
  - because monad puts computational strategy in a single
    place (instead of distributed in entire program)
- Isolation
  - imperative-style structures isolated from main program.

-- the type of monad m
data m a = ...

-- return is a type constructor that creates monad instances
return :: a -> m a

-- combines a monad instance 'm a' with a computation 'a -> m b'
-- to produce another monad instance 'm b'
(>>=) :: m a -> (a -> m b) -> m b

Container analogy
- type constructor 'm' is container that can hold different values 'a'
- 'm a' is container holding value of type 'a'
- 'return' puts value into monad container
- >>= takes value from monad container, passes it a function
  to produce a monad container containing a new value, possibly of a different type.
  - binding function can implement strategy for combining computations in the monad.
-}

type Sheep = Text

father :: Sheep -> Maybe Sheep
father "Julian" = Just "Harold"
father "Harold" = Just "Venice"
father "Venice" = Just "Willard"
father "Leslie" = Just "Robert"
father "Cheryl" = Just "Cheryl father"
father _        = Nothing

mother :: Sheep -> Maybe Sheep
mother "Julian" = Just "Leslie"
mother "Leslie" = Just "Cheryl"
mother _        = Nothing

maternalGF1 :: Sheep -> Maybe Sheep
maternalGF1 s = case mother s of
                    Nothing -> Nothing
                    Just m  -> father m

-- maternalGF1 "Julian"
-- maternalGF1 "Harold"

momsPaternalGF1 :: Sheep -> Maybe Sheep
momsPaternalGF1 s = case mother s of
                        Nothing -> Nothing
                        Just m  -> case father m of
                                       Nothing -> Nothing
                                       Just gf -> father gf
{-
List monad enables computations that can return 0, 1, or more values.

(>>=)     :: Monad m => m a -> (a -> m b) -> m b
(=<<)     :: Monad m => (a -> m b) -> m a -> m b
concatMap ::            (a -> [b]) -> [a] -> [b]
-}

listEx = [1,2,3] >>= \x -> [x + 1]
-- => [2,3,4]

{-
Maybe monad
- combining computations that may not return values
[] monad
- combining computations that can return 0, 1, or more values

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad Maybe where
    Nothing  >>= f = Nothing
    (Just x) >>= f = f x
    return         = Just

-}

maternalGF2 :: Sheep -> Maybe Sheep
maternalGF2 s = mother s >>= father

-- maternalGF2 "Julian"
-- maternalGF2 "Harold"

dadsMaternalGF2 :: Sheep -> Maybe Sheep
dadsMaternalGF2 s = father s >>= mother >>= mother

father3 :: Sheep -> [Sheep]
father3 "Julian" = ["Harold"]
father3 "Harold" = ["Venice"]
father3 "Venice" = ["Willard"]
father3 "Leslie" = ["Robert"]
father3 "Cheryl" = ["Cheryl father3"]
father3 _        = []

mother3 :: Sheep -> [Sheep]
mother3 "Julian" = ["Leslie"]
mother3 "Leslie" = ["Cheryl"]
mother3 _        = []

maternalGF3 :: Sheep -> [Sheep]
maternalGF3 s = mother3 s >>= father3

-- maternalGF3 "Julian"
-- maternalGF3 "Harold"

dadsMaternalGF3 :: Sheep -> [Sheep]
dadsMaternalGF3 s = father3 s >>= mother3 >>= mother3

{-
'do' notation resembles imperative language
- computation built from sequence of computations

Monad laws not enforced by Haskell compiler: programmer must ensure.
Laws ensures semantics of do-notation consistent.
- (return x) >>= f == f x
  - return is left-identity for >>=
- m >>= return     == m
  - return is right-identity for >>=
- (m >>= f) >>= g  == m >>= (\x -> f x >>= g)
- >>= is associative

No way to get values out of monad as defined in Monad class (on purpose).
Specific monads might provide such functions (e.g., 'fromJust' or pattern-matching '(Just x)')

One-way monads
- values enter monad via 'return'
- computations performed within monad via '>>='
- but can't get values out of monad.
  - e.g., IO monad
- enables "side-effects" in monadic operations but prevent them escaping to rest of program


Common pattern
- represent monadic values as functions
- when value of monadic computation required, "run" monad to provide the answer.

MonadPlus

Some monads obey additional laws
- mzero >>= f         == mzero
- m >>= (\x -> mzero) == mzero
- mzero `mplus` m     == m
- m `mplus` mzero     == m
(i.e., mzero/0, mplus/+, >>=/×)

class (Monad m) => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus Maybe where
    mzero             = Nothing
    Nothing `mplus` x = x
    x `mplus` _       = x

Identifies Nothing as the zero value.
Adding two Maybe values gives first value that is not Nothing

[] monad : mzero/empty list, mplus/++

'mplus' combines two monadic values into single monadic value
-}

parent3 :: Sheep -> [Sheep]
parent3 s = mother3 s `mplus` father3 s

-- parent3 "Julian"
-- parent3 "Harold"

{-
EXERCISES
./X_02_example.hs
-}

-- End of file.

