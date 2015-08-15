{-# LANGUAGE OverloadedStrings #-}
{-
:set -XOverloadedStrings
-}

import           Control.Monad (MonadPlus (..), mapM, mapM_, sequence,
                                sequence_)
import           Data.Text     hiding (foldr)

{-
Created       : 2015 Aug 15 (Sat) 09:41:08 by Harold Carr.
Last Modified : 2015 Aug 15 (Sat) 14:31:28 by Harold Carr.

https://wiki.haskell.org/All_About_Monads
http://web.archive.org/web/20061211101052/http://www.nomaware.com/monads/html/index.html

1.1 What is a monad?

Monads
- sequential computations
- monad determines how combined computations form a new computation
- frees programmer coding the combination manually

1.2 Why should I make the effort to understand monads?

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

2 Meet the Monads

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

2.3 An example
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
2.4 List is also a monad

List monad enables computations that can return 0, 1, or more values.

(>>=)     :: Monad m => m a -> (a -> m b) -> m b
(=<<)     :: Monad m => (a -> m b) -> m a -> m b
concatMap ::            (a -> [b]) -> [a] -> [b]
-}

listEx = [1,2,3] >>= \x -> [x + 1]
-- => [2,3,4]

{-
2.5 Summary

Maybe monad
- combining computations that may not return values
[] monad
- combining computations that can return 0, 1, or more values

3.2 The Monad class

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

3.3 Example continued

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
3.4 Do notation

'do' notation resembles imperative language
- computation built from sequence of computations

4 The monad laws

Monad laws not enforced by Haskell compiler: programmer must ensure.
Laws ensures semantics of do-notation consistent.
- (return x) >>= f == f x
  - return is left-identity for >>=
- m >>= return     == m
  - return is right-identity for >>=
- (m >>= f) >>= g  == m >>= (\x -> f x >>= g)
- >>= is associative

4.3 No way out

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

4.4 Zero and Plus

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
5 Exercises

./X_02_example.hs

6 Monad support in Haskell

6.1.2 The sequencing functions

-- givenlist of monadic computations
-- executes each one in turn
-- returns list of results
-- If any computation fails, then the whole function fails:
sequence :: Monad m => [m a] -> m [a]
sequence = foldr mcons (return [])
  where mcons p q = p >>= \x -> q >>= \y -> return (x:y)
-}

seqExM :: Maybe [Integer]
seqExM = sequence [(Just 1), (Just 2)]

seqExL :: [[Integer]]
seqExL = sequence [[     1], [     2]]

{-          mcons
           /     \
          1       mcons
                 /     \
                2       return []

-- same behavior but does not return list of results
-- useful for side-effects
sequence_HC :: Monad m => [m a] -> m ()
sequence_HC = foldr (>>) (return ())
-}

seq_ExM :: IO ()
seq_ExM = sequence_ [print 1, print 2]

{-
            >>
           /  \
    print 1    >>
              /  \
       print 2    return ()
-}

{-
6.1.3 The mapping functions

-- maps monadic computation over list of values
-- returns list of results
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f as = sequence_ (map f as)

Example:

putString :: [Char] -> IO ()
putString s = mapM_ putChar s

Common pattern: mapM used in a do block, similar to map on lists.

-- compare the non-monadic and monadic signatures
map  ::            (a ->   b) -> [a] ->   [b]
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-}

mapMExM :: Maybe [Integer]
mapMExM = mapM (Just) [1,2,3]

mapM_ExM :: IO ()
mapM_ExM = mapM_ (print) [1,2,3]

{-
6.2.2 Monadic versions of list functions

-}

-- End of file.

