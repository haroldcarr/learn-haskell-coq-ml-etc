{-# LANGUAGE OverloadedStrings #-}
{-
:set -XOverloadedStrings
-}

module Wiki_haskell_org_all_about_monads
where

import           Control.Monad      (MonadPlus (..), ap, foldM, liftM, liftM2,
                                     mapM, mapM_, sequence, sequence_, zipWithM,
                                     zipWithM_)
import           Control.Monad.Plus (mfromMaybe)
import           Data.Char          (isSpace)
import           Data.Text          as T hiding (break, dropWhile, foldM, foldl,
                                          foldr, map, tail, words)
import           X_02_example       hiding (parent)

{-
Created       : 2015 Aug 15 (Sat) 09:41:08 by Harold Carr.
Last Modified : 2015 Aug 15 (Sat) 17:50:04 by Harold Carr.

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

maternalGF1 :: Sheep -> Maybe Sheep
maternalGF1 s = case mother s of
                    Nothing -> Nothing
                    Just m  -> father m

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

dadsMaternalGF2 :: Sheep -> Maybe Sheep
dadsMaternalGF2 s = father s >>= mother >>= mother

maternalGF3 :: Sheep -> [Sheep]
maternalGF3 s = mfromMaybe (mother s) >>= mfromMaybe . father

dadsMaternalGF3 :: Sheep -> [Sheep]
dadsMaternalGF3 s = mfromMaybe (father s) >>= mfromMaybe . mother >>= mfromMaybe . mother

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

parent :: Sheep -> [Sheep]
parent s = mfromMaybe (mother s) `mplus` mfromMaybe (father s)

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
seqExM = sequence [Just 1, Just 2]

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
mapMExM = mapM Just [1,2,3]

mapM_ExM :: IO ()
mapM_ExM = mapM_ print [1,2,3]

{-
6.2.2 Monadic versions of list functions

foldM : monadic foldl : monadic computations left-to-right

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM f a []     = return a
foldM f a (x:xs) = f a x >>= \y -> foldM f y xs

easier to understand pseudo-Haskell:

foldM f a1 [x1,x2,...,xn] = do a2 <- f a1 x1
                               a3 <- f a2 x2
                               ...
                               f an xn

Right-to-left : reverse input before calling foldM.

Example 3:
-}

-- TODO : use this

-- traceFamily :: Sheep -> [ Sheep -> Maybe Sheep ] -> Maybe Sheep
traceFamily :: Monad m => Sheep -> [ Sheep -> m Sheep ] -> m Sheep
traceFamily = foldM getParent
  where getParent s f = f s

paternalGrandmotherEx        s = traceFamily s [father, mother]
mothersPaternalGrandfatherEx s = traceFamily s [mother, father, father]

{-
Typical use of foldM is within a do block.
See example4.hs
    program builds dictionary from entries in all files named on the command line

-- like list filter, but inside of a monad.
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM p []     = return []
filterM p (x:xs) = do b  <- p x
                      ys <- filterM p xs
                      return (if b then (x:ys) else ys)

See example5.hs

-- zipWithM : monadic zipWith function on lists
zipWithM ::(Monad m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence (zipWith f xs ys)

-- discards output
zipWithM_ ::(Monad m) => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-}

zipWithMHC :: Maybe [(Int,Char)]
zipWithMHC  = zipWithM  (curry Just)  [1,2,3] "abc"

zipWithM_HC :: IO ()
zipWithM_HC = zipWithM_ (curry print) [1,2,3] ['a','b','c']

{-
6.2.3 Conditional monadic computations

when :: (Monad m) => Bool -> m () -> m ()
when p s = if p then s else return ()

unless :: (Monad m) => Bool -> m () -> m ()
unless p s = when (not p) s

6.2.4 ap and the lifting functions

Lifting : converts a non-monadic function to work monadic values.

Useful for operating on monad values outside of a do block.
Or cleaner code within a do block.

liftM :: (Monad m) => (a -> b) -> (m a -> m b)
liftM f = \a -> do
    a' <- a
    return (f a')

liftM2 :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
liftM2 f = \a b ->
    a' <- a
    b' <- b
    return (f a' b')

up to liftM5 defined in Monad module.

example 6: To make code more concise:
-}

-- converts "Smith, John" into "John Smith"
swapNames :: String -> String
swapNames s = let (ln,fn) = break (==',') s
              in dropWhile isSpace (tail fn) ++ " " ++ ln

getName :: String -> Maybe String
getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  liftM swapNames (lookup name db)

{- Without using the liftM operation, we would have had to do something
   that is less succinct, like this:

getName name = do let db = [("John", "Smith, John"), ("Mike", "Caine, Michael")]
                  tempName <- lookup name db
	          return (swapNames tempName)

The difference is even greater when lifting functions with more arguments.

Lifting enables  concise higher-order functions.

-}

-- returns list containing result of folding the given binary operator
-- through all combinations of elements of the given lists.
-- e.g., allCombinations (+) [[0,1],[1,2,3]]
--   => [0+1,0+2,0+3,1+1,1+2,1+3], or [1,2,3,2,3,4]
--       allCombinations (*) [[0,1],[1,2],[3,5]] would be
--   => [0*1*3,0*1*5,0*2*3,0*2*5,1*1*3,1*1*5,1*2*3,1*2*5], or [0,0,0,0,3,5,6,10]
allCombinations :: (a -> a -> a) -> [[a]] -> [a]
allCombinations fn []     = []
allCombinations fn (l:ls) = foldl (liftM2 fn) l ls

ac1 = allCombinations (+) [[0,1],[1,2,3]]
ac2 = allCombinations (*) [[0,1],[1,2],[3,5]]
ac3 = allCombinations div [[100, 45, 365], [3, 5], [2, 4], [2]]

{-
related function : 'ap' : sometimes more lift.

ap :: (Monad m) => m (a -> b) -> m a -> m b
ap = liftM2 ($)

Note:      liftM2 f      x      y
           return f `ap` x `ap` y

and so on for functions of more arguments.

Useful when working with higher-order functions and monads.

Effect of ap depends on the monad in which it is used.

[(*2),(+3)] `ap` [0,1,2] == [0,2,4,3,4,5]
(Just (*2)) `ap` (Just 3) == Just 6
-}

-- lookup the commands and fold ap into the command list to
-- compute a result.

apEx val cmds0 =
    let fns  = [("double",(2*))
               ,("halve",(`div`2))
               ,("square",\x->x*x)
               ,("negate", negate)
               ,("incr",(+1))
               ,("decr",(+(-1)))
               ]
        cmds = map (`lookup` fns) (words cmds0)
     in foldl (flip ap) (Just val) cmds

apEx1 = apEx 2 "double square decr negate"

{-
6.2.5 Functions for use with MonadPlus

-}

------------------------------------------------------------------------------

test :: IO ()
test = do
    let dolly = breedSheep
    print (mother dolly)
    print [maternalGF1 dolly, maternalGF2 dolly]
    print (maternalGF3 dolly)
    print (momsPaternalGF1 dolly)
    print (dadsMaternalGF2 dolly)
    print (dadsMaternalGF3 dolly)
    print (parent dolly)
    print seqExM
    print seqExL
    seq_ExM
    print mapMExM
    mapM_ExM
    print (paternalGrandmotherEx dolly)
    print (mothersPaternalGrandfatherEx dolly)
    print (traceFamily dolly [mother,mother,mother])
    print zipWithMHC
    _ <- zipWithM_HC
    print [getName "John", getName "Mike", getName "Harold"]
    print [ac1,ac2,ac3]
    print apEx1
    return ()

-- End of file.

