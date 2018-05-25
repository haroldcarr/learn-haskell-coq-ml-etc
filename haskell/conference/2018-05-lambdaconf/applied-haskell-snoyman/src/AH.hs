{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}

module AH where

import           Text.Read

-- ==============================================================================
-- https://github.com/fpco/applied-haskell/tree/2018#readme

-- Pre-reading

-- fmap in terms of >>= and return
{-# ANN fmapBR "HLint: ignore Use fmap" #-}
fmapBR :: Monad m => (a -> b) -> m a -> m b
fmapBR f ma = ma >>= return . f

-- fmap in terms of pure and <*>
fmapPA :: Monad m => (a -> b) -> m a -> m b
fmapPA f ma = pure f <*> ma

-- >>= in terms of Applicative instance and join
bindAJ :: (Monad m, Applicative m) => m a -> (a -> m b) -> m b
bindAJ ma f = joinB (pure f <*> ma)

-- join in terms of >>=
{-# ANN joinB "HLint: ignore Use join" #-}
joinB :: (Monad m) => m (m a) -> m a
joinB x = x >>= id

------------------------------------------------------------------------------
-- https://www.snoyman.com/blog/2017/01/functors-applicatives-and-monads

getLine2 = do
  putStr "> "
  i1 <- getLine
  putStr "> "
  i2 <- getLine
  return (i1, i2)

--------------------------------------------------
-- functors

twoMonadLevels1 = do
  (si1, si2) <- getLine2
  let x = do
        i1 <- readMaybe si1
        i2 <- readMaybe si2
        return (i1 - i2)
  print x

twoMonadLevels2 = do
  (si1, si2) <- getLine2
  let x = do
        f <- fmap (-) (readMaybe si1) -- returns Integer -> Integer
        fmap f (readMaybe si2)
  print x

--------------------------------------------------
-- applicative functors

twoMonadLevels3 = do
  (si1, si2) <- getLine2
  print $ fmap (-) (readMaybe si1) <*> readMaybe si2

twoMonadLevels4 = do
  (si1, si2) <- getLine2
  print $ (-)  <$>  readMaybe si1  <*> readMaybe si2

--------------------------------------------------
-- monads

twoMonadLevels5 = do
  (si1, si2) <- getLine2
  let x = do
        i1 <- readMaybe si1
        i2 <- readMaybe si2
        return $
          if i1 < i2      -- context sensitivity
            then i2 - i1
            else i1 - i2
  print x

--------------------------------------------------
-- exercises

-- 1. fmap using <*> and return
-- see above

-- 2.

returnMaybe = Just

-- 3. N/A

-- 4. N/A

-- 5.

yourHelperFunction f i1 i2 =
  if i1 < i2
    then f i2 i1
    else f i1 i2

yourHelperFunction2 f i1 i2
  | i1 < i2   = f i2 i1
  | otherwise = f i1 i2

-- ==============================================================================

-- Intro - Overview
-- https://github.com/fpco/applied-haskell/blob/2018/overview.md

-- Intro - Synonym in base
-- https://haskell-lang.org/tutorial/synonyms

{-
GHC.OldList.concat ::                            [[a]] -> [a]
Prelude.concat     ::  Foldable t            => t [a]  -> [a]
mconcat            ::              Monoid a  =>   [a]  ->  a
fold               :: (Foldable t, Monoid a) => t  a   ->  a

--------------------------------------------------

GHC.OldList.concatMap ::                           (a -> [b]) -> [a] -> [b]
Prelude.concatMap     ::  Foldable t            => (a -> [b]) -> t a -> [b]
foldMap               :: (Foldable t, Monoid b) => (a ->  b) ->  t a ->  b

--------------------------------------------------

(*>) :: Applicative f => f a -> f b -> f b
(>>) :: Monad       m => m a -> m b -> m b

--------------------------------------------------

pure   :: Applicative f => a -> f a
return :: Monad       m => a -> m a

--------------------------------------------------

map   ::              (a -> b) -> [a] -> [b]
fmap  :: Functor f => (a -> b) -> f a -> f b
liftM :: Monad   m => (a -> b) -> m a -> m b

--------------------------------------------------

traverse_ :: (Foldable    t, Applicative f) => (a -> f b) -> t a -> f ()
mapM_     :: (Foldable    t, Monad       m) => (a -> m b) -> t a -> m ()

traverse  :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
mapM      :: (Traversable t, Monad       m) => (a -> m b) -> t a -> m (t b)

-----

for       :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
forM      :: (Traversable t, Monad       m) => t a -> (a -> m b) -> m (t b)

------------------------------------------------------------------------------

sequenceA_ :: (Foldable   t, Applicative f) => t (f a) -> f ()
sequence_  :: (Foldable   t, Monad       m) => t (m a) -> m ()

sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
sequence  :: (Traversable t, Monad       m) => t (m a) -> m (t a)

--------------------------------------------------

(++)                ::               [a] -> [a] -> [a]
Data.Semigroup.(<>) :: Semigroup a => a  ->  a  ->  a
Data.Monoid.(<>)    :: Monoid    m => m  ->  m  ->  m
mappend             :: Monoid    m => m  ->  m  ->  m
-}

-- ==============================================================================

-- -- Intro - Operator glossary
-- https://haskell-lang.org/tutorial/operators

{-
function application

($) :: (a -> b) -> a -> b

use case: enables replacing
   foo  (bar  (baz bin))
with
   foo $ bar $ baz bin

use case : applying fun to arg : ($ 5) means apply function to 5

   foo = map ($ 5) [\x -> x + x,\x -> x * x]

--------------------------------------------------

Function composition

(.) :: (b -> c) -> (a -> b) -> (a -> c)

--------------------------------------------------

Reverse function application

(&) :: a -> (a -> b) -> b

like $, only backwards

    foo $ bar $ baz bin == bin & baz & bar & foo

--------------------------------------------------

Monoidal append

(<>) :: Monoid m => m -> m -> m

--------------------------------------------------

Functor map

-- fmap
(<$>) :: Functor f =>  (a ->   b) -> f a -> f b

-- replace values inside functor
(<$)  :: Functor f =>   a -> f b  -> f a
 ($>) :: Functor f => f a ->   b  -> f b

--------------------------------------------------

Applicative function application

-- apply function inside first functor to value inside arg functor and wrap/return
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

-- ignore value of 2nd arg
(<*)  :: Applicative f => f a -> f b -> f a

-- ignore value of 1st arg
 (*>) :: Applicative f => f a -> f b -> f b

--------------------------------------------------

monadic binding/composition
(>>=) :: Monad m =>       m a  -> (a -> m b) ->       m b
(=<<) :: Monad m => (a -> m b) ->       m a  ->       m b
(>>)  :: Monad m =>       m a  ->       m b  ->       m b
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)


--------------------------------------------------

Alternative

(<|>) :: Alternative f => f a -> f a -> f a
-}

-- ==============================================================================

-- -- Intro - Common typeclasses
-- https://github.com/fpco/applied-haskell/blob/2018/common-typeclasses.md
-- https://wiki.haskell.org/Typeclassopedia

-- TODO

-- ==============================================================================

-- -- Intro - All About Strictness

------------------------------------------------------------------------------
-- https://www.fpcomplete.com/blog/2017/09/all-about-strictness

-- * BangPatterns

addL = (+)

add !x !y = x + y -- evaluate args before entering functions (not really, see next)

-- unsugared

add' x y = x `seq` y `seq` x + y

addIt1 = do
  let !five  = add (1 + 1) (1 + 2) -- evaluate five now
      !seven = add (1 + 2) (1 + 3) -- evaluate seven now (even though not used)
  putStrLn ("Five: " ++ show five)

addIt2 = do
  let  five  = add (1 + 1) (1 + 2) -- evaluate five now
       seven = add (1 + 2) (1 + 3) -- evaluate seven now (even though not used)
  five `seq` seven `seq` putStrLn ("Five: " ++ show five)

-- * bottom

addIt3 = do
  let  five  = add (1 + 1) (1 + 2)
       seven = add (1 + 2) undefined
  putStrLn ("Five: " ++ show five)

addIt4 = do
  let  !five  = add (1 + 1) (1 + 2)
       !seven = add (1 + 2) undefined
  putStrLn ("Five: " ++ show five)

-- * evaluate, force

data RunningTotal = RunningTotal
  { sum   :: Int
  , count :: Int
  }

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise  = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage :: [Int] -> IO ()
printListAverage = go (RunningTotal 0 0)
  where
    go rt                          []  = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let rt = RunningTotal (sum + x) (count + 1)
       in go rt xs

average :: IO ()
average = printListAverage [1..1000000]

{-
LOTS OF MEMORY USAGE!

stack build
stack exec average --rts-options "+RTS -s"
-}

-- Weak Head Normal Form (WHNF)
-- see : https://stackoverflow.com/a/6889335/369198 -- TODO

-- seq evaluates to WHNF

x1 = putStrLn $        undefined  `seq` "Hello"
-- WHNF only evaluates outer constructor
x2 = putStrLn $ Just   undefined  `seq` "Hello"
-- partially applied fun is WHNF
x3 = putStrLn $ error             `seq` "Hello"
x4 = putStrLn $ (\_ -> undefined) `seq` "Hello"
-- fully applied WHNF
x5 = putStrLn $ error "FOO"       `seq` "Hello"

-- no
e1 = (+) undefined
-- no
e2 = Just undefined
-- yes
e3 = undefined 5
-- I said YES, but it is NO
e4 = error "foo" :: Int -> Double

