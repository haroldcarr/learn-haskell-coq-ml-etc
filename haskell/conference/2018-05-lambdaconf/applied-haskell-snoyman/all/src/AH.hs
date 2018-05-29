{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module AH where

import qualified Conduit             as C
import           Control.DeepSeq
import qualified Control.Monad.State as S
import qualified Data.Foldable       as F
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as VU
import           Debug.Trace
import           GHC.Generics        (Generic)
import           Text.Read

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Redundant seq" :: String) #-}
{-# ANN module ("HLint: ignore Use const" :: String) #-}
{-# ANN module ("HLint: ignore Use foldl" :: String) #-}
{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
{-# ANN module ("HLint: ignore Use sum" :: String) #-}

-- ==============================================================================
-- https://github.com/fpco/applied-haskell/tree/2018#readme

-- * Pre-reading

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

-- * Intro - Overview
-- https://github.com/fpco/applied-haskell/blob/2018/overview.md

-- * Intro - Synonym in base
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

-- * Intro - Operator glossary
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

-- * Intro - Common typeclasses
-- https://github.com/fpco/applied-haskell/blob/2018/common-typeclasses.md
-- https://wiki.haskell.org/Typeclassopedia

-- TODO

-- ==============================================================================

-- * Intro - All About Strictness

------------------------------------------------------------------------------
-- https://www.fpcomplete.com/blog/2017/09/all-about-strictness

-- ** BangPatterns

addL = (+)

add !x !y = x + y -- evaluate args before entering functions (not really, see next)

-- unsugared

add' x y = x `seq` y `seq` x + y

a1 = do
  let   five  = trace "five"  (add (1 + 1) (1 + 2))
        seven = trace "seven" (add (1 + 2) (1 + 3))
  putStrLn ("Five: " ++ show five)

a2 = do
  let  !five  = trace "five"  (add (1 + 1) (1 + 2)) -- evaluate five now
       !seven = trace "seven" (add (1 + 2) (1 + 3)) -- evaluate seven now (even though not used)
  putStrLn ("Five: " ++ show five)

a3 = do
  let   five  = trace "five"  (add (1 + 1) (1 + 2))
        seven = trace "seven" (add (1 + 2) (1 + 3))
  five `seq` seven `seq` putStrLn ("Five: " ++ show five)

-- * bottom

a4 = do
  let  five  = add (1 + 1) (1 + 2)
       seven = add (1 + 2) undefined
  putStrLn ("Five: " ++ show five)

a5 = do
  let  !five  = add (1 + 1) (1 + 2)
       !seven = add (1 + 2) undefined
  putStrLn ("Five: " ++ show five)

-- ** evaluate, force

data RunningTotal = RunningTotal
  { sum   :: Int
  , count :: Int
  }

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise  = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage1 :: [Int] -> IO ()
printListAverage1 = go (RunningTotal 0 0)
  where
    go rt                          []  = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let rt = RunningTotal (sum + x) (count + 1)
       in go rt xs

average1 :: IO ()
average1 = printListAverage1 [1..1000000]

{-
LOTS OF MEMORY USAGE!

stack build
stack exec average --rts-options "+RTS -s" -- a 1
-}

-- ** Weak Head Normal Form (WHNF)
-- see : https://stackoverflow.com/a/6889335/369198 -- TODO

-- seq evaluates to WHNF
-- WHNF is "unwrapping constructor" (not what is inside)

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

-- fix average

printListAverage2 :: [Int] -> IO ()
printListAverage2 = go (RunningTotal 0 0)
  where
    go rt                          []  = printAverage rt
    go (RunningTotal !sum !count) (x:xs) =     -- do NOT accumulate chain of thunks
      let rt = RunningTotal (sum + x) (count + 1)
       in go rt xs

average2 :: IO ()
average2 = printListAverage2 [1..1000000]

{-
BETTER MEMORY USAGE

stack build
stack exec average --rts-options "+RTS -s" -- a 2
-}

printListAverage3 :: [Int] -> IO ()
printListAverage3 = go (RunningTotal 0 0)
  where
    go rt                          []  = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let !sum'   = sum   + x            -- better because evaluates now
          !count' = count + 1            -- instead of next time in loop like average2
          rt = RunningTotal sum' count'
       in go rt xs

average3 :: IO ()
average3 = printListAverage2 [1..1000000]

{-
BETTER MEMORY USAGE

stack build
stack exec average --rts-options "+RTS -s" -- a 3
-}

-- ** DEEPSEQ : full evalutation to Normal Form (NF)
-- Works by providing `Control.DeepSeq` `NFData` type class that defines `rnf` : reduce to normal form.

instance NFData RunningTotal where
  rnf (RunningTotal sum count) = sum `deepseq` count `deepseq` ()

printListAverage4 :: [Int] -> IO ()
printListAverage4 = go (RunningTotal 0 0)
  where
    go rt                          []  = printAverage rt
    go (RunningTotal sum count) (x:xs) =
      let rt = RunningTotal (sum +x) (count + 1)
       in rt `deepseq` go rt xs

average4 :: IO ()
average4 = printListAverage2 [1..1000000]

{-
stack build
stack exec average --rts-options "+RTS -s" -- a 4

Using `deepseq` on all inside a data constructor usually the right approach for `NFData` instances.

Can use `DeriveGeneric` and `GHC.Generics`:
-}

data RunningTotal2 = RunningTotal2
  { sum2   :: Int
  , count2 :: Int
  } deriving Generic
instance NFData RunningTotal2

{-
Besides avoiding space leaks (as is done above),
also avoids accidentally including exceptions inside thunks within a value.
See: `tryAnyDeep` in `safe-exceptions`.

-- | Same as upstream Control.Monad.Catch try, but will not catch asynchronous exceptions
try        :: (MonadCatch m, Exception e)                      => m a -> m (Either e a)
-- | try specialized to only catching IOExceptions
tryIO      ::  MonadCatch m                                    => m a -> m (Either IOException a)
-- | try specialized to catch all synchronous exceptions
tryAny     ::  MonadCatch m                                    => m a -> m (Either SomeException a)
-- | Same as try, but fully force evaluation of the result value to find all impure exceptions.
tryDeep    :: (MonadCatch m, MonadIO m, Exception e, NFData a) => m a -> m (Either e a)
-- | tryDeep specialized to catch all synchronous exceptions
tryAnyDeep :: (MonadCatch m, MonadIO m, NFData a)              => m a -> m (Either SomeException a)
-}

mydeepseq :: NFData a => a -> b -> b
mydeepseq a b = rnf a `seq` b

-- ** STRICT DATA

-- make it impossible to constructor a value with laziness inside

data RunningTotal5 = RunningTotal5
  { sum5   :: !Int
  , count5 :: !Int
  }
  -- deriving Generic

printAverage5 :: RunningTotal5 -> IO ()
printAverage5 (RunningTotal5 sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage5 :: [Int] -> IO ()
printListAverage5 = go (RunningTotal5 0 0)
  where
    go rt [] = printAverage5 rt
    go (RunningTotal5 sum count) (x:xs) =
      let rt = RunningTotal5 (sum + x) (count + 1)
       in go rt xs

average5 = printListAverage5 [1..1000000]

{-
stack build
stack exec average --rts-options "+RTS -s" -- a 5

For "small" values like an Int, GHC will automatically unbox strict fields.

BEST PRACTICE: unless you know you want laziness for a field, make it strict.
- avoids accidental space leaks
- avoids accidentally including bottom values
- when constructing a value with record syntax, GHC will
    - give an error if strict field forgotten
    - warning for non-strict fields
-}

-- ** newtype

data    Foo = Foo  Int
data    Bar = Bar !Int
newtype Baz = Baz  Int -- no runtime representation

-- err
nt1 = case undefined     of { Foo _ -> putStrLn "Still alive!" }
-- still alive
nt2 = case Foo undefined of { Foo _ -> putStrLn "Still alive!" }

-- err
nt3 = case undefined     of { Bar _ -> putStrLn "Still alive!" }
-- err
nt4 = case Bar undefined of { Bar _ -> putStrLn "Still alive!" }

-- I guessed err but it is still alive
{-
No RT rep.
Impossible for Baz constructor to hide an extra layer of bottomness.
'Baz undefined' and 'undefined' are indistinguishable.
Unwrapping Baz constructor has no effect on RT behavior, since no there in the first place.
Pattern match inside nt5 does nothing.
Equivalent to : case undefined of { _ -> putStrLn "Still alive!" }
Since not inspecting the undefined at all, no exception is thrown.
-}
nt5 = case undefined     of { Baz _ -> putStrLn "Still alive!" }
-- I guessed err but it is still alive
{-
No RT representation, so same as nt5
-}
nt6 = case Baz undefined of { Baz _ -> putStrLn "Still alive!" }

-- ** other ways to force evaluation : $! $!! force

mysum1 :: [Int] -> Int
mysum1 list0 = go list0 0
  where
    go [] total     = total
    go (x:xs) total = go xs $   total + x

mysum2 :: [Int] -> Int
mysum2 list0 = go list0 0
  where
    go [] total     = total
    go (x:xs) total = go xs $!  total + x -- $!  uses     seq to force evaluation before recursion

mysum3 :: [Int] -> Int
mysum3 list0 = go list0 0
  where
    go [] total     = total
    go (x:xs) total = go xs $!! total + x -- $!! uses deepseq to force evaluation before recursion

mysum4 :: [Int] -> Int
mysum4 list0 = go list0 0
  where
    go [] total     = total
    -- function : evaluates a WHNF expression to NF
    go (x:xs) total = go xs $!! force (total + x)

runsum1 = print $ mysum1 [1..10000000]
runsum2 = print $ mysum2 [1..10000000]
runsum3 = print $ mysum3 [1..10000000]
runsum4 = print $ mysum3 [1..10000000]

{-
stack build
stack exec average --rts-options "+RTS -s" -- s 1
stack exec average --rts-options "+RTS -s" -- s 2
stack exec average --rts-options "+RTS -s" -- s 3
stack exec average --rts-options "+RTS -s" -- s 4
-}

dollar,dollar' ::             (a -> b) -> a -> b
dollar''       :: NFData a => (a -> b) -> a -> b
force'         :: NFData a =>  a            -> a
dollar   f a =                             f a
dollar'  f a =                 a `seq`     f a
dollar'' f a =                 a `deepseq` f a
force'     a =                 a `deepseq`   a

-- ** DATA STRUCTURES

-- *** LISTS

-- lazy
data List1 a = Cons1 a (List1 a) | Nil1
-- hello
ec11 = Cons1 undefined undefined `seq` putStrLn "Hello World"
-- hello
ec12 =       undefined:undefined `seq` putStrLn "Hello World"

-- spine strict (i.e., strict in tail)
data List2 a = Cons2 a !(List2 a) | Nil2
-- error
ec21 = Cons2 undefined undefined `seq` putStrLn "Hello World"
-- hello
ec22 = Cons2 undefined (Cons2 undefined Nil2) `seq` putStrLn "Hello World"

-- value strict
data List3 a = Cons3 !a !(List3 a) | Nil3
-- error
ec31 = Cons3 undefined (Cons3 undefined Nil3) `seq` putStrLn "Hello World"

-- strict in value but not tail - not aware of any use of this
data List4 a = Cons4 !a (List4 a) | Nil4

-- *** VECTORS

--  Data.Vector : (aka "boxed vectors") are spine strict

-- hello
vb1 = V.fromList           [undefined] `seq` putStrLn "Hello World"
-- error
vb2 = V.fromList (undefined:undefined) `seq` putStrLn "Hello World"
-- I guessed hello --- but error
vb3 = V.fromList            undefined  `seq` putStrLn "Hello World"

-- Data.Vector.Unboxed are value strict
-- GHC needs inference help
vuFromList :: [Int] -> VU.Vector Int
vuFromList = VU.fromList

-- all throw error
vu1 = vuFromList           [undefined] `seq` putStrLn "Hello World"
vu2 = vuFromList (undefined:undefined) `seq` putStrLn "Hello World"
vu3 = vuFromList            undefined  `seq` putStrLn "Hello World"

-- no strict boxed vector

-- *** SETS and MAPS

{-
unordered-containers
Map-like structures have Strict and Lazy variants
Set-like structures do not (e.g., Data.IntSet)
Because all are spine strict : strict in keys.
Set only has keys : so also value strict.
Map lazy spine-strict, value-lazy
Map strict are spine and value strict.

TODO EXERCISE Analyze the Data.Sequence.Seq data type and classify it as either lazy, spine strict, or value strict.
-}

-- ** FUNCTION ARGUMENTS

{-
function strict in an arg
- if applied to bottom value for that argument result is bottom
- + : strict in both
- const : strict in first
- (:) laz in both
-}

-- ** FOLDS

runfoldl  = print $   foldl  (+) 0 [1..10000000]
runfoldl' = print $ F.foldl' (+) 0 [1..10000000]
runfoldr  = print $   foldr  (+) 0 [1..10000000]
runfoldr' = print $ F.foldr' (+) 0 [1..10000000]

{-
stack build
stack exec average --rts-options "+RTS -s" -- f 1
stack exec average --rts-options "+RTS -s" -- f 2
stack exec average --rts-options "+RTS -s" -- f 3
stack exec average --rts-options "+RTS -s" -- f 4
-}

-- *** STREAMING DATA

averageC :: Monad m => C.ConduitM Int o m Double
averageC = divide <$> C.foldlC add (0, 0)
  where
    divide (total, count)   = fromIntegral total / count
    add    (total, count) x = (total + x, count + 1)

runaveragec = print $ C.runConduitPure $ C.enumFromToC 1 10000000 C..| averageC

{-
stack build
stack exec average --rts-options "+RTS -s" -- a 6

EXERCISE TODO : Make above run in constant resident memory, by using:

1. The force function
2. Bang patterns
3. A custom data type with strict fields
-}

-- *** CHAIN REACTION TODO

-- ** FURTHER READING TODO

{-
http://fixpt.de/blog/2017-12-04-strictness-analysis-part-1.html
-}

-- ==============================================================================

-- * Data Structure - Data Structures

-- ==============================================================================

-- * Data Structure - String Types

-- ==============================================================================

-- * Data Structure - Containers

-- ==============================================================================

-- * Data Structure - Vector

-- ==============================================================================

-- * Data Structure - revisit data structure quiz

-- ==============================================================================

-- * RIO - Monad Transformers
-- https://github.com/fpco/applied-haskell/blob/2018/monad-transformers.md

-- ** Using Either as a monad : foldL with early termination

foldEitherTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldEitherTerminate f acc0 list0 = either id id (go acc0 list0) -- either makes go call in Either context
  where
    go !accum rest = do
      -- usual end of list termination, but in Either
      (x, xs) <- case rest of
                   []   -> Left accum
                   x:xs -> Right (x, xs)
      accum' <- f accum x -- can exit early via Left
      go accum' xs

sumTerm = foldEitherTerminate go 0 ([300, -1] ++ [1..])
 where go acc x
         | x < 0     = Left acc
         | otherwise = Right (acc + x)

-- foldL using State

foldState :: (b -> a -> b) -> b -> [a] -> b
foldState f acc0 list0 = S.execState (mapM_ go list0) acc0
  where go x = S.modify' (`f` x)

-- StateEither monad

-- Cannot compose State and Either.  So define new StateEither monad

newtype StateEither s e a = StateEither
  { -- given initial state value
    -- return updated state value and an Either result value.
    -- When result is Left: stop processing. When Right: continue.
    runStateEither :: s -> (s, Either e a) }
  deriving Functor

instance Applicative (StateEither s e) where
  pure a = StateEither (\s -> (s, Right a))
  StateEither ff <*> StateEither fa = StateEither $ \s0 ->
    case ff s0 of
      (s1, Left e)  -> (s1, Left e)
      (s1, Right f) -> case fa s1 of
        (s2, Left e)  -> (s2, Left e)
        (s2, Right a) -> (s2, Right (f a))

instance Monad (StateEither s e) where
  return = pure
  StateEither f >>= g = StateEither $ \s0 ->
    case f s0 of
      (s1, Left e)  -> (s1, Left e)
      (s1, Right x) -> runStateEither (g x) s1

execStateEither :: StateEither s e a -> s -> s
execStateEither m = fst . runStateEither m

modifyStateEither' :: (s -> Either e s) -> StateEither s e ()
modifyStateEither' f = StateEither $ \s0 ->
  case f s0 of
    Left   e  -> (s0, Left e)
    Right !s1 -> (s1, Right ())

foldStateEitherTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldStateEitherTerminate f accum0 list0 = execStateEither (mapM_ go list0) accum0
  where go x = modify' (`f` x)

-- monads can make it easier to implement some functions
-- composing monads isn't possible
-- manually defining the compositions is possible

-- Reformulating StateEither

-- newtype StateEither s e a = StateEither (s -> (s, Either e a))
-- newtype State s a = State (s -> (s, a))
-- newtype StateEither2 s e a = StateEither2 (State s (Either e a))

newtype StateEither2 s e a = StateEither2
  { unStateEither2 :: S.State s (Either e a) }
  deriving Functor

-- This never touches the state value.
-- It reuses underlying State's Monad instance via do-notation and return.
-- Just focuses on Either shortcut logic.
instance Applicative (StateEither2 s e) where
  pure a = StateEither2 $ return $ Right a
  StateEither2 ff <*> StateEither2 fa = StateEither2 $ do
    ef <- ff
    case ef of
      Left  e -> return $ Left e
      Right f -> do
        ea <- fa
        case ea of
          Left  e -> return $ Left e
          Right a -> return $ Right $ f a

instance Monad (StateEither2 s e) where
  return = pure
  StateEither2 f >>= g = StateEither2 $ do
    ex <- f
    case ex of
      Left  e -> return         $ Left e
      Right x -> unStateEither2 $ g x

execStateEither2 :: StateEither2 s e a -> s -> s
execStateEither2 (StateEither2 m) s = execState m s

modifyStateEither2' :: (s -> Either e s) -> StateEither2 s e ()
modifyStateEither2' f = StateEither2 $ do
  s0 <- S.get
  case f s0 of
    Left   e -> return $ Left e
    Right s1 -> do
      S.put $! s1
      return $ Right ()

-- Just State? -- TODO ...

-- ==============================================================================

-- * RIO - RIO
-- see ../../

