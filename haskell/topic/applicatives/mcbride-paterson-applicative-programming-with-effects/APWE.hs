-- for instance Functor Id => Applicative Id where
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

module APWE where

import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as Map
import           Prelude             hiding (Traversable, traverse)
import           Test.HUnit
import           Test.HUnit.Util

-- http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf

------------------------------------------------------------------------------
-- p. 2  Example: sequencing commands

-- sequencing commands via Monad ap and return

-- return :: Monad       m => a -> m a

-- http://hackage.haskell.org/package/base-4.6.0.0/docs/src/Control-Monad.html#ap
liftM2'  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2' f m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (f x1 x2)

-- > return f `ap` x1 `ap` ... `ap` xn
-- equivalent to
-- > liftMn f x1 x2 ... xn

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' =  liftM2' id

seq'  :: Monad m => [m a] -> m [a]
seq'  []       = return []
seq'  (c : cs) = return (:) `ap'` c `ap'` seq'  cs

-- p. 4

-- sequencing commands via Applicative <*> and pure

--  class Applicative f where
--    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--    pure  :: Applicative f => a -> f a

-- Note: Applicative generalizes S and K combinators (see eval below)
-- from threading an environment to threading an effect in general.

-- Can be defined using Monad operations (but the power of those operations not strictly needed).
-- http://hackage.haskell.org/package/base-4.4.1.0/docs/src/Control-Applicative.html
-- instance Applicative [] where
--     pure = return
--    (<*>) = ap

seq'' :: Applicative f => [f a] -> f [a]
seq'' []       = pure []
seq'' (c : cs) = pure (:) <*>  c <*>  seq'' cs

ioa = [getLine,getLine,getLine]
-- seq'  ioa
-- seq'' ioa

t1 :: [Test]
t1 = tt "t1"
     [ seq'                                [Just 1,                      Just 2                                    ]
     , seq''                               [Just 1,                      Just 2                                    ]

     ,                      (Just (:)) <*> (Just 1) <*> (                      (Just (:)) <*> (Just 2)  <*> (Just []))
     , liftM2 id            (Just (:))     (Just 1) <*> (                      (Just (:)) <*> (Just 2)  <*> (Just []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))    (                      (Just (:)) <*> (Just 2)  <*> (Just []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))    (           (liftM2 id (Just (:))     (Just 2)) <*> (Just []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))     (liftM2 id (liftM2 id (Just (:))     (Just 2))     (Just []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))     (liftM2 id ( Just (id       (:)            2))     (Just []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))     (liftM2 id ( Just (         (:)            2))     (Just []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))     (Just             (         (:)            2             []))
     , liftM2 id (liftM2 id (Just (:))     (Just 1))     (Just                                     [2              ])
     , liftM2 id (           Just ((:)           1))     (Just                                     [2              ])
     , Just      (                 (:)           1                                                 [2              ])
     , Just                                     [1,                                                 2              ]
     ]
     (Just [1, 2])

------------------------------------------------------------------------------
-- p. 2  Example: transposing 'matrices'

-- typeclassopedia
--   collection point of view
--   pair functions and inputs elementwise and produce list of resulting outputs

transpose   :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs : xss) = zipWith (:) xs (transpose xss)

repeat' :: a -> [a]
repeat' x = x : repeat' x

zapp :: [a -> b] -> [a] -> [b]
zapp (f : fs) (x : xs) = f x : zapp fs xs
zapp       _        _  = []

transpose'  :: [[a]] -> [[a]]
transpose'         [] = repeat []
transpose' (xs : xss) = repeat' (:) `zapp` xs `zapp` transpose' xss

v = [[1,2,3],[4,5,6]]

t2 :: [Test]
t2 = tt "t2"
     [ (transpose  v)
     , (transpose' v)
     ]
     [[1,4],[2,5],[3,6]]

-- http://stackoverflow.com/questions/22734551/request-clarification-of-transposition-example-in-mcbride-paterson-applicative-p
-- Note: the transpose defined on page 5 is NOT the standard idiom bracket translation:
-- typeclassopedia
--   non-deterministic computation point of view
--   apply function to inputs in turn

transpose'' :: [[a]] -> [[a]]
transpose''         [] = pure []
transpose'' (xs : xss) = pure (:) <*> xs <*> transpose'' xss

t3 :: [Test]
t3 = t "t3"
     (transpose'' v)
     [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

-- Instead, the paper says "where pure = repeat and (~) = zapp" which tranlates to transpose' above.

------------------------------------------------------------------------------
-- p. 3  Example: hiding environment when evaluating expressions

data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
           deriving (Show)

type Env v = Map.Map v Int

fetch :: Ord v => v -> Env v -> Int
fetch v env = env Map.! v

-- explicit environment
eval  :: Ord v => Exp v -> Env v -> Int
eval  (Var x)   env = fetch x env
eval  (Val i)   env = i
eval  (Add p q) env = eval p env + eval q env

-- hiding environment using S and K combinators
eval' :: Ord v => Exp v -> Env v -> Int
eval' (Var x)       = fetch x
eval' (Val i)       = k i
eval' (Add p q)     = k (+) `s` eval' p `s` eval' q

k :: a -> Env v -> a
k x env = x

s :: (Env v -> a -> b) -> (Env v -> a) -> (Env v -> b)
s ef es env = (ef env) (es env)

t4 = tt "t4"
     [ eval' (Add (Val 3) (Val 4))   (Map.fromList [("x",4)])
     , eval' (Add (Val 3) (Var "x")) (Map.fromList [("x",4)])
     ]
     7

-- p. 4

-- hiding the environment using Applicative
eval'' :: Ord v => Exp v -> Env v -> Int
eval'' (Var x)       = fetch x
eval'' (Val i)       = pure i
eval'' (Add p q)     = pure (+) <*> (eval'' p) <*> (eval'' q)

t5 = tt "t5"
     [ eval'' (Add (Val 3) (Val 4))   (Map.fromList [("x",4)])
     , eval'' (Add (Val 3) (Var "x")) (Map.fromList [("x",4)])
     ]
     7

------------------------------------------------------------------------------
-- p. 5  traversing data structures

-- `dist'` is a generalization of `transpose''` to work with any Applicative (not just lists)
dist' :: Applicative f => [f a] -> f [a]
dist' [] = pure []
dist' (v : vs) = pure (:) <*> v <*> (dist' vs)

-- map operation (that might fail) such that any individual failure causes overall failure
-- traverses list twice
flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap f ss = dist' (fmap f ss)

isEven :: Int -> Maybe Int
isEven x = if even x then Just x else Nothing

t6 = tt "t6"
     [   flakyMap isEven [3,4]
     , dist' (fmap isEven [3,4])
     , dist' [Nothing,Just 8]
     , pure (:) <*> Nothing <*>       (dist' [Just 8])
     , pure (:) <*> Nothing <*> (pure (:) <*> Just 8 <*> (dist' []))
     , pure (:) <*> Nothing <*> (pure (:) <*> Just 8 <*> (pure []))
     , Just (:) <*> Nothing <*> (Just (:) <*> Just 8 <*> (Just []))
     , (liftM2 (id) (Just (:)) Nothing) <*> (Just (:) <*> Just 8 <*> (Just []))
     ,                         Nothing  <*> (Just (:) <*> Just 8 <*> (Just []))  -- this is the MAGIC step
     ,            (liftM2 (id) Nothing      (Just (:) <*> Just 8 <*> (Just [])))
     ]
     Nothing

t7 = t "t7"
     (flakyMap isEven [2,4])
     (Just [2,4])

-- applicative mapping
-- same this as `flakyMap`, but traverse list once
-- just like `map` for lists, but applicative version
traverse' :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse' f [] = pure []
traverse' f (x : xs) = pure (:) <*> (f x) <*> (traverse' f xs)

t8 = t "t8"
     (traverse' isEven [5,8])
     Nothing

t9 = t "t9"
     (traverse' isEven [6,8])
     (Just [6,8])

-- TODO: I do not understand this section.

-- generalized `traverse'` (like `fmap` is generalized map)
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- where `t` is the generalization (of [])
-- http://hackage.haskell.org/package/base-4.6.0.1/docs/src/Data-Traversable.html#Traversable
class Functor t => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = dist . fmap f
  dist     :: Applicative f =>           t (f a) -> f (t a)
  dist       = traverse id

-- note: can use above to define `fmap` where `f` above is `Id`

newtype Id a = An { an :: a } deriving (Eq, Show)

instance Functor Id where
     f `fmap` An x = An (f x)

instance Applicative Id where
  pure             = An
  An f <*>    An x = An (f x)

fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f = an . traverse (An . f)

instance Traversable Maybe where

t10 = t "t10"
      (fmap (An . (*2)) (Just 3))
      (Just (An {an = 6}))

-- infinite loop:
--       (fmap' (*2) (Just 3))

-- :t                                (An . (*2))
-- =>                                (An . (*2))            :: Num b => b -> Id b

-- :t       traverse                 (An . (*2))   (Just 3)
-- =>       traverse                 (An . (*2))   (Just 3) :: Num b => Id (Maybe b)

-- :t (an . traverse                 (An . (*2)))  (Just 3)
-- => (an . traverse                 (An . (*2)))  (Just 3) :: Num b =>     Maybe b

-- :t (an . (dist             . fmap (An . (*2)))) (Just 3)
-- => (an . (dist             . fmap (An . (*2)))) (Just 3) :: Num a => Maybe a

-- :t (an . (traverse id      . fmap (An . (*2)))) (Just 3)
-- => (an . (traverse id      . fmap (An . (*2)))) (Just 3) :: Num b => Maybe b

-- :t (an . ((dist . fmap id) . fmap (An . (*2)))) (Just 3)
-- => (an . ((dist . fmap id) . fmap (An . (*2)))) (Just 3) :: Num a => Maybe a

-- :t fmap (An . (*2))
-- => fmap (An . (*2)) :: (Functor f, Num b) => f b -> f (Id b)

-- p. 6  traverse a tree
{-
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l a r) = (Node (fmap f l) (f a) (fmap f r))

instance Applicative Tree where
  pure x                          = Node (pure x) x (pure x)
  Leaf         <*> _              = Leaf
  _            <*> Leaf           = Leaf
  (Node l f r) <*> (Node l' v r') = Node (l <*> l') (f v) (r <*> r')

instance Traversable Tree where
  traverse f Leaf         = pure (Leaf)
  traverse f (Node l x r) = pure Node <*> (traverse f l) <*> (f x) <*> (traverse f r)

t11 = t "t11"
      (traverse (*2) (Node Leaf 3 Leaf))
      (Node Leaf 6 Leaf)
-}

------------------------------------------------------------------------------
-- p. 8/9  Applicative viz Monad

-- Monad bind (>>=) enables the value returned by one computation to inﬂuence the choice of another.
-- Applicative apply (<*>) keeps the structure of a computation ﬁxed, just sequencing the effects.

------------------------------------------------------------------------------
-- http://en.wikibooks.org/wiki/Haskell/Applicative_Functors

-- handle variable number of applicative arguments

tw1 = tt "tw1"
      [        (\a   -> a * 3)       `fmap` (Just 10) -- fmap by itself can only deal with one context/container

       -- fmap combined with apply (<*>) can handle multiple contexts/containers

      ,        (\a b c -> a + b + c) `fmap` (Just  5) <*> (Just 10) <*> (Just 15)
      ,        (\a b c -> a + b + c) <$>    (Just  5) <*> (Just 10) <*> (Just 15)
      , pure   (\a b c -> a + b + c) <*>    (Just  5) <*> (Just 10) <*> (Just 15)
      , Just   (\a b c -> a + b + c) <*>    (Just  5) <*> (Just 10) <*> (Just 15)
      , liftA3 (\a b c -> a + b + c)        (Just  5)     (Just 10)     (Just 15)
      ]
      (Just 30)

-- this is how it does it

tw2 = tt "tw2"
      [                (\a b c -> a + b + c)       `fmap` (Just 5) <*> (Just 10) <*> (Just 15)
      ,          Just ((\a b c -> a + b + c)                    5) <*> (Just 10) <*> (Just 15)
      ,         Just (((\a b c -> a + b + c)                    5)           10) <*> (Just 15)
      ,        Just ((((\a b c -> a + b + c) 5)                              10)           15)
      ]
      (Just 30)

------------------------------------------------------------------------------

-- hide an argument until needed (e.g., at `id` below)

hide :: Int -> Int -> Int
hide 2 = pure 2
hide 5 = pure (+) <*> hide 2 <*> hide 2
hide _ = id

th1 = t "th1" ((hide 2) 100) 2
th2 = t "th2" ((hide 5) 100) 4
th3 = t "th3" ((hide 1) 100) 100

------------------------------------------------------------------------------

runTests :: IO Counts
runTests =
    runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t4 ++ t5 ++ t6 ++ t7 ++ t8 ++ t9
                              ++ t10
                              ++ tw1 ++ tw2
                              ++ th1 ++ th2 ++ th3

-- End of file
