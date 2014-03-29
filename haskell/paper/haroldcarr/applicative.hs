import           Control.Applicative
import           Control.Monad
import qualified Data.Map            as Map
import           Test.HUnit
import           Test.HUnit.Util

-- http://www.soi.city.ac.uk/~ross/papers/Applicative.pdf

------------------------------------------------------------------------------
-- p. 2

-- return :: Monad       m => a -> m a
--     ap :: Monad       m => m (a -> b) -> m a -> m b


-- http://hackage.haskell.org/package/base-4.6.0.0/docs/src/Control-Monad.html#ap
liftM2'  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2' f m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> return (f x1 x2)

-- > return f `ap` x1 `ap` ... `ap` xn
-- equivalent to
-- > liftMn f x1 x2 ... xn

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' =  liftM2' id

-- http://hackage.haskell.org/package/base-4.4.1.0/docs/src/Control-Applicative.html
-- instance Applicative [] where
--     pure = return
--    (<*>) = ap

seq'  :: Monad       m => [m a] -> m [a]
seq'  []       = return []
seq'  (c : cs) = return (:) `ap'` c `ap'` seq'  cs

-- p. 4
--  (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--   pure :: Applicative f => a -> f a
seq'' :: Applicative f => [f a] -> f [a]
seq'' []       = pure   []
seq'' (c : cs) = pure   (:) <*>  c <*>  seq'' cs

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
-- p. 2
-- typeclassopedia: collection point of view
-- pair functions and inputs elementwise and produce list of resulting outputs

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

-- p. 5
-- typeclassopedia: non-deterministic computation point of view
-- apply function to inputs in turn

transpose'' :: [[a]] -> [[a]]
transpose''         [] = pure []
transpose'' (xs : xss) = pure (:) <*> xs <*> transpose'' xss

t3 :: [Test]
t3 = t "t3"
     (transpose'' v)
     [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]

------------------------------------------------------------------------------
-- p. 3

data Exp v = Var v
           | Val Int
           | Add (Exp v) (Exp v)
           deriving (Show)

type Env v = Map.Map v Int

fetch :: Ord v => v -> Env v -> Int
fetch v env = env Map.! v

eval  :: Ord v => Exp v -> Env v -> Int
eval  (Var x)   env = fetch x env
eval  (Val i)   env = i
eval  (Add p q) env = eval p env + eval q env

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
-- p. 5

dist :: Applicative f => [f a] -> f [a]
dist [] = pure []
dist (v : vs) = pure (:) <*> v <*> (dist vs)

-- traverses list twice
flakyMap :: (a -> Maybe b) -> [a] -> Maybe [b]
flakyMap f ss = dist (fmap f ss)

funny x = if x `mod` 2 /= 0 then Nothing else Just x

t6 = tt "t6"
     [   flakyMap funny [3,4]
     , dist (fmap funny [3,4])
     , dist [Nothing,Just 8]
     , pure (:) <*> Nothing <*> (dist [Just 8])
     , pure (:) <*> Nothing <*> (pure (:) <*> Just 8 <*> (dist []))
     , pure (:) <*> Nothing <*> (pure (:) <*> Just 8 <*> (pure []))
     , Just (:) <*> Nothing <*> (Just (:) <*> Just 8 <*> (Just []))
     , (liftM2 (id) (Just (:)) Nothing) <*> (Just (:) <*> Just 8 <*> (Just []))
     ,                         Nothing  <*> (Just (:) <*> Just 8 <*> (Just []))  -- this is the MAGIC step
     ,            (liftM2 (id) Nothing      (Just (:) <*> Just 8 <*> (Just [])))
     ]
     Nothing

t7 = t "t7"
     (flakyMap funny [2,4])
     (Just [2,4])

-- traverse list once
traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse f [] = pure []
traverse f (x : xs) = pure (:) <*> (f x) <*> (traverse f xs)

t8 = t "t8"
     (traverse funny [5,8])
     Nothing

t9 = t "t9"
     (traverse funny [6,8])
     (Just [6,8])

------------------------------------------------------------------------------

runTests :: IO Counts
runTests =
    runTestTT $ TestList $ t1 ++ t2 ++ t3 ++ t4 ++ t5 ++ t6 ++ t7 ++ t8 ++ t9

-- End of file
