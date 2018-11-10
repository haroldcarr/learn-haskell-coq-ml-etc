{-
Created       : by NICTA.
Last Modified : 2014 Jul 15 (Tue) 10:50:57 by Harold Carr.
-}

{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.State where

import           Course.Applicative
import           Course.Apply
import           Course.Bind
import           Course.Core
import           Course.Functor
import           Course.List
import           Course.Monad
import           Course.Optional
import qualified Data.Set           as S
import qualified Prelude            as P

import qualified Data.Char          as C (digitToInt)
import qualified Test.HUnit         as T
import qualified Test.HUnit.Util    as U

-- $setup
-- >>> import Test.QuickCheck.Function
-- >>> import Data.List(nub)
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> import Course.Core
-- >>> import Course.List
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- | Implement the `Functor` instance for `State s`.
-- HC: This requires the Applicative/State instance (for `pure`).
-- >>> runState ((+1) <$> pure 0) 0
-- (1,0)
instance Functor (State s) where
  (<$>) ::
    (a -> b)
    -> State s a
    -> State s b
  f <$> State k = State (\s -> let (a, t) = k s in (f a, t))

tsf :: [T.Test]
tsf = U.tt "tsf"
      [ (+1) <$> (pure       0)
      , (+1) <$> (Full       0)
      ,          (Full ((+1) 0))
      ,          (Full 1)
      ]
      (Full 1)

trs :: [T.Test]
trs = U.tt "trs"
      [ runState             ((+1) <$> pure           0)                          0
      , runState             ((+1) <$> State (\s  -> (0, s))   )                  0
      , runState (State (\s -> let (a, t) = ((\s' -> (0, s')) s) in ((+1) a, t))) 0
      ,                 (\s -> let (a, t) = ((\s' -> (0, s')) s) in ((+1) a, t))  0
      ,                        let (a, t) = ((\s' -> (0, s')) 0) in ((+1) a, t)
      ,                        let (a, t) =          (0, 0 )     in ((+1) a, t)
      ,                                                             ((+1) 0, 0)
      ,                                                             (1     , 0)
      ]
      (1,0)

-- | Implement the `Apply` instance for `State s`.
-- >>> runState (pure (+1) <*> pure 0) 0
-- (1,0)
--
-- >>> import qualified Prelude as P
-- >>> runState (State (\s -> ((+3), s P.++ ["apple"])) <*> State (\s -> (7, s P.++ ["banana"]))) []
-- (10,["apple","banana"])
instance Apply (State s) where
  (<*>) ::
    State s (a -> b)
    -> State s a
    -> State s b
  State f <*> State a = State (\s -> let (g, t) = f s
                                         (z, u) = a t
                                     in (g z, u))

tap :: [T.Test]
tap = U.tt "tap"
      [ runState                              (pure (+1)       <*>           (pure 1))                     45
      , runState                    ((State (\s -> ((+1), s))) <*>           (pure 1))                     45
      , runState                    ((State (\s -> ((+1), s))) <*>  (State (\s -> (1, s))))                45
      , runState (State (\s -> let (g, t) = (\s -> ((+1), s))  s; (z, u) = (\s -> (1, s))  t in (g z, u))) 45
      ,                 (\s -> let (g, t) = (\s -> ((+1), s))  s; (z, u) = (\s -> (1, s))  t in (g z, u))  45
      ,                        let (g, t) = (\s -> ((+1), s)) 45; (z, u) = (\s -> (1, s))  t in (g z, u)
      ,                        let (g, t) =        ((+1),45)    ; (z, u) = (\s -> (1, s))  t in (g z, u)
      ,                        let (g, _) =        ((+1),45)    ; (z, u) = (\s -> (1, s)) 45 in (g z, u)
      ,                        let (g, _) =        ((+1),45)    ; (z, u) =        (1,45)     in (g z, u)
      ,                                                                                       ((+1)1,45)
      ,                                                                                           (2,45)
      ]
      (2,45)

-- | Implement the `Applicative` instance for `State s`.
-- >>> runState (pure 2) 0
-- (2,0)
instance Applicative (State s) where
  pure ::
    a
    -> State s a
  pure a = State (\s -> (a, s))

-- | Implement the `Bind` instance for `State s`.
-- >>> runState ((const $ put 2) =<< put 1) 0
-- ((),2)
--
-- >>> runState ((\_ -> get) =<< (const $ put 2) =<< put 1) 0
-- (2,2)
--
-- (=<<) :: (a -> f b) -> f a -> f b
instance Bind (State s) where
  (=<<) ::
    (a -> State s b)
    -> State s a
    -> State s b
  l =<< State r = State (\s -> let (ra, rs) = r s
                               in  runState (l ra) rs)

tbs :: [T.Test]
tbs = U.tt "tbs"
      [ runState (                                                            (\_ -> get) =<<  put               1)   0
      , runState (State (\s -> let (ra, rs) = (\_ -> ((), 1)) s in  runState ((\_ -> get)                    ra) rs)) 0
      ,                 (\s -> let (ra, rs) = (\_ -> ((), 1)) s in  runState ((\_ -> get)                    ra) rs)  0
      ,                        let (ra, rs) = (\_ -> ((), 1)) 0 in  runState ((\_ -> get)                    ra) rs
      ,                        let (ra, rs) = (\_ -> ((), 1)) 0 in  runState ((\_ -> get)                    ra) rs
      ,                                                             runState ((\_ -> get)                    ()) 1
      ,                                                             runState ((\_ -> (State (\s -> (s, s)))) ()) 1
      ,                                                             runState (       (State (\s -> (s, s)))    ) 1
      ,                                                                                     (\s -> (s, s))       1
      ]
      (1,1)

ts :: ((List Char, List Char), (List Char, List Char))
ts = runState (
         get              >>= \(_,h :.t )  ->
         put (h :.Nil,t ) >>= \_           ->

         get              >>= \(_,h':.t')  ->
         put (h':.Nil,t') >>= \_           ->

         get

              ) (Nil,'f':.'o':.'o':.Nil)

tss :: [T.Test]
tss = U.t "tss"
      ts
      (('o':.Nil,'o':.Nil),('o':.Nil,'o':.Nil))

type Stack a = List a

pop :: State (Stack a) a
pop = State $ \(x:.xs) -> (x,xs)

push :: a -> State (Stack a) ()
push a = State $ \xs -> ((),a:.xs)

se :: Num a => State (Stack a) ()
se =
    pop       >>= \top   ->
    push (-1) >>= \_     ->
    get       >>= \stack ->
    put (top:.top:.stack)

tse :: [T.Test]
tse = U.t "tse2"
    (runState se (10:.9:.Nil))
    ((),(10:.10:.(-1):.9:.Nil))

instance Monad (State s) where

-- | Run the `State` seeded with `s` and retrieve the resulting state.
--
-- prop> \(Fun _ f) -> exec (State f) s == snd (runState (State f) s)
exec ::
  State s a
  -> s
  -> s
exec (State k) = snd . k
-- HC: exec (State f) s = snd (f s)

-- | Run the `State` seeded with `s` and retrieve the resulting value.
--
-- prop> \(Fun _ f) -> eval (State f) s == fst (runState (State f) s)
eval ::
  State s a
  -> s
  -> a
eval (State k) = fst . k
-- HC: eval (State f) s = fst (f s)


-- | A `State` where the state also distributes into the produced value.
--
-- >>> runState get 0
-- (0,0)
get ::
  State s s
get = State (\s -> (s, s))

-- | A `State` where the resulting state is seeded with the given value.
--
-- >>> runState (put 1) 0
-- ((),1)
put ::
  s
  -> State s ()
put = State . const . (,) ()
-- HC: put p = State (\_ -> ((), p))

-- | HC
modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

-- http://blog.sigfpe.com/2006/05/grok-haskell-monad-transformers.html
t_f :: (t -> t) -> State t (t, t)
t_f f =
    get      >>= \a ->
    modify f >>= \_ ->
    get      >>= \b ->
    pure (a,b)

t_i :: [T.Test]
t_i = U.t "t_i"
      (eval (t_f (+1)) 0)
      (0,1)

t_s :: [T.Test]
t_s = U.t "t_s"
      (eval (t_f (++ ('1':.Nil))) ('0':.Nil))
      ('0':.Nil,'0':.'1':.Nil)

-- | Find the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Monad` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
--
-- >>> let p x = (\s -> (const $ pure (x == 'c')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Full 'c',3)
--
-- >>> let p x = (\s -> (const $ pure (x == 'i')) =<< put (1+s)) =<< get in runState (findM p $ listh ['a'..'h']) 0
-- (Empty,8)
findM ::
  Monad f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil = pure Empty
findM p (h :. t) = (\b -> if b then pure (Full h) else findM p t) =<< (p h)

tp :: Num s => Char -> State s Bool
tp x = (\s -> (const $ pure (x == 'a')) =<< put (1+s)) =<< get

-- :t (findM tp $ listh ['a'])
-- => (findM tp $ listh ['a']) :: Num s => State s (Optional Char)

ttp :: [T.Test]
ttp = U.tt "ttp"
      [ runState (tp                                                                               'a'                                                               ) 0
      , runState (                                                           (\s -> (const $ pure ('a' == 'a')) =<<              put (1+s))  =<< get                 ) 0
      , runState (                                                           (\s -> (const $ pure ('a' == 'a')) =<<              put (1+s))  =<< State (\s -> (s, s))) 0
      , runState (                                                           (\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s))) =<< State (\s -> (s, s))) 0
      , runState (                                                           (\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s))) =<< State (\s -> (s, s))) 0
      , runState (State (\s -> let (ra, rs) = (\s -> (s, s)) s in  runState ((\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s))) ra) rs))                  0
      ,                 (\s -> let (ra, rs) = (\s -> (s, s)) s in  runState ((\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s))) ra) rs)                   0
      ,                        let (ra, rs) = (\s -> (s, s)) 0 in  runState ((\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s))) ra) rs
      ,                        let (ra, rs) =        (0, 0)    in  runState ((\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s))) ra) rs
      ,                                                            runState ((\s -> (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+s)))  0) 0
      ,                                                            runState (       (const $ pure ('a' == 'a')) =<< State (\_ -> ((), 1+0)))     0
      ,     runState (State (\s -> let (ra, rs) = (\_ -> ((), 1+0)) s in  runState ((const $ pure ('a' == 'a')) ra) rs))                         0
      ,                     (\s -> let (ra, rs) = (\_ -> ((), 1+0)) s in  runState ((const $ pure ('a' == 'a')) ra) rs)                          0
      ,                            let (ra, rs) = (\_ -> ((), 1+0)) 0 in  runState ((const $ pure ('a' == 'a')) ra) rs
      ,                                                                   runState ((const $ pure ('a' == 'a')) ()) 1
      ,                                                                   runState ((const $ pure (True      )) ()) 1
      ,                                                                   runState ((const $ State (\s -> (True, s))) ()) 1
      --, TODO: gets type errors : but I swear this is reduction:         runState (         State (\s -> (True, s))    ) 1
      ,                                                                                            (\s -> (True, s))      1
      ]
      (True,1)

tfM :: [T.Test]
tfM = U.tt "tfM"
      [ runState (findM                                                                                                    tp $     listh ['a'])                  0
      , runState (                                                             (\b -> if b    then pure (Full 'a') else findM tp Nil) =<< (tp 'a'))               0
      , runState (                                                             (\b -> if b    then pure (Full 'a') else findM tp Nil) =<< State (\s -> (True,1))) 0
      ,                 (\s -> let (ra, rs) = (\s -> (True,1)) s in  runState ((\b -> if b    then pure (Full 'a') else findM tp Nil)   ra) rs)                   0
      ,                        let (ra, rs) = (\s -> (True,1)) 0 in  runState ((\b -> if b    then pure (Full 'a') else findM tp Nil)   ra) rs
      ,                        let (ra, rs) =        (True,1)    in  runState ((\b -> if b    then pure (Full 'a') else findM tp Nil)   ra) rs
      ,                                                              runState ((\b -> if b    then pure (Full 'a') else findM tp Nil) True) 1
      ,                                                              runState (       if True then pure (Full 'a') else findM tp Nil)       1
      ,                                                              runState (                    pure (Full 'a')                  )       1
      ,                                                              runState (            State (\s -> (Full 'a', s))              )       1
      ,                                                                                          (\s -> (Full 'a', s))                      1

      ]
      (Full 'a',1)

-- | Find the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
--
-- /Tip:/ Use `findM` and `State` with a @Data.Set#Set@.
--
-- prop> case firstRepeat xs of Empty -> let xs' = hlist xs in nub xs' == xs'; Full x -> length (filter (== x) xs) > 1
-- prop> case firstRepeat xs of Empty -> True; Full x -> let (l, (rx :. rs)) = span (/= x) xs in let (l2, r2) = span (/= x) rs in let l3 = hlist (l ++ (rx :. Nil) ++ l2) in nub l3 == l3
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat = listWithState findM S.member

firstRepeatC :: Ord a => List a -> Optional a
firstRepeatC x = eval (findM (\a -> State (S.member a &&& S.insert a)) x) S.empty

firstRepeatHC ::
  Ord a =>
  List a
  -> Optional a
firstRepeatHC la = eval (findM rp la) S.empty

-- Note: the put/insert/s is not the Set seen by the predicate (the next monadic use of the predicate will see it)
rp :: Ord a => a -> State (S.Set a) Bool
rp x = (\s -> (const $ pure (S.member x s)) =<< put (S.insert x s)) =<< get

-- | Remove all duplicate elements in a `List`.
-- /Tip:/ Use `filtering` and `State` with a @Data.Set#Set@.
--
-- prop> firstRepeat (distinct xs) == Empty
--
-- prop> distinct xs == distinct (flatMap (\x -> x :. x :. Nil) xs)
distinct ::
  Ord a =>
  List a
  -> List a
distinct = listWithState filtering S.notMember

listWithState ::
  Ord a1 =>
  ((a1 -> State (S.Set a1) a2)
  -> t
  -> State (S.Set a3) a)
  -> (a1 -> S.Set a1 -> a2)
  -> t
  -> a
listWithState f m x =
  eval (f (State . lift2 (lift2 (,)) m S.insert) x) S.empty

distinctTwo ::
  Ord a =>
  List a
  -> List a
distinctTwo x = eval (filtering (\a -> State (S.notMember a &&& S.insert a)) x) S.empty

-- | A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `first` and `produce`.
----
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappy 4
-- False
--
-- >>> isHappy 7
-- True
--
-- >>> isHappy 42
-- False
--
-- >>> isHappy 44
-- True
isHappy ::
  Integer
  -> Bool
isHappy =
  contains 1 .
    firstRepeat .
    produce (toInteger .
             sum .
             map (join (*) . 
                  digitToInt) .
             show')

-- | HC: OLD: A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
--
-- /Tip:/ Use `findM` with `State` and `produce`.
--
-- /Tip:/ Use `flatten` to write a @square@ function.
--
-- /Tip:/ Use library functions: @Optional#contains@, @Data.Char#digitToInt@.
--
-- >>> isHappyOld 4
-- False
--
-- >>> isHappyOld 7
-- True
--
-- >>> isHappyOld 42
-- False
--
-- >>> isHappyOld 44
-- True
{-
isHappy :: Integer -> Bool
isHappy x = case runState (findM ih (produce sumOfSquares x)) S.empty of
                (Empty , _) -> error "can this happen?"
                (Full v, _) -> v == 1
-}
isHappyC :: Integer -> Bool
isHappyC = contains 1 . (`eval` S.empty) .
               findM (\j -> State $ \s -> (j == 1 || S.member j s, S.insert j s)) .
                   produce (P.sum . (<$>) (join (*) . toInteger . digitToInt) . show)

isHappyOld ::
  Integer
  -> Bool
isHappyOld x = contains 1 $ eval (findM ih (produce sumOfSquares x)) S.empty

ih :: Integer -> State (S.Set Integer) Bool
ih x = (\s -> (const $ pure (S.member x s)) =<< put (S.insert x s)) =<< get

sumOfSquares :: Integer -> Integer
sumOfSquares = sumOfSquares' 0 . show
  where
    sumOfSquares' acc [] = P.toInteger acc
    sumOfSquares' acc (x:xs) = sumOfSquares' (((P.^) (C.digitToInt x) 2) + acc) xs
{-
sumOfSquares 4
sumOfSquares 16
sumOfSquares 37
sumOfSquares 58
sumOfSquares 89
sumOfSquares 145
sumOfSquares 42
sumOfSquares 20
sumOfSquares 4

sumOfSquares 7
sumOfSquares 49
sumOfSquares 97
sumOfSquares 130
sumOfSquares 10
sumOfSquares 1

produce sumOfSquares 7
-}

------------------------------------------------------------------------------

testState :: IO T.Counts
testState =
    T.runTestTT P.$ T.TestList P.$ tsf P.++ trs P.++ tap P.++ tbs P.++ tss P.++ tse P.++ t_i P.++ t_s P.++ ttp P.++ tfM

-- End of file.

