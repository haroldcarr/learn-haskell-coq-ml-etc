{-
Created       : by NICTA.
Last Modified : 2014 Jul 15 (Tue) 12:04:55 by Harold Carr.
-}

{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.StateT where

import           Course.Applicative
import           Course.Apply
import           Course.Bind
import           Course.Core
import           Course.Functor
import           Course.Id
import           Course.List
import           Course.Monad
import           Course.Optional
import           Course.State
import qualified Data.Set           as S
import qualified Prelude            as P

import qualified Test.HUnit         as T
import qualified Test.HUnit.Util    as U

-- $setup
-- >>> import Test.QuickCheck
-- >>> import qualified Prelude as P(fmap)
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap listh arbitrary

-- | A `StateT` is a function from a state value `s` to a functor f of (a produced value `a`, and a resulting state `s`).
newtype StateT s f a =
  StateT {
    runStateT ::
      s
      -> f (a, s)
  }

-- | Implement the `Functor` instance for @StateT s f@ given a @Functor f@.
--
-- >>> runStateT ((+1) <$> (pure 2) :: StateT Int List Int) 0
-- [(3,0)]
instance Functor f => Functor (StateT s f) where
  (<$>) ::
    (a -> b)
    -> StateT s f a
    -> StateT s f b
  f <$> StateT k = StateT ((<$>) (first f) . k)
-- HC f <$> StateT k = StateT (\ s -> (\ ~(a, s') -> (f a, s')) <$> (k s))

{-
:t (pure 3) :: StateT a Optional Int
:t (+1) <$> ((pure 3) :: StateT a Optional Int)
:t runStateT ((+1) <$> ((pure 3) :: StateT a Optional Int)
-}

tstf :: [T.Test]
tstf = U.tt "tstf"
      [ runStateT                                ((+1)         <$>       ((pure  3) :: StateT a Optional Int)) 0
      , runStateT (StateT (\ s -> (\ ~(a, s') -> ((+1) a, s')) <$> ((\s -> pure (3, s)) s)))                   0 -- by Functor StateT
      , runStateT (StateT (\ s -> (\ ~(a, s') -> ((+1) a, s')) <$> ((\s -> Full (3, s)) s)))                   0 -- by Applicative StateT
      ,                   (\ s -> (\ ~(a, s') -> ((+1) a, s')) <$> ((\s -> Full (3, s)) s))                    0 -- by runStateT
      ,                           (\ ~(a, s') -> ((+1) a, s')) <$> ((\s -> Full (3, s)) 0)                       -- by application
      ,                           (\ ~(a, s') -> ((+1) a, s')) <$>         Full (3, 0)                           -- by application
      ,                     Full ((\ ~(a, s') -> ((+1) a, s'))                  (3, 0))                          -- by Functor Optional
      ,                     Full (                (+1) 3, 0 )                                                    -- by application
      ]
      (Full (4,0))

-- | Implement the `Apply` instance for @StateT s f@ given a @Bind f@.
--
-- >>> runStateT (pure (+2) <*> ((pure 2) :: StateT Int List Int)) 0
-- [(4,0)]
--
-- >>> import qualified Prelude as P
-- >>> runStateT (StateT (\s -> Full ((+2), s P.++ [1])) <*> (StateT (\s -> Full (2, s P.++ [2])))) [0]
-- Full (4,[0,1,2])
instance Bind f => Apply (StateT s f) where
  (<*>) ::
    StateT s f (a -> b)
    -> StateT s f a
    -> StateT s f b
  StateT f <*> StateT a =
    -- StateT (\s -> (\(g, t) -> (\(z, u) -> (g z, u)) <$> a t) =<< f s)
    StateT ((\(g, t) -> first g <$> a t) <=< f)

-- | Implement the `Applicative` instance for @StateT s f@ given a @Applicative f@.
--
-- >>> runStateT (pure 2) 0
-- (2,0)
--
-- >>> runStateT ((pure 2) :: StateT Int List Int) 0
-- [(2,0)]
instance Monad f => Applicative (StateT s f) where
  pure ::
    a
    -> StateT s f a
  pure a = StateT (\s -> pure (a, s))

-- | Implement the `Bind` instance for @StateT s f@ given a @Monad f@.
-- Make sure the state value is passed through in `bind`.
--
-- >>> runStateT ((const $ putT 2) =<< putT 1) 0
-- ((),2)
instance Monad f => Bind (StateT s f) where
  (=<<) ::
    (a -> StateT s f b)
    -> StateT s f a
    -> StateT s f b
  f =<< StateT k = StateT ((=<<) (\(a, t) -> runStateT (f a) t) . k)
{-
  k =<< m = StateT (\s -> runStateT m s >>= \(a, s') ->
                          runStateT (k a) s')
-}
instance Monad f => Monad (StateT s f) where

-- | A `State'` is `StateT` specialised to the `Id` functor.
type State' s a =
  StateT s Id a

-- | Provide a constructor for `State'` values
--
-- >>> runStateT (state' $ runState $ put 1) 0
-- Id ((),1)
state' ::
  (s -> (a, s))
  -> State' s a
state' k = StateT (Id . k)
{-
state' f = StateT (\s -> Id (f s))
-}
-- | Provide an unwrapper for `State'` values.
--
-- >>> runState' (state' $ runState $ put 1) 0
-- ((),1)
runState' ::
  State' s a
  -> s
  -> (a, s)
runState' (StateT k) = runId . k
{-
runState' st s = let (Id p) = runStateT st s in p
-}
-- | Run the `StateT` seeded with `s` and retrieve the resulting state.
execT ::
  Functor f =>
  StateT s f a
  -> s
  -> f s
execT (StateT k) = (<$>) snd . k

-- | Run the `State` seeded with `s` and retrieve the resulting state.
exec' ::
  State' s a
  -> s
  -> s
exec' t = runId . execT t
{-
exec' s' a = snd $ runState' s' a
-}
-- | Run the `StateT` seeded with `s` and retrieve the resulting value.
evalT ::
  Functor f =>
  StateT s f a
  -> s
  -> f a
evalT (StateT k) = (<$>) fst . k

-- | Run the `State` seeded with `s` and retrieve the resulting value.
eval' ::
  State' s a
  -> s
  -> a
eval' t = runId . evalT t
{-
eval' s' a = fst $ runState' s' a
-}
-- | A `StateT` where the state also distributes into the produced value.
--
-- >>> (runStateT (getT :: StateT Int List Int) 3)
-- [(3,3)]
getT ::
  Monad f =>
  StateT s f s
getT = StateT (\s -> pure (s, s))

-- | A `StateT` where the resulting state is seeded with the given value.
--
-- >>> runStateT (putT 2) 0
-- ((),2)
--
-- >>> runStateT (putT 2 :: StateT Int List ()) 0
-- [((),2)]
putT ::
  Monad f =>
  s
  -> StateT s f ()
putT = StateT . const . pure . (,) ()
{-
putT s = StateT $ \s -> pure ((), s)
-}
-- | Remove all duplicate elements in a `List`.
--
-- /Tip:/ Use `filtering` and `State'` with a @Data.Set#Set@.
--
-- prop> distinct' xs == distinct' (flatMap (\x -> x :. x :. Nil) xs)
distinct' ::
  (Ord a, Num a) =>
  List a
  -> List a
distinct' x = eval' (filtering (\a -> state' (S.notMember a &&& S.insert a)) x) S.empty

-- | Remove all duplicate elements in a `List`.
-- However, if you see a value greater than `100` in the list,
-- abort the computation by producing `Empty`.
--
-- /Tip:/ Use `filtering` and `StateT` over `Optional` with a @Data.Set#Set@.
--
-- >>> distinctF $ listh [1,2,3,2,1]
-- Full [1,2,3]
--
-- >>> distinctF $ listh [1,2,3,2,1,101]
-- Empty
distinctF ::
  (Ord a, Num a) =>
  List a
  -> Optional (List a)
distinctF = error "TODO/distinctF"

-- | An `OptionalT` is a functor of an `Optional` value.
data OptionalT f a =
  OptionalT {
    runOptionalT ::
      f (Optional a)
  }

-- | Implement the `Functor` instance for `OptionalT f` given a Functor f.
--
-- >>> runOptionalT $ (+1) <$> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty]
instance Functor f => Functor (OptionalT f) where
  f <$> OptionalT x = OptionalT ((<$>) f <$> x)

-- | Implement the `Apply` instance for `OptionalT f` given a Apply f.
--
-- >>> runOptionalT $ OptionalT (Full (+1) :. Full (+2) :. Nil) <*> OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Empty,Full 3,Empty]
instance Apply f => Apply (OptionalT f) where
  (<*>) = error "TODO/Apply/OptionalT"

-- | Implement the `Applicative` instance for `OptionalT f` given a Applicative f.
instance Applicative f => Applicative (OptionalT f) where
  pure =  error "TODO/Applicative/OptionalT"

-- | Implement the `Bind` instance for `OptionalT f` given a Monad f.
--
-- >>> runOptionalT $ (\a -> OptionalT (Full (a+1) :. Full (a+2) :. Nil)) =<< OptionalT (Full 1 :. Empty :. Nil)
-- [Full 2,Full 3,Empty]
instance Monad f => Bind (OptionalT f) where
  (=<<) = error "TODO/Bind/OptionalT"

instance Monad f => Monad (OptionalT f) where

-- | A `Logger` is a pair of a list of log values (`[l]`) and an arbitrary value (`a`).
data Logger l a =
  Logger (List l) a
  deriving (Eq, Show)

-- | Implement the `Functor` instance for `Logger`.
--
-- >>> (+3) <$> Logger (listh [1,2]) 3
-- Logger [1,2] 6
instance Functor (Logger l) where
  (<$>) = error "TODO/Functor/Logger"

-- | Implement the `Apply` instance for `Logger`.
instance Apply (Logger l) where
  (<*>) = error "TODO/Apply/Logger"

-- | Implement the `Applicative` instance for `Logger`.
--
-- >>> pure "table" :: Logger Int P.String
-- Logger [] "table"
instance Applicative (Logger l) where
  pure = error "TODO/Applicative/Logger"

-- | Implement the `Bind` instance for `Logger`.
-- The `bind` implementation must append log values to maintain associativity.
--
-- >>> (\a -> Logger (listh [4,5]) (a+3)) =<< Logger (listh [1,2]) 3
-- Logger [1,2,4,5] 6
instance Bind (Logger l) where
  (=<<) = error "TODO/Bind/Logger"

instance Monad (Logger l) where

-- | A utility function for producing a `Logger` with one log value.
--
-- >>> log1 1 2
-- Logger [1] 2
log1 ::
  l
  -> a
  -> Logger l a
log1 = error "TODO/log1"

-- | Remove all duplicate integers from a list. Produce a log as you go.
-- If there is an element above 100, then abort the entire computation and produce no result.
-- However, always keep a log. If you abort the computation, produce a log with the value,
-- "aborting > 100: " followed by the value that caused it.
-- If you see an even number, produce a log message, "even number: " followed by the even number.
-- Other numbers produce no log message.
--
-- /Tip:/ Use `filtering` and `StateT` over (`OptionalT` over `Logger` with a @Data.Set#Set@).
--
-- >>> distinctG $ listh [1,2,3,2,6]
-- Logger ["even number: 2","even number: 2","even number: 6"] (Full [1,2,3,6])
--
-- >>> distinctG $ listh [1,2,3,2,6,106]
-- Logger ["even number: 2","even number: 2","even number: 6","aborting > 100: 106"] Empty
distinctG ::
  (Integral a, Show a) =>
  List a
  -> Logger Chars (Optional (List a))
distinctG = error "TODO/distinctG"

------------------------------------------------------------------------------

testStateT :: IO T.Counts
testStateT =
     T.runTestTT P.$ T.TestList P.$ tstf

-- End of file.
