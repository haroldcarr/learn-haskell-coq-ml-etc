{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds  #-}

-- The following is needed to define MonadPlus instance. It is decidable
-- (there is no recursion!), but GHC cannot see that.
{-# LANGUAGE UndecidableInstances #-}



-- Demo of Extensible effects
-- This is the real implementation, but stripped of INLINE pragma and other
-- performace-inducing stuff

-- There are several implementations in Haskell (see Hackages, including
-- packaging of the implementation to be presented). There are at least
-- two implementations in Scala. Extensible effects in PureScript is
-- the defining feature of the language.
-- There is even an implementation of extensible effects in Coq!


module Tutorial3_Orig where

import Control.Monad
import Control.Applicative
import OpenUnion52
import FTCQueue1

-- Example of the Drunken coin by Sam Lindley

-- Non-determinism effect
data Choice a where
  Choice :: Choice Bool

-- See the inferred type
coinflip = E (inj Choice) (tsingleton Val)


data Failure a where
  Failure :: Failure a

-- Abbreviting the E inj ... pattern
failure = send Failure

-- Explicit signature. Try to forget Failure, and see what happens
choose :: (Member Failure r, Member Choice r) => [a] -> Eff r a
choose = foldr (\x l -> do { h <- coinflip; if h then return x else l}) failure

-- Drunken coin example

drunkencoin = do
  f <- coinflip
  if f then choose [True,False]
       else failure

drunkencoins n = sequence $ replicate n drunkencoin

-- See the inferred types


-- Interpreters

-- The type of run ensures that all effects must be handled:
-- only pure computations may be run.
run :: Eff '[] w -> w
run (Val x) = x
-- the other case is unreachable since Union [] a cannot be
-- constructed.
-- Therefore, run is a total function if its argument terminates.

-- First, obtain all solutions

{-
-- Inferred type
-- runChooseAll :: Eff (Choice ': r) t -> Eff r [t]
runChooseAll (Val x) = return [x]
-- Exercise: use alternative

-- Using the handle-relay abbreviation
-- runFailure = undefined handle_relay

-- Closed handler
runFailureClosed :: Eff '[Failure] a -> Maybe a
runFailureClosed (Val x) = Just x
runFailureClosed (E u q) = case decomp u of
  Right Failure -> Nothing
  -- Nothing more happens

-- See the inferred type and the results of the following expressions

-- Hrm, Haskelll...
-- _ = runChooseAll $ drunkencoins 3
_ = runFailure . runChooseAll $ drunkencoins 3
_ = run . runFailure . runChooseAll $ drunkencoins 3
_ = runFailureClosed . runChooseAll $ drunkencoins 3

_ = run . runChooseAll . runFailure $ drunkencoins 3

-- Handle both effects at the same time
runChooseAllF (Val x) = return [x]
runChooseAllF (E u q) = case decomp u of
  Right Choice -> (++) <$>
                  runChooseAllF (qApp q True) <*>
                  runChooseAllF (qApp q False)
  Left u       -> case decomp u of
    Right Failure -> return []
    Left  u       -> E u (qComps q runChooseAllF)

_ = run . runChooseAllF $ drunkencoins 3

-- Use external source of non-determinism
runChooseSome :: Member Choice r => Eff (Choice ': r) a -> Eff r a
runChooseSome = handle_relay return (\Choice k -> k =<< coinflip)

-- What is the difference from the above? See the signature!
runChooseSome1 = interpose return (\Choice k -> k =<< coinflip)

-- The statement of the equational law
eq e = runChooseSome1 e `asTypeOf` e

runChooseSome11 = interpose return (\Choice k -> k =<< fmap not coinflip)

-- Now we clearly see the result of the superposition
_ = run . runChooseAllF . runChooseSome11 $ drunkencoins 3


-- ------------------------------------------------------------------------
-- Committed Choice

-- Soft-cut: non-deterministic if-then-else, aka Prolog's *->
-- Declaratively,
--    ifte t th el = (t >>= th) `mplus` ((not t) >> el)
-- However, t is evaluated only once. In other words, ifte t th el
-- is equivalent to t >>= th if t has at least one solution.
-- If t fails, ifte t th el is the same as el.

-- We actually implement LogicT, the non-determinism reflection,
-- of which soft-cut is one instance.
-- See the LogicT paper for an explanation
msplit :: (Member Choice r, Member Failure r) =>
          Eff r a -> Eff r (Maybe (a, Eff r a))
msplit = loop []
 where
 -- single result
 loop [] (Val x)  = return (Just (x,mzero))
 -- definite result and perhaps some others
 loop jq (Val x)  = return (Just (x, msum jq))
 -- not yet definite answer
 loop jq (E u q)  | Just Failure <- prj u =
   case jq of
                   -- no futher choices
                   []     -> return Nothing
                   -- other choices remain, try them
                   (j:jq) -> loop jq j
 loop jq (E u q)  = case prj u of
  Just Choice -> loop ((qApp q False):jq) (qApp q True)
  _      -> E u (qComps q (loop jq))

type NonDet r = (Member Choice r, Member Failure r)

-- Other committed choice primitives can be implemented in terms of msplit
-- The following implementations are directly from the LogicT paper
ifte :: NonDet r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = msplit t >>= check
 where check Nothing          = el
       check (Just (sg1,sg2)) = (th sg1) `mplus` (sg2 >>= th)

once :: NonDet r => Eff r a -> Eff r a
once m = msplit m >>= check
 where check Nothing        = mzero
       check (Just (sg1,_)) = return sg1

instance NonDet r => Alternative (Eff r) where
  empty     = failure
  m1 <|> m2 = do {x <- coinflip; if x then m1 else m2}

instance NonDet r => MonadPlus (Eff r) where
  mzero = empty
  mplus = (<|>)

-- primes (very inefficiently -- but a good example of ifte)
test_ifte = do
  n <- gen
  ifte (do
     d <- gen
     guard $ d < n && n `mod` d == 0
     -- _ <- trace ("d: " ++ show d) (return ())
    )
    (\_->mzero)
    (return n)
 where gen = msum . fmap return $ [2..30]


test_ifte_run :: [Int]
test_ifte_run = run . runChooseAllF $ test_ifte
-- [2,3,5,7,11,13,17,19,23,29]

-}


-- -----------------------------------------------------------------------
-- Open Union interface
{-
type Union (r :: [* -> *]) a
class Member (t :: k) (r :: [k])

inj    :: Member t r => t v -> Union r v
prj    :: Member t r => Union r v -> Maybe (t v)

decomp :: Union (t:r) v -> Either (Union r v) (t v)

The type of inj/prj really shows the union as a (multi)set.

decomp imposes the ordering. Dissatisfaction. What we really need
is something like the local instances with the closure seamntics.
And Haskell almost has what we need! (Implicit parameters).
-}


-- ------------------------------------------------------------------------
-- A monadic library for communication between a handler and
-- its client, the administered computation

data Eff r a where                      -- Existensial, not full GADT
  Val   :: a -> Eff r a
  E :: Union r x -> Arrs r x a -> Eff r a

-- Effectful arrow type: a function from a to b that also does effects
-- denoted by r
type Arr r a b = a -> Eff r b

-- An effectful function from 'a' to 'b' that is a composition
-- of several effectful functions. The paremeter r describes the overall
-- effect.
-- The composition members are accumulated in a type-aligned queue
type Arrs r a b = FTCQueue (Eff r) a b

-- Application to the `generalized effectful function' Arrs r b w
-- A bit more understandable version
qApp :: Arrs r b w -> b -> Eff r w
qApp q x = case tviewl q of
   TOne k  -> k x
   k :| t -> bind' (k x) t
 where
   bind' :: Eff r a -> Arrs r a b -> Eff r b
   bind' (Val y) k     = qApp k y
   bind' (E u q) k = E u (q >< k)

-- Eff is still a monad and a functor (and Applicative)
-- (despite the lack of the Functor constraint)

instance Functor (Eff r) where
  fmap f (Val x) = Val (f x)
  fmap f (E u q) = E u (q |> (Val . f)) -- does no mapping yet!

instance Applicative (Eff r) where
  pure = Val
  Val f <*> Val x = Val $ f x
  Val f <*> E u q = E u (q |> (Val . f))
  E u q <*> Val x = E u (q |> (Val . ($ x)))
  E u q <*> m      = E u (q |> (`fmap` m))
  
instance Monad (Eff r) where
  return x = Val x
  Val x >>= k = k x
  E u q >>= k = E u (q |> k)          -- just accumulates continuations

-- send a request and wait for a reply
send :: Member t r => t v -> Eff r v
send t = E (inj t) (tsingleton Val)
-- This seems to be a very beneficial rule! On micro-benchmarks, cuts
-- the needed memory in half and speeds up almost twice.
{-# RULES
  "send/bind" [~3] forall t k. send t >>= k = E (inj t) (tsingleton k)
 #-}


-- A convenient pattern: given a request (open union), either
-- handle it or relay it.
handle_relay :: (a -> Eff r w) ->
                (forall v. t v -> Arr r v w -> Eff r w) ->
                Eff (t ': r) a -> Eff r w
handle_relay ret h m = loop m
 where
  loop (Val x)  = ret x
  loop (E u q)  = case decomp u of
    Right x -> h x k
    Left  u -> E u (tsingleton k)
   where k = qComp q loop

-- Add something like Control.Exception.catches? It could be useful
-- for control with cut.

-- Intercept the request and possibly reply to it, but leave it unhandled
-- (that's why we use the same r all throuout)
{-# INLINE interpose #-}
interpose :: Member t r =>
             (a -> Eff r w) -> (forall v. t v -> Arr r v w -> Eff r w) ->
             Eff r a -> Eff r w
interpose ret h m = loop m
 where
   loop (Val x)  = ret x
   loop (E u q)  = case prj u of
     Just x -> h x k
     _      -> E u (tsingleton k)
    where k = qComp q loop

-- Compose effectful arrows (and possibly change the effect!)
qComp :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arr r' a c
-- qComp g h = (h . (g `qApp`))
qComp g h = \a -> h $ qApp g a

qComps :: Arrs r a b -> (Eff r b -> Eff r' c) -> Arrs r' a c
qComps g h = tsingleton $ \a -> h $ qApp g a

