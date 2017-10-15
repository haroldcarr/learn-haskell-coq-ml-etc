{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- ** `Practical' denotational semantics

module Tutorial1_Orig where


-- Consider a small subset of Haskell
-- (or a pure functional subset of other suitable language)
-- as an `executable Math'
-- How to deal with side-effects, like input?

-- Simple sub-language of effectful integer
-- expressions, embedded in Haskell

-- Define by denotational semantics: giving meaning to each
-- expression and how to compose them
-- Don't commit to a particular domain yet

class ReaderLang d where
  int :: Int -> d            -- Int literals
  add :: d -> d -> d
  ask :: d

-- Sample expression
rlExp = add ask (add ask (int 1))



-- What should be that d?

-- EDLS, pp 2 and 6

-- EDLS, p7
-- * Implementing Math

data CRead = CRVal Int | Get0 (Int -> CRead)

instance ReaderLang CRead where
  int x = CRVal x
  ask   = Get0 CRVal

  -- t1 = ask + ask
  -- p9

  add (CRVal x) (CRVal y)  = CRVal (x+y)
  add (Get0 k)   y         = Get0 (\x -> add (k x) y)
  add x         (Get0 k)   = Get0 (\y -> add x (k y))


-- The meaning of rlExp in that domain
_ = rlExp :: CRead


-- Need authority (admin)!

-- p 11
runReader0 :: Int -> CRead -> Int
runReader0 e (CRVal x)  = x
runReader0 e (Get0 k)   = runReader0 e $ k e

_ = runReader0 2 rlExp


-- CRead is too particular semantic domain: nothing but Int value
-- (computations)

-- Need something more general

-- Again, p7

{-
data Comp req a where
  Val :: a   -> Comp req a
  E   :: _1 -> (_2 -> Comp req a) -> Comp req a

-- Effect signature
data Get x where
  Get :: Get Int

instance ReaderLang (Comp Get Int) where
  int x = Val x
  ask   = E Get Val

  add (Val x) (Val y)  = Val (x+y)
  add (E r k)   y      = E r (\x -> add (k x) y)
  add x       (E r k)  = E r (\y -> add x (k y))

-- How to extend to other types of env?

runReader :: Int -> Comp Get a -> a
runReader e (Val x)   = x
runReader e (E Get k) = runReader e $ k e

_ = runReader 2 rlExp :: Int

-- If we need subtraction, should we write the last two clauses
-- over again?
-- Generalizing even more

-- p7, Fig 3

inV :: a -> Comp req a
inV = Val

bind :: Comp req a -> (a -> Comp req b) -> Comp req b
bind (Val x) f = f x
bind (E r k) f = E r (\x -> bind (k x) f)


-- We can easily write even richer Reader languages, uniformly

rlExp2 =
  bind ask $ \x ->
  bind ask $ \y ->
  Val (x + y + 1)

-- with multiplication, subtraction, end re-using previous expressions

rlExp3 =
  bind rlExp2 $ \x ->
  bind ask $ \y ->
  Val (x * y - 1)

_ = runReader 2 rlExp3 :: Int

-}
