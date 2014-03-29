{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Applicative(
  Applicative(..)
, sequence
, replicateA
, filtering
, return
, fail
, sequenceC , replicateAC , tsc , trc , tfe , testApplicative -- TODO: to avoid error messages
) where

import           Course.Apply
import           Course.Core
import           Course.Id
import           Course.List
import           Course.Optional
import qualified Prelude         as P

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U


-- HC: http://stackoverflow.com/questions/6570779/why-should-i-use-applicative-functors-in-functional-programming
-- Applicative functors enable using "normal" function (taking non-functorial arguments) to operate on values in functor contexts.
-- Corollary: can be used for effectful programming without monads.
-- Useful when sequencing needed but don't need to name any intermediate results.
class Apply f => Applicative f where
  pure ::
    a -> f a

-- | Witness that all things with (<*>) and pure also have (<$>).
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
(<$>) ::
  Applicative f =>
  (a -> b)
  -> f a
  -> f b
(<$>) f fa = pure f <*> fa

-- | Insert into Id.
--
-- prop> pure x == Id x
instance Applicative Id where
  pure = Id

-- | Insert into a List.
--
-- prop> pure x == x :. Nil
instance Applicative List where
  pure x = x :. Nil
--C pure   =  (:. Nil)

-- | Insert into an Optional.
--
-- prop> pure x == Full x
instance Applicative Optional where
  pure = Full

-- | Insert into a constant function.
--
-- prop> pure x y == x
instance Applicative ((->) t) where
  pure = const

-- | Sequences a list of structures to a structure of list.
--
-- >>> sequence (Id 7 :. Id 8 :. Id 9 :. Nil)
-- Id [7,8,9]
--
-- >>> sequence ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [[1,1],[1,2],[2,1],[2,2],[3,1],[3,2]]
--
-- >>> sequence (Full 7 :. Empty :. Nil)
-- Empty
--
-- >>> sequence (Full 7 :. Full 8 :. Nil)
-- Full [7,8]
--
-- >>> sequence ((*10) :. (+2) :. Nil) 6
-- [60,8]
--
-- >>> sequence ((*10) :. Nil) 6
-- [60]
--
-- >>> (pure (:.) <*> (*10) <*> (pure Nil)) 6
-- [60]
sequence ::
  Applicative f =>
  List (f a)
  -> f (List a)
sequence Nil     = pure Nil
sequence (x:.xs) = pure (:.) <*> x <*> (sequence xs)

sequenceC :: Applicative f => List (f a) -> f (List a)
sequenceC = foldRight (lift2 (:.)) (pure Nil)

tsc :: [T.Test]
tsc = U.tt "tsc"
      [ sequenceC ((*10) :. (+2) :. Nil)   6
      ,                       foldRight (lift2 (:.)) (pure Nil) ((*10) :. (+2) :. Nil)   6
      , lift2 (:.) (*10)     (foldRight (lift2 (:.)) (pure Nil) (         (+2) :. Nil))  6
      -- ...
      ]
      (60:.8:.Nil)

-- | Replicate an effect a given number of times.
--
-- >>> replicateA 4 (Id "hi")
-- Id ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 (Full "hi")
-- Full ["hi","hi","hi","hi"]
--
-- >>> replicateA 4 Empty
-- Empty
--
-- >>> replicateA 4 (*2) 5
-- [10,10,10,10]
--
-- >>> replicateA 3 ['a', 'b', 'c']
-- ["aaa","aab","aac","aba","abb","abc","aca","acb","acc","baa","bab","bac","bba","bbb","bbc","bca","bcb","bcc","caa","cab","cac","cba","cbb","cbc","cca","ccb","ccc"]
replicateA ::
  Applicative f =>
  Int
  -> f a
  -> f (List a)
replicateA i fa = sequence (replicate i fa)

replicateAC :: Applicative f => Int -> f a -> f (List a)
replicateAC n = sequence . replicate n

trc :: [T.Test]
trc = U.tt "trc"
      [ replicateAC 4 (*2) 5
      ]
      (10:.10:.10:.10:.Nil)

-- | Filter a list with a predicate that produces an effect.
--
-- >>> filtering (Id . even) (4 :. 5 :. 6 :. Nil)
-- Id [4,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. Nil)
-- Full [4,5,6]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. Nil)
-- Full [4,5,6,7]
--
-- >>> filtering (\a -> if a > 13 then Empty else Full (a <= 7)) (4 :. 5 :. 6 :. 13 :. 14 :. Nil)
-- Empty
--
-- >>> filtering (>) (4 :. 5 :. 6 :. 7 :. 8 :. 9 :. 10 :. 11 :. 12 :. Nil) 8
-- [9,10,11,12]
filtering ::
  Applicative f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
-- NO GOOD: filtering f xs = sequence (foldRight (\x acc -> if f x then pure x :. acc else acc) Nil xs)
filtering p = foldRight (\a -> lift2 (\b -> if b then (a:.) else id)
                                     (p a))
                        (pure Nil)

tfe :: [T.Test]
tfe = U.tt
 "tfe"
 [ filtering                                                   (Id . even)                (5 :. 6 :. Nil)

 , foldRight (\a -> lift2 (\b -> if b then (a:.) else id)     ((Id . even) a)) (pure Nil) (5 :. 6 :. Nil)

 -- first fold
 ,           (\a -> lift2 (\b -> if b then (a:.) else id)     ((Id . even) a))             5      (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil) (6 :. Nil))
 ,           (\a ->       (\b -> if b then (a:.) else id) <$> ((Id . even) a))             5  <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil) (6 :. Nil))
 ,                        (\b -> if b then (5:.) else id) <$> ((Id . even)                 5) <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil) (6 :. Nil))
 ,                        (\b -> if b then (5:.) else id) <$>  (Id False)                     <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil) (6 :. Nil))
 ,                    Id ((\b -> if b then (5:.) else id)          False)                     <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil) (6 :. Nil))
 ,                    Id (                            id)                                     <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil) (6 :. Nil))

 -- second fold
 , Id (id)  <*> (\a -> lift2 (\b -> if b then (a:.) else id)     ((Id . even) a)) 6     (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil)  Nil)
 , Id (id)  <*> (\a ->       (\b -> if b then (a:.) else id) <$> ((Id . even) a)) 6 <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil)  Nil)
 , Id (id)  <*>             ((\b -> if b then (6:.) else id) <$> ((Id . even) 6)    <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil)  Nil))
 , Id (id)  <*>             ((\b -> if b then (6:.) else id) <$> (Id True)          <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil)  Nil))
 , Id (id)  <*>         (Id ((\b -> if b then (6:.) else id)         True)          <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil)  Nil))
 , Id (id)  <*>         (Id                   (6:.))                                <*> (foldRight (\a -> lift2 (\b -> if b then (a:.) else id) ((Id . even) a)) (pure Nil)  Nil)

 -- third fold
 , Id (id)  <*>         (Id                   (6:.))                                <*> (pure Nil)
 , Id (id                                     (6:.))                                <*> (pure Nil)
 , Id                                         (6:.)                                 <*> (pure Nil)
 , Id                                         (6:.                                            Nil)
 ]
 (Id (6:.Nil))

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Applicative IO where
  pure =
    P.return

instance Applicative [] where
  pure =
    P.return

instance Applicative P.Maybe where
  pure =
    P.return

return ::
  Applicative f =>
  a
  -> f a
return =
  pure

fail ::
  Applicative f =>
  Chars
  -> f a
fail =
  error . hlist

------------------------------------------------------------------------------

testApplicative :: IO T.Counts
testApplicative =
    T.runTestTT P.$ T.TestList P.$ tsc P.++ trc P.++ tfe

-- End of file.
