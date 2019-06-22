module Lib where

import Control.Applicative.ListF
import Control.Applicative.Step
import Control.Monad.Freer.Church
import Control.Natural.IsoF
import Data.Functor.Apply.Free
import Data.Functor.Combinator
import Data.Functor.Combinator.Unsafe
import Data.HBifunctor
import Data.HBifunctor.Associative
import Data.HBifunctor.Tensor
import Data.HFunctor
import Data.HFunctor.Chain
import Data.HFunctor.Final
import Data.HFunctor.Interpret

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
import import Data.HFunctor
:i HFunctor
:t hmap
import Data.HBifunctor
:i HBifunctor
:t hbimap
import Control.Applicative.ListF
:i MaybeF
   newtype MaybeF (f :: * -> *) a = MaybeF {runMaybeF :: Maybe (f a)}

instance HFunctor MaybeF where
    hmap f (MaybeF xs) = MaybeF (fmap f xs)

:t MaybeF (Just [1])
   :: Num a => MaybeF []    a
:t MaybeF (Just (Just 1))
   :: Num a => MaybeF Maybe a
:t MaybeF (Just (Right 1))
   :: Num a2 => MaybeF (Either a1) a2
:t MaybeF (Just (Left 1))
   :: Num a1 => MaybeF (Either a1) a2

:t hmap _                   (MaybeF (Just [1]))
   Found hole: _ :: [x1] -> g x1
:t hmap (\[x]   -> Just x)  (MaybeF (Just [1]))
   :: Num x => MaybeF Maybe x
   hmap (\[x]   -> Just x)  (MaybeF (Just [1]))
   hmap (\[x,_] -> Just x)  (MaybeF (Just [1,2]))
   hmap (\[x,_] -> Right x) (MaybeF (Just [1,2]))

:t hmap _                   (MaybeF Nothing)
   Found hole: _ :: f0 x1 -> g x1
      Where: ‘f0’ is an ambiguous type variable
:t hmap (\[x]   -> Just x)  (MaybeF Nothing)
   :: MaybeF Maybe x
   hmap (\[x]   -> Just x)  (MaybeF Nothing)
   hmap (\[x,_] -> Just x)  (MaybeF Nothing)
   hmap (\[x,_] -> Right x) (MaybeF Nothing)

:i inject
:t inject
   :: Inject t => f x -> t f x
:t hmap (\(Just x   -> Just x)  (inject (Just [1]))
inject (Just   3 ) :: MaybeF Maybe           Int
inject        [3]  :: MaybeF []              Int
inject (Right  3 ) :: MaybeF (Either String) Int
inject (Left  "3") :: MaybeF (Either String) Int

   hmap (\[x] -> Just x) (inject [3]  :: MaybeF []    Int)
   hmap (\[x] -> Just x) (inject [3]) :: MaybeF Maybe Int

import Data.HFunctor.Interpret
:i Interpret
class Inject t => Interpret (t :: (* -> *) -> * -> *) where
  type family C (t :: (* -> *) -> * -> *) :: (* -> *) -> Constraint
  retract :: C t f =>
             t f natural-transformation-0.4:Control.Natural.~> f
  interpret :: C t g =>
               (f natural-transformation-0.4:Control.Natural.~> g)
               -> t f natural-transformation-0.4:Control.Natural.~> g

interpret (\[x] -> Just x) (inject [3]  :: MaybeF []    Int)
-}
