{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
-- {-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module NP where

import           Data.Proxy
import           GHC.Exts   (Constraint)

{-# ANN module ("HLint: ignore Avoid lambda"  :: String) #-}
{-# ANN module ("HLint: ignore Eta reduce"    :: String) #-}
{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- data List a = Nil | Cons a (List a)

{-
data List :: * -> * where
  Nil   ::  List a
  Cons  ::  a -> List a -> List a

data HList :: [*] -> * where
  HNil  ::  HList '[]
  HCons ::  x -> HList xs -> HList (x ': xs)

infixr 5 `HCons`
-}

data NP :: (k -> *) -> [k] -> * where
  Nil  ::  NP f '[]
  (:*) ::  f x -> NP f xs -> NP f (x ': xs)

-- All :: (* -> Constraint) -> [*] -> Constraint

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

-- UndecidableSuperClasses
class (f (g x)) => Compose f g x
instance (f (g x)) => Compose f g x

deriving instance All (Compose Show f) xs => Show (NP f xs)

infixr 5 :*

newtype I a = I a
  deriving Show

newtype K a b = K a
  deriving Show

map_NP :: (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP _ Nil       = Nil
map_NP f (x :* xs) = f x :* map_NP f xs

eq_NP :: (All Eq xs) => NP I xs -> NP I xs -> Bool
eq_NP Nil         Nil         = True
eq_NP (I x :* xs) (I y :* ys) = x == y && eq_NP xs ys
-- eq_NP _           _           = error "impossible"

cmap_NP :: (All c xs) => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmap_NP _ _ Nil       = Nil
cmap_NP p f (x :* xs) = f x :* cmap_NP p f xs


-- pure  :: a -> f a

class Pure_NP (xs :: [k]) where
  pure_NP' :: (forall x . f x) -> NP f xs

instance Pure_NP '[] where
  pure_NP' _x = Nil

instance (Pure_NP xs) => Pure_NP (x ': xs) where
  pure_NP' x = x :* pure_NP' x

class SingI (xs :: [k]) where
  sing :: Sing xs

instance SingI '[] where
  sing = SNil

instance (SingI xs) => SingI (x ': xs) where
  sing = SCons

data Sing :: [k] -> * where
  SNil  :: Sing '[]
  SCons :: SingI xs => Sing (x ': xs)

pure_NP :: SingI xs => (forall x . f x) -> NP f xs
pure_NP x = go sing x
  where
    go :: Sing xs -> (forall x . f x) -> NP f xs
    go SNil  _ = Nil
    go SCons y = y :* pure_NP y
{-
cpure_NP :: (SingI xs, All c xs) => Proxy c -> (forall x . c x => f x) -> NP f xs
cpure_NP = undefined
-}
-- (<*>) :: f (a -> b) -> f a -> f b

ap_NP :: NP (f -.-> g) xs -> NP f xs -> NP g xs
ap_NP Nil          Nil       = Nil
ap_NP (Fn f :* fs) (x :* xs) = f x :* ap_NP fs xs
-- ap_NP _            _         = error "impossible"

newtype (f -.-> g) x = Fn (f x -> g x)

map_NP' :: SingI xs => (forall x . f x -> g x) -> NP f xs -> NP g xs
map_NP' f xs = pure_NP (Fn f) `ap_NP` xs

zipWith_NP :: SingI xs => (forall x . f x -> g x -> h x) -> NP f xs -> NP g xs -> NP h xs
zipWith_NP f xs ys = pure_NP (fn_2 f) `ap_NP` xs `ap_NP` ys
{-
czipWith_NP :: (All c xs, SingI xs)
            => Proxy c
            -> (forall x . c x => f x -> g x -> h x)
            -> NP f xs -> NP g xs -> NP h xs
czipWith_NP p f xs ys = cpure_NP p (fn_2 f) `ap_NP` xs `ap_NP` ys
-}
fn_2
  :: forall k (f1 :: k -> *) (x :: k) (f2 :: k -> *) (g :: k -> *)
   . (f1 x -> f2 x -> g x)
  -> (-.->)
  f1
  (f2 -.-> g) x
fn_2 f = Fn (\ x -> Fn (\ y -> f x y))

{-
eq_NP' :: (All Eq xs, SingI xs) => NP I xs -> NP I xs -> Bool
eq_NP' xs ys =
    and
  $ collapse_NP
  $ czipWith_NP (Proxy :: Proxy Eq) (\ (I x) (I y) -> K (x == y)) xs ys
-}
collapse_NP :: NP (K a) xs -> [a]
collapse_NP Nil         = []
collapse_NP (K x :* xs) = x : collapse_NP xs
