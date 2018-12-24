{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing          #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib where

import Data.Kind

{-# ANN module "HLint: ignore Eta reduce" #-}

-- ===========================================================================
-- type constructors
-- type functions that take type(s) as arguments and return a type

data MaybeD  a
  = NothingD
  | JustD a

data MaybeG  a             where
    NothingG  ::                          MaybeG  a
    JustG     ::                     a -> MaybeG  a

data MaybeG' a             where
    NothingG' ::                          MaybeG' a
    JustG'    :: forall  a         . a -> MaybeG' a

data Maybe :: Type -> Type where
    Nothing   :: forall (a :: Type).      Maybe   a
    Just      :: forall (a :: Type). a -> Maybe   a

------------------------------------------------------------------------------

data EitherD  a b
  = LeftD  a
  | RightD b

data EitherG  a b                   where
    LeftG   ::                                 a -> EitherG  a b
    RightG  ::                                 b -> EitherG  a b

data EitherG' a b                   where
    LeftG'  :: forall  a           b         . a -> EitherG' a b
    RightG' :: forall  a           b         . b -> EitherG' a b

data Either :: Type -> Type -> Type where
    Left    :: forall (a :: Type) (b :: Type). a -> Either   a b
    Right   :: forall (a :: Type) (b :: Type). b -> Either   a b

-- ===========================================================================
-- higher kinds
-- type functions that take type functions(s) as arguments

-- functor

class Functor0  f                  where
  fmap0 ::                                 (a -> b) -> f a -> f b

class Functor1 (f :: Type -> Type) where
  fmap1 ::                                 (a -> b) -> f a -> f b

class Functor2 (f :: Type -> Type) where
  fmap2 :: forall (a :: Type) (b :: Type). (a -> b) -> f a -> f b

class Functor3  (f :: Type -> Type) where
  type FnctorCnstrt3 (f :: Type -> Type) (x :: Type) :: Constraint
  type FnctorCnstrt3 f x  = ()
  fmap3 :: forall (a :: Type) (b :: Type)
         . (FnctorCnstrt3 f a, FnctorCnstrt3 f b)
        =>                                 (a -> b) -> f a -> f b

class Functor   (f :: Type -> Type) where
  type FnctorCnstrt (f :: Type -> Type) (x :: Type) :: Constraint
  type FnctorCnstrt f x  = ()
  fmap  :: forall (a :: Type) (b :: Type)
         . (FnctorCnstrt f a, FnctorCnstrt f b)
        =>                    ((a :: Type) -> (b :: Type))
        -> (f :: Type -> Type) (a :: Type)
        -> (f :: Type -> Type)                (b :: Type)

instance Functor     MaybeD where
  fmap _  NothingD = NothingD
  fmap f (JustD a) = JustD   (f a)

instance Functor    (Maybe :: Type -> Type) where
  type FnctorCnstrt (Maybe :: Type -> Type) (a :: Type) = ()
  fmap _  Nothing  = Nothing
  fmap f (Just  a) = Just    (f a)

instance Functor    (EitherD a) where
  fmap _ (LeftD  a) = LeftD  a
  fmap f (RightD b) = RightD (f b)

instance Functor    (Either (a :: Type) :: Type -> Type) where
  type FnctorCnstrt (Either (a :: Type) :: Type -> Type) (b :: Type) = ()
  fmap _ (Left   a) = Left   a     :: Either a b
  fmap f (Right  b) = Right  (f b) :: Either a _ -- TODO

-- applicative

class Functor f => Applicative0 f where
    pure0   ::                  a -> f a
    liftA20 ::  f (a -> b) -> f a -> f b

class Functor f                   => Applicative1 f                  where
    pure1   ::                       a
            ->  f                    a
    liftA21 ::  f                   (a          ->  b)
            ->  f                    a
            ->  f                                   b

class Functor (f :: Type -> Type) => Applicative (f :: Type -> Type) where
    type ApplicativeConstraint (f :: Type -> Type) (x :: Type) :: Constraint
    type ApplicativeConstraint f x = FnctorCnstrt f x
    pure    ::   ApplicativeConstraint (f :: Type -> Type) (a :: Type)
            =>                      (a :: Type)
            -> (f :: Type -> Type)  (a :: Type)
    liftA2  :: ( ApplicativeConstraint (f :: Type -> Type) (a :: Type)
               , ApplicativeConstraint (f :: Type -> Type) (b :: Type) )
            => (f :: Type -> Type) ((a :: Type) -> (b :: Type))
            -> (f :: Type -> Type)  (a :: Type)
            -> (f :: Type -> Type)                 (b :: Type)

instance Applicative Maybe where
    pure                  a  = Just    a
    liftA2  Nothing       _  = Nothing
    liftA2       _  Nothing  = Nothing
    liftA2 (Just f) (Just a) = Just (f a)

-- monad

class Applicative  m                  => Monad0  m                  where
    return0 ::                                   a               -> m a
    bind0   ::                                 m a -> (a -> m b) -> m b

class Applicative (m :: Type -> Type) => Monad1 (m :: Type -> Type) where
    return1 :: forall  a                     .   a               -> m a
    bind1   :: forall  a           b         . m a -> (a -> m b) -> m b

class Applicative (m :: Type -> Type) => Monad2 (m :: Type -> Type) where
    return2 :: forall (a :: Type)            .   a               -> m a
    bind2   :: forall (a :: Type) (b :: Type). m a -> (a -> m b) -> m b

class Applicative (m :: Type -> Type) => Monad  (m :: Type -> Type) where
    type MonadConstraint (m :: Type -> Type) (x :: Type) :: Constraint
    type MonadConstraint f x = ApplicativeConstraint f x
    return  :: ( MonadConstraint m a )
            => forall (a :: Type)            .   a               -> m a
    bind    :: ( MonadConstraint m a, MonadConstraint m b )
            => forall (a :: Type) (b :: Type). m a -> (a -> m b) -> m b
