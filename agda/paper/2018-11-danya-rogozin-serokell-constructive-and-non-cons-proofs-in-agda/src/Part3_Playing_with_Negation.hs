{-# LANGUAGE EmptyCase  #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Part3_Playing_with_Negation where

data Void -- no constructors

type Not a = a -> Void

exFalso :: Void -> a
exFalso = \case {}

exContr :: forall a b. Not a -> a -> b
exContr f x = exFalso (f x)

exContr2 :: forall a b. (a -> Void) -> a -> b
exContr2 f x = exFalso (f x)

contraposition :: forall a b. (a -> b) -> (Not b -> Not a)
contraposition f g = g . f

notIntro :: forall a b. (a -> b) -> (a -> Not b) -> Not a
notIntro f g x = g x (f x)

disjImpl :: forall a b. Not (Either a b) -> a -> b
disjImpl = undefined -- TODO : cannot pattern match on first arg







