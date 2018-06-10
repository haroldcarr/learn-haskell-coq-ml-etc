> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE StandaloneDeriving     #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module Fix where

> class Functor f => Fixpoint f t | t -> f where
>   inF  :: f t -> t
>   outF :: t -> f t
>
> instance Functor f => Fixpoint f (Fix f) where
>   inF  = Fix
>   outF = unFix
>
> newtype Fix f = Fix { unFix :: f (Fix f) }
>
> deriving instance Eq   (f (Fix f)) => Eq   (Fix f)
> deriving instance Show (f (Fix f)) => Show (Fix f)
