> {-# LANGUAGE FlexibleContexts       #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE RankNTypes             #-}
> {-# LANGUAGE ScopedTypeVariables    #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module RS where
>
> import           Control.Arrow ((&&&))
> import           Fix

> cata
>   :: Functor f
>   => (f a -> a)
>   -> Fix f
>   -> a
> cata f = f . fmap (cata f) . unFix
>
> para
>   :: Fixpoint f t
>   => (f (a, t) -> a)
>   -> t
>   -> a
> para alg = alg . fmap (para alg &&& id) . outF
>
> funzip
>   :: Functor f
>   => f (a, b)
>   -> (f a, f b)
> funzip = fmap fst &&& fmap snd
