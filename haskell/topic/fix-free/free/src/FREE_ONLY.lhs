> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module FREE_ONLY where
>
> import           Test.HUnit      (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util as U (t, tt)

------------------------------------------------------------------------------
FREE

> -- | an abstract syntax tree for general computation
> -- `f` is the structure to be made recursive
> -- `a' is the base value
> data Free f a
>   = Pure a              -- ^ base case of value of type `a`
>   | Free (f (Free f a)) -- ^ recursion on `f` of type `Free f a`
>
> deriving instance (Eq   a, Eq   (f (Free f a))) => Eq   (Free f a)
> deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

> pn,pj       ::        Free Maybe (Maybe Int)
> pn           = Pure Nothing
> pj           = Pure (Just 3)
> fjjjp       ::        Free Maybe (Maybe Int)
> fjjjp        = Free (Just (Free (Just (Free (Just (Pure (Just 3)))))))
> jjjp        :: Maybe (Free Maybe (Maybe Int))
> jjjp         =       Just (Free (Just (Free (Just (Pure (Just 3))))))
> fjjp        ::        Free Maybe (Maybe Int)
> fjjp         =             Free (Just (Free (Just (Pure (Just 3)))))

> pe,p1       ::        Free [] [Int]
> pe           = Pure []
> p1           = Pure [3]
> flllp       ::        Free [] [Int]
> flllp        = Free [Free [Free [Pure [3]]]]
> lllp        ::       [Free [] [Int]]
> lllp         =      [Free [Free [Pure [3]]]]
> fllp        ::        Free [] [Int]
> fllp         =       Free [Free [Pure [3]]]

> unpure          :: Free f a -> a
> unpure (Pure a)  = a
> unfree          :: Free f a -> f (Free f a)
> unfree (Free fa) = fa

> upn     = U.t "upn"     (unpure pn)    Nothing
> upj     = U.t "unj"     (unpure pj)    (Just 3)
> uffjjjp = U.t "uffjjjp" (unfree fjjjp) jjjp

> mapF :: (a -> b) -> Free f a -> Free f b
> mapF f (Pure  a) = Pure (f a)
> mapF f (Free fa) = undefined -- no way to get "inside" `f`

> -- if specific `f` then can do case analysis
> mapMB :: (a -> b) -> Free Maybe (Maybe a) -> Free Maybe (Maybe b)
> mapMB f (Pure  a) = Pure (fmap f a)
> mapMB f (Free fa) = case fa of
>   Nothing -> Free Nothing
>   Just j  -> Free (Just (mapMB f j)) -- note: this is the "two-levels" mentioned later

> mmbfjjjp = U.t "mmbfjjjp"
>   (mapMB (*2) fjjjp)
>   (Free (Just (Free (Just (Free (Just (Pure (Just 6))))))))

> test :: IO Counts
> test =
>   runTestTT $ TestList $ upn ++ upj ++ uffjjjp ++ mmbfjjjp
