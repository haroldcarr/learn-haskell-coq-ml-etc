> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# LANGUAGE LambdaCase           #-}
> {-# LANGUAGE RankNTypes           #-}
> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module FAM where
>
> import           Data.Monoid     (Sum (..))
> import           Prelude         hiding (pi)
> import           Test.HUnit      (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util as U (t, tt)
>
> {-# ANN module ("HLint: ignore Use <$>" :: String) #-}

how Free actually works via type driven development
https://medium.com/@fintan.halpenny/free-me-exploring-the-free-data-type-c863499a82f8

Edward Kmett
https://www.stackage.org/haddock/lts-9.4/free-4.12.4/Control-Monad-Free.html#t:Free

------------------------------------------------------------------------------
FREE

Use : an abstract syntax tree for general computation

> -- | `f` is structure to be made recursive
> --   `a' is base value
> data Free f a
>   = Pure a              -- ^ base case of value of type `a`
>   | Free (f (Free f a)) -- ^ recursion on `f` of type `Free f a`
>
> deriving instance (Eq   a, Eq   (f (Free f a))) => Eq   (Free f a)
> deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

https://hackage.haskell.org/package/free-4.12.1/docs/src/Control-Monad-Free.html#Free

~~~{.haskell}
instance (Functor f, Eq1 f) => Eq1 (Free f) where
  Pure  a ==# Pure  b =             a ==              b
  Free fa ==# Free fb = fmap Lift1 fa ==# fmap Lift1 fb
  _       ==# _       = False

instance (Eq (f (Free f a)), Eq a) => Eq (Free f a) where
  Pure  a == Pure  b =  a ==  b
  Free fa == Free fb = fa == fb
  _       == _       = False
~~~

- to compare values of type (Free f a) for equality
  - need to be able to compare the constructors : `Pure` and `Free`
  - and the fields of the constructors : `a` and `fa :: (f (Free f a))`
- circular because Free is circular
- GHC uses the provided instance in the implementation, yielding recursion.
  - means it may go into an infinite loop if the instances are defined that way

What can you do with the above ADT?  Projections:

> unpure          :: Free f a ->           a
> unpure (Pure  a) =  a
> unpure        _  = undefined
> unfree          :: Free f a -> f (Free f a)
> unfree (Free fa) = fa
> unfree        _  = undefined

See: FREE_ONLY

------------------------------------------------------------------------------
FUNCTOR

> -- | `f` "inside" `Free` a Functor so can use `fmap` to access structure
> instance Functor f => Functor (Free f) where
>   -- fmap :: (a -> b) ->      f a ->      f b
>   -- substitute `Free f` for `f`:
>   -- fmap :: (a -> b) -> Free f a -> Free f b
>   fmap fn (Pure a) = Pure (fn a) -- no recursion, so just unwrap, apply, rewrap
>
>   -- fmap fn af@(Free fa) = undefined
>   -- fmap fn af@(Free fa) = _x
>   --   _x :: Free f b (`f` is a Functor)
>   --   fn :: a -> b
>   --   af :: Free f a
>   --   fa :: f (Free f a)
>   --         says type 'Free f a' inside Functor 'f'
>   --         want to change to type 'Free f b' using 'fn'
>   --         need to get 'fn' two-levels inside
>   --         - the `Free` level
>   --         - the level of the specific `Functor`
>   --         then "rewrap" back to starting level
>   -- fmap fn (Free fa) = Free $ fmap _ii fa
>   --  _ii :: Free f a -> Free f b
>   --         _ii is the fmap we are defining partially applied : fmap fn
>   fmap fn (Free fa) = Free $ fmap (fmap fn) fa
>   --                         ^     ^
>   --                    functor    Free
>   --                    level      level

Why Free : generic recursion.

Without Free, each level of recursion shows up in the type signature

> jjj :: Maybe (Maybe (Maybe Int))
> jjj = Just (Just (Just 3))

With Free, type signature constant regardless of depth/kind of recursion

> pn' :: Free Maybe Int
> pn, pj, fnx, fjn, fjjjp :: Free Maybe (Maybe Int)

> -- fmap gets to the value, no matter how deep
> fmm :: Show a => Maybe a -> String
> fmm = \case Nothing -> "N"; Just a -> "J" ++ show a
> fm :: Free Maybe (Maybe Int) -> Free Maybe String
> fm = fmap fmm
> fm' :: Free Maybe Int -> Free Maybe String
> fm' = fmap show

> pn      =                           Pure  Nothing
> famPnt  = U.t "famPnt" (unpure pn)        Nothing
> famPn   = U.t "famPn"  (fm pn)     (Pure "N")
> pn'     = Pure 3
> famPn'  = U.t "famPn'" (fm' pn')   (Pure "3")

> pj      =                           Pure (Just 3)
> famPjt  = U.t "famPjt" (unpure pj)       (Just 3)
> famPj   = U.t "famPj"  (fm pj)     (Pure "J3")

> fnx     =                           Free  Nothing
> famFnt  = U.t "famFnt" (unfree fnx)       Nothing

> fjn     =                   Free (Just (Free Nothing))
> famFjn  = U.tt "famFjn"
>   [              fm        fjn
>   ,              fm        (Free (Just (Free Nothing)))
>   ,              fmap fmm  (Free (Just (Free Nothing)) ::        Free Maybe (Maybe Int))
>   , Free $ fmap (fmap fmm)       (Just (Free Nothing)  :: Maybe (Free Maybe (Maybe Int)))
>   , Free $ Just (fmap fmm              (Free Nothing   ::        Free Maybe (Maybe Int)))
>   , Free $ Just $ Free $ fmap (fmap fmm)    (Nothing   :: Maybe (Free Maybe (Maybe Int)))
>   , Free $ Just $ Free                       Nothing
>   ]
>   (Free (Just (Free Nothing)))

> fjnx    =                   Free (Just (Pure Nothing))
> famFjnx = U.tt "famFjnx"
>   [ fm fjnx
>   ,              fm        (Free (Just (Pure Nothing)))
>   ,              fmap fmm  (Free (Just (Pure Nothing)) ::        Free Maybe (Maybe Int))
>   , Free $ fmap (fmap fmm)       (Just (Pure Nothing)  :: Maybe (Free Maybe (Maybe Int)))
>   , Free $ Just (fmap fmm              (Pure Nothing   ::        Free Maybe (Maybe Int)))
>   , Free $ Just $ Pure $ fmm                (Nothing   :: Maybe (Free Maybe (Maybe Int)))
>   , Free $ Just $ Pure   "N"
>   ]
>   (Free (Just (Pure "N")))


> fjjjp   = Free (Just (Free (Just (Free (Just (Pure (Just 3)))))))
> fjjjp'  = Free (Just (Free (Just (Free (Just (Pure       3))))))
> famFffp = U.t "famFffp"
>          (fm fjjjp)
>          (Free (Just (Free (Just (Free (Just (Pure "J3")))))))
> famFffp'= U.t "famFffp'"
>          (fm' fjjjp')
>          (Free (Just (Free (Just (Free (Just (Pure      "3")))))))

> pi,fjjjpi          :: Free Maybe Int

> pi      =                          Pure 3
> famPit  = U.t "famPit" (unpure pi)      3

> fjjjpi  = Free (Just (Free (Just (Free (Just (Pure 3))))))
> famFjjjpi = U.t "famFjjjpi"
>                (unfree fjjjpi)
>                (Just (Free (Just (Free (Just (Pure 3))))))

> famPlus1 = U.t "famPlus1"
>     (fmap (+1) (Free (Just (Free (Just (Free (Just (Pure (Sum (3::Int))))))))))
>     (           Free (Just (Free (Just (Free (Just (Pure (Sum 4))))))))

------------------------------------------------------------------------------
APPLICATIVE

> -- f must be a Functor to enable 'fmap' to get "inside"
> instance Functor f => Applicative (Free f) where
> -- pure :: a -> Free f a
>   pure = Pure
>
> -- (<*>) :: Free f (a -> b) -> Free f a -> Free f b
> -- CASE 1
>   (Pure fn) <*> (Pure a) = Pure $ fn a -- Unwrap fun and val, apply, rewrap.
>
> -- CASE 2
>   (Pure fn) <*> (Free fa) = Free $ fmap (fmap fn) fa -- right same as fmap, so:
> -- (Pure fn) <*> ffa = fmap f ffa
>
> -- this covers both case 3 and 4
> -- Free ma <*> b = Free $ (<*> b) <$> ma
>
>
> -- CASE 3
> -- need to unwrap some structure
> -- need to apply <*> recursively
> -- to get inside the following, use fmap
> --   fn       :: f (Free f (a -> ... a ... -> b))
> --   fa       :: f (Free f a)
> -- (Free fn) <*> (Free fa) = _innerFunc Main.<*> _innerFa
> --
> --   _innerFa :: Free f t, and need to use fa
> -- (Free fn) <*> (Free fa) =
> --    Free $ fmap (\innerFunc -> innerFunc Main.<*> _innerFa) fn
> --
> -- (Free fn) <*> (Free fa) =
> --    Free $ fmap (\innerFunc -> fmap (\inner -> innerFunc Main.<*> inner) fa) fn
> --    Expected type: Free (Free f) a
> --      Actual type: Free f a
>
> -- Missing rewrap, so
>   (Free fn) <*> (Free fa) =
>      Free $ fmap (\innerFunc -> Free $ fmap (innerFunc <*>) fa)
>             fn
>
> -- CASE 4
> -- (This case seems to cover Case 3 as well.
> --  See: https://hackage.haskell.org/package/free-4.12.4/docs/src/Control-Monad-Free.html )
> --
> --  fn       :: f (Free f (a -> b))
> --  a        :: a
> --  _what    :: Free f a
> --  (Free fn) <*> (Pure a) =
> --     Free $ fmap (\innerFunc -> innerFunc <*> _what) fn
>
>   (Free fn) <*> (Pure a) =
>      Free $ fmap (\innerFunc -> innerFunc <*> Pure a) fn
>   -- alternate/better:
>   --  fn     :: f (Free f (a -> b))
>   --  fa     :: Free f a
>   -- apply <*> recursively
>   -- (Free fn) <*> fa = Free $ fmap (<*> fa) fn
>   -- fmap into the inner part of fn, and use <*> to get at Free a (ie fa)

> famap = U.t "famap"
>   (Pure (*) <*> Free (Just (Free (Just (Pure 3)))) <*> Free (Just (Free (Just (Pure (3::Int))))))
>                (Free (Just (Free (Just                (Free (Just (Free (Just (Pure 9)))))))))

:t (FAM.Pure (*) <*> FAM.Free (Just (FAM.Free (Just (FAM.Pure 3)))))
   :: Num a => FAM.Free Maybe (a -> a)

------------------------------------------------------------------------------
MONAD

```haskell
instance Functor f => Monad (Free f) where
  -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Pure  a) >>= k = k a -- unwrap a, apply k (k returns Free f b)
```

Free constructor case.

```haskell
instance Functor f => Monad (Free f) where
  (Pure  a) >>= k = k a
  (Free fa) >>= k = _iWantToBreakFree :: Free f b
```

Strategy : fmap over, apply recursively, wrap

> instance Functor f => Monad (Free f) where
>   (Pure a)  >>= k = k a
>   (Free fa) >>= k = Free $ fmap (>>= k) fa

two functions to help write a DSL using Free : liftF, foldFree

- liftF    : lift any Functor into a Free structure
  - useful for writing helper functions to describe actions in a DSL
- liftF first try : use Free constructor on Functor
  - but does not give what is needed
  - want `Free f a` but constructor Free needs a `f (Free f a)`
  - to turn inside of Functor into Free, utilise fmap and Pure

> liftF :: Functor f => f a -> Free f a
> liftF = Free . fmap Pure

- foldFree : fold Free structure into a Monad
  - for interpreting our DSL into some Monad

foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x

1st param is fun going from any f x to Monad, m x
- called a "natural transformation"
- use to fold `Free f x` structure to Monad `m`

> -- extract a from Pure and use Monad's pure to place a in that context instead.
> foldFree :: Monad m =>  (forall x. f x -> m x) -> Free f a -> m a
> foldFree _ (Pure a) = pure a

> -- k  :: f x -> m x
> -- fa :: f (Free f a)
> -- can't use fmap because not Functor constraint
> -- only one can be done : apply k to fa
> -- foldFree k (Free fa) = _iWantToFoldFree (k fa)
> -- _iWantToFoldFree :: m (Free f x) -> m x (where Monad m)
> -- application returns Monad with `Free f a` inside
> -- need to get to inner Free even further
> -- foldFree k (Free fa) = _iWantToFoldFree (k fa)
> -- Monad so can utilise (>>=)
> -- foldFree k (Free fa) = k fa >>= _iWantToFoldFree2
> -- _iWantToFoldFree2 :: Free f x -> m x
> -- if k fa :: m (Free f a) and result :: m x
> -- then expect: (>>=) :: m (Free f a) -> (Free f a -> m x) -> m x
> -- function in middle of sig looks familiar
> -- - foldFree with natural transformation already applied
> foldFree k (Free fa) = k fa >>= foldFree k

When constrained to certain typeclasses and polymorphism, can only do so many things.

Solutions tend to "fall out" in reasoning.

code inspired this article : https://github.com/FintanH/free-me

http://dlaing.org/cofun/posts/free_and_cofree.html
http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
https://youtu.be/eKkxmVFcd74?list=WL
https://stackoverflow.com/questions/17307416/difference-between-free-monads-and-fixpoints-of-functors

> test :: IO Counts
> test =
>   runTestTT $ TestList $
>     famPnt ++ famPn ++ famPn' ++ famPjt ++ famPj ++ famFnt ++ famFjn ++ famFjnx ++
>     famFffp ++ famFffp' ++ famPit ++ famFjjjpi ++ famPlus1 ++ famap
