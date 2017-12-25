 https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
 https://www.reddit.com/r/haskell/comments/3hw90k/what_is_the_reflection_package_for/

GOAL: propogate values around via types using
- Kmett's : Data.Reflection : http://hackage.haskell.org/package/reflection
- this tutorial shows how it works, what it allows

> module R0 where
>
> import Data.Proxy
> import Data.Reflection

configurations problem
- propagate run-time preferences throughout a program
- allowing multiple concurrent conﬁguration sets to coexist safely under statically guaranteed separation

GHC `ImplicitParams` can do, but have concerns

reflection API:

data Proxy k = Proxy

class Reifies s a | s -> a where
  -- turns a type into a value
  reflect :: proxy s -> a

newtype Magic a r = Magic (forall (s :: *). Reifies s a => Proxy s -> r)

-- turn a value into a type
reify :: forall a r. a -> (forall (s :: *). Reifies s a => Proxy s -> r) -> r
reify a k = unsafeCoerce (Magic k :: Magic a r) (const a) Proxy
{-# INLINE reify #-}

:t reflect
reflect :: Reifies s a => proxy s -> a

> -- e1 :: Int
> e1 = reify 10 $ reflect

> -- e2 :: String
> e2 = reify 'c' $ show . reflect

- reify value 10 over the enclosed lambda
- inside the lambda, `reflect` value to get `10 :: Int` back
- type of `reify` shows
  - lambda accepts param of type `Proxy s`
- never had any instances of `Reifies` for the types
- how does it know what value to return, given the `Proxy`?

To understand
- look at the source of `reify`
- see how it elaborates to GHC Core
