> module FAM where

https://medium.com/@fintan.halpenny/free-me-exploring-the-free-data-type-c863499a82f8

Edward Kmett
https://www.stackage.org/haddock/lts-9.4/free-4.12.4/Control-Monad-Free.html#t:Free

> -- an abstract syntax tree for general computation
> data Free f a
>   = Pure a
>   | Free (f (Free f a))

FUNCTOR

> -- the `f` "inside" must be a Functor (need `fmap` to access the structure)
> instance Functor f => Functor (Free f) where
>   fmap fn (Pure a) = Pure (fn a) -- no recursion, so just unwrap, apply, rewrap
>
>   -- fmap :: (a -> b) ->      f a -> f b
>   -- substitute `Free f` for `f`:
>   -- fmap :: (a -> b) -> Free f a -> Free f b
>   -- fmap _fn (Free _fr) = _x
>   --   _x :: Free f b (and `f` is a Functor)
>   --   fn :: a -> b
>   --   af :: Free f a
>   --   fr :: f (Free f a) -- says 'Free f a' inside 'f'
>   --            want to change to 'Free f b' using 'fn'
>   --            so need to get 'fn' two-levels inside
>   --            and when done "rewrap" back to starting level
>   -- fmap fn af@(Free fr) = Free $ fmap ii fr
>   --   ii :: Free f a -> Free f b
>   fmap fn af@(Free fr) = Free $ fmap (fmap fn) fr

APPLICATIVE

> -- f must be a Functor to enable 'fmap' to get "inside"
> instance Functor f => Applicative (Free f) where
> -- pure :: a -> Free f a
>   pure = Pure
>
> -- (<*>) :: Free f (a -> b) -> Free f a -> Free f b
> -- CASE 1
>   (Pure f) <*> (Pure a) = Pure $ f a -- Unwrap fun and val, apply, rewrap.
>
> -- CASE 2
>   (Pure f) <*> (Free freer) = Free $ fmap (fmap f) freer -- right same as fmap, so:
> -- (Pure f) <*> freer = fmap f freer
>
> -- CASE 3
> -- need to unwrap some structure
> -- need to apply <*> recursively
> -- to get inside the following, use fmap
> --   freeFunc :: f (Free f (a -> ... a ... -> b))
> --   freer    :: f (Free f a)
> -- (Free freeFunc) <*> (Free freer) = _innerFunc Main.<*> _innerFreer
> --
> --   _innerFreer :: Free f t, and need to use freer
> -- (Free freeFunc) <*> (Free freer) =
> --    Free $ fmap (\innerFunc -> innerFunc Main.<*> _innerFreer) freeFunc
> --
> -- (Free freeFunc) <*> (Free freer) =
> --    Free $ fmap (\innerFunc -> fmap (\inner -> innerFunc Main.<*> inner) freer) freeFunc
> --    Expected type: Free (Free f) a
> --      Actual type: Free f a
>
> -- Missing rewrap, so
>   (Free freeFunc) <*> (Free freer) =
>      Free $ fmap (\innerFunc -> Free $ fmap (\inner -> innerFunc <*> inner)
>                   freer)
>             freeFunc
>
> -- CASE 4
> --  freeFunc :: f (Free f (a -> b))
> --  a        :: a
> --  _what    :: Free f a
> --  (Free freeFunc) <*> (Pure a) =
> --     Free $ fmap (\innerFunc -> innerFunc <*> _what) freeFunc
>
>   (Free freeFunc) <*> (Pure a) =
>      Free $ fmap (\innerFunc -> innerFunc <*> Pure a) freeFunc
>   -- alternate/better:
>   --  freeFunc :: f (Free f (a -> b))
>   --  freer    :: Free f a
>   -- apply <*> recursively
>   -- (Free freeFunc) <*> freer = Free $ fmap (<*> freer) freeFunc
>   -- fmap into the inner part of freeFunc, and use <*> to get at Free a (ie freer)

MONAD

> instance Functor f => Monad (Free f) where
>   -- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
>   (Pure a) >>= k = k a -- unwrap a, apply k (k returns Free f b)

. Moving onto the Free constructor case.
instance Functor f => Monad (Free f) where (Pure a) >>= k = k a (Free freer) >>= k =
_iWantToBreakFree Hmmmm, what should we do? Oh ya, the same as every other time! fmap over, apply
recursively and wrap it all up again.  instance Functor f => Monad (Free f) where (Pure a) >>= k = k
a (Free freer) >>= k = Free $ fmap (>>= k) freer Some Helpers From my Friends This is awesome, we
have our instances, which from the look of the free there can be many more! But instead, we will
define two functions that will help us in our endeavour of writing a DSL using Free. These two
functions will be liftF and foldFree. liftF allows us to lift any Functor into a Free structure and
foldFree allows us to fold our Free structure into a Monad. The first is useful for writing helper
functions to describe actions in the DSL and the second is helpful for interpreting our DSL into
some Monad.  Let’s look at liftF first: liftF :: Functor f => f a -> Free f a liftF =
_iWantToBreakFree My first instinct is to use the Free constructor on our Functor but that does not
give us what we need. To explain, we want a Free f a but the constructor Free needs a f (Free f
a). So to turn the inside of our Functor into a Free will utilise fmap and Pure. Our final
definition is: liftF :: Functor f => f a -> Free f a liftF = Free . fmap Pure Next up on our list is
foldFree: foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x To quickly explain, the
first parameter to foldFree is a function going from any f x to a Monad, m x. This is called a
“natural transformation” if you want to explore this concept more. The forall x. says that this
function must work for any x but we don’t know what x at this time. We then use this function to
fold down our Free f x structure to the Monad , m. Let’s get into the thick of it and implement it.
Let’s do the easy case first: foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x
foldFree _ (Pure a) = pure a We extract our a from Pure and use the Monad's pure to place in that
context instead. Next up: foldFree :: Monad m => (forall x. f x -> m x) -> Free f x -> m x foldFree
k (Free freer) = _iWantToFoldFree Let’s see what we have here: k :: f x -> m x freer :: f (Free f a)
We don’t have our trusty fmap here because we never said f was a functor, so there’s really only one
thing we can do, and that’s apply k to freer!  foldFree :: Monad m => (forall x. f x -> m x) -> Free
f x -> m x foldFree k (Free freer) = _iWantToFoldFree (k freer) Let’s just think about what this
gives us in terms of types: k :: f x -> m x freer :: f (Free f a) k freer :: m (Free f a) So when we
apply it we actually have a Monad with Free f a inside it. Well what can we do next to break down
that inner Free even further? Well since we are working with a Monad we can utilise (>>=).  foldFree
:: Monad m => (forall x. f x -> m x) -> Free f x -> m x foldFree k (Free freer) = k freer >>=
_iWantToFoldFree Again we should ask, what is the type of _iWantToFoldFree? Well using the type
signature of (>>=) we can figure this out. If k freer is m (Free f a) and our result should be m x
due to our foldFree type signature, then we would expect: (>>=) :: m (Free f a) -> (Free f a -> m x)
-> m x Hmmm, that function in the middle of our signature looks pretty familiar… That’s right! It’s
our foldFree with the natural transformation already applied! And of course we can feel at ease with
this solution because it’s recursive.  foldFree :: Monad m => (forall x. f x -> m x) -> Free f x ->
m x foldFree k (Free freer) = k freer >>= foldFree k Final Words Well damn, that turned out to be
quite the lengthy article just to explain a few functions, but this actually gives us enough power
to write as many DSLs as we want. On top of this we now understand how Free actually works and not
just how to use it! That’s something extremely useful in itself.  Something else we can learn from
this is that type driven development is amazingly useful. When we constrain ourselves to certain
typeclasses and polymorphism we can only do so many things. This way our solutions tend to “fall
out” and we can reason about what our program is doing. Then we can utilise the polymorphism,
writing the implementation once and reusing many times. Very cool!  If you want to see some actual
code with rambling comments that inspired this article you can check
https://github.com/FintanH/free-me. On top of that there’s a mini IO DSL example that everyone seems
to write as their Hello, World of Free. I will probably write about that next time. Maybe.

http://dlaing.org/cofun/posts/free_and_cofree.html
http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
https://youtu.be/eKkxmVFcd74?list=WL

https://stackoverflow.com/questions/17307416/difference-between-free-monads-and-fixpoints-of-functors
