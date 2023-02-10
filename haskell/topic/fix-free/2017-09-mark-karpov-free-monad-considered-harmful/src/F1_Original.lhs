> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE LambdaCase                 #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE RankNTypes                 #-}
> {-# LANGUAGE StandaloneDeriving         #-}
> {-# LANGUAGE UndecidableInstances       #-}
>
> module F1_Original where
>
> import           Control.Monad.State
> import           Prelude   as P
> import qualified System.IO as SIO

https://markkarpov.com/post/free-monad-considered-harmful.html
Mark Karpov
Free monad considered harmful
Published on September 27, 2017, last updated September 29, 2017

advising against free monads

posts on Free
http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html
http://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html

>
> data Free f a
>   = Pure a
>   | Free (f (Free f a))
>
> deriving instance (Eq   a, Eq   (f (Free f a))) => Eq   (Free f a)
> deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
>
> unpure          :: Free f a ->           a
> unpure (Pure  a) =  a
> unpure (Free  _) = error "unpure (Free  _)"
> unfree          :: Free f a -> f (Free f a)
> unfree (Free fa) = fa
> unfree (Pure  _) = error "unfree (Pure  _)"
>
> instance Functor f => Functor (Free f) where
>   fmap f (Pure a)   = Pure (f a)
>   fmap f (Free x)   = Free (fmap f <$> x)
>
> instance Functor f => Applicative (Free f) where
>   pure = Pure
>   Pure f <*> Pure a = Pure (f a)
>   Pure f <*> Free x = Free (fmap f <$> x)
>   Free x <*> my     = Free ((<*> my) <$> x)
>
> instance Functor f => Monad (Free f) where
>   return = pure
>   Pure a >>= f      = f a
>   Free x >>= f      = Free ((>>= f) <$> x)

a free monad can only build a data structure representing monadic computation
- can then be inspected or interpreted

> -- | the specific functor algebra
> data Terminal a
>   = GetLine  (String -> a)
>   | PrintLine String    a
>   deriving Functor
>
> instance Show a => Show (Terminal a) where
>   show (GetLine _f) = "GetLine f"
>   show (PrintLine s a) = "PrintLine " ++ s ++ " " ++ show a

:t GetLine id
 :: Terminal String

:t PrintLine "x" ()
 :: Terminal ()

> -- |  the free monad
> type TerminalM = Free Terminal
>
> -- | an action in the free monad
> getLineF :: TerminalM String
> getLineF  = Free (GetLine return)

:t GetLine return
 :: Monad m => Terminal (m String)
:t GetLine return :: Terminal (Maybe String)
 :: Terminal (Maybe String)
:t GetLine return :: Terminal [String]
 :: Terminal [String]

:t Free (GetLine (\_ -> return 'c'))
 :: Free Terminal Char
:t Free (GetLine (\_ -> return "s"))
 :: Free Terminal [Char]
:t Free (GetLine (\_ -> return 1))
 :: Num a => Free Terminal a

> -- | lift a functor value into the free monad
> liftF :: Functor f => f a -> Free f a
> liftF  = Free . fmap return

:t liftF (Just 3)
 :: Num a => Free Maybe a
liftF (Just 3)
 Free (Just (Pure 3))

> -- | an action in the free monad
> printLineF :: String -> TerminalM ()
> printLineF str = liftF (PrintLine str ())

:t PrintLine "s" ()
 :: Terminal ()
:t PrintLine "s" 1
 :: Num a => Terminal a
:t liftF (PrintLine "s" ())
 :: Free Terminal ()

> -- | an entire program consisting of actions bound together using monadic bind
> myProgramF :: TerminalM ()
> myProgramF  = do
>   a <- getLineF
>   b <- getLineF
>   printLineF (a ++ b)

:t unfree myProgramF
 :: Terminal (Free Terminal ())
let (GetLine gl1) = unfree myProgramF
:t x
 :: String -> Free Terminal ()
:t unfree (gl1 "s")
let (GetLine gl2) = unfree (gl1 "s")
let (PrintLine s r) = unfree (gl2 "s")
:t s
 :: String
:t r
 r :: Free Terminal ()
:t unfree r
 :: Terminal (Free Terminal ())
r
=> Pure ()

could interpret or transform 'myProgram' value

> -- | definition of free monad : given a natural transformation, get a monad homomorphism
> foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
> foldFree _ (Pure a)  = return a
> foldFree f (Free as) = f as >>= foldFree f

> interpretFIO :: TerminalM a -> IO a
> interpretFIO  = foldFree $ \case
>   GetLine       next -> next <$> SIO.getLine
>   PrintLine str next -> next <$  putStrLn str

:t interpretFIO myProgramF
 :: IO ()

------------------------------------------------------------------------------
Inspection

free monads are not that easy to inspect

to analyze 'myProgram' need to provide an environment for getting through its layers
- e.g., way to generate values getLines would return
  (i.e., need to give something to the function inside GetLine to make it produce next action)
- since its a monad, further actions can depend on those values

functions will almost inevitably be in the functor you build upon (as in GetLine)
- functions are not inspectable

------------------------------------------------------------------------------
Efficiency

look at (>>=) for free monad

on each use (>>=)
- the whole structure accumulated so far needs to be traversed with fmap
- then at the end (where Pure thing hangs) (>>= f) will be applied
  and chances are the "snake" will grow by one layer

efforts to improve : Freer monads, more extensible effects
- http://okmij.org/ftp/Haskell/extensible/more.pdf
- lighten prerequisites on type f (doesn’t need to be a Functor)
- solution involves storing function (a Kleisli arrow) to apply to some initial value and
  just composing functions on the right hand side of (>>=) with that using Kleisli composition
  similar to the approach based on coyoneda lemma for functors.
- not common to see them used yet
- better than free monads with respect to efficiency of building of actual data structure

------------------------------------------------------------------------------
Composability

issue: combining code in free monads with different functor types
- solved by making the actual functor type polymorphic
- then using functor injection as shown in : http://degoes.net/articles/modern-fp-part-2

example: add logging functor to terminal

> data Log a
>   = Log String a
>   deriving Functor
>
> class Monad m => MonadLog m where
>   logM        :: String -> m ()
>
> class (Functor f, Functor g) => Inject f g where
>   inject      :: f a -> g a
>   project     :: g a -> Maybe (f a)
>
> getLineC      :: Inject Terminal f => Free f String
> getLineC       = Free (inject $ GetLine return)
>
> printLineC    :: Inject Terminal f => String -> Free f ()
> printLineC str = liftF (inject $ PrintLine str ())
>
> logC          :: Inject Log f => String -> Free f ()
> logC       str = liftF (inject $ Log str ())
>
> myProgramC    :: (Inject Terminal f, Inject Log f) => Free f ()
> myProgramC = do
>   a <- getLineC
>   b <- getLineC
>   logC b
>   printLineC (a ++ b)

above gives composable code with free monads

is it the best way to write code?

------------------------------------------------------------------------------
A better solution

want to be able to interpret a monadic action in different ways, inspect/transform it, etc.

existing Haskell mechanism to give different concrete meanings
to same abstract (i.e., polymorphic) thing: type classes

> -- | abstract actions related to working with a terminal
> class Monad m => MonadTerm m where
>   getLineM   :: m String
>   printLineM :: String -> m ()
>
> myProgramM :: MonadTerm m => m () -- TerminalM ()
> myProgramM  = do
>   a <- getLineM
>   b <- getLineM
>   printLineM (a ++ b)

------------------------------------------------------------------------------
Efficiency

efficient IO impl:

> instance MonadTerm IO where
>   getLineM   = SIO.getLine
>   printLineM = P.print

more complex application using ReaderT design pattern:
https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

instance (HasMyEnvA r, HasMyEnvB r) => MonadFoo (ReaderT r IO) where
  -- …

- exception-friendly because of reader monad transformer is stateless
- context is easy to manipulate
  - advise using lenses with this setup
    - then possible to change (not only read) a specific component of abstract 'r'
    - can have a region of code with changed environment using local with Lens'
      and withReader with the more general Lens type

for performance : use INLINEABLE and SPECIALIZE pragmas
- squeeze out undesirable polymorphism
- unless the polymorphic code is defined in the same module
  where it’s used with concrete monadic stack,
  in which case GHC is able to specialize by itself

------------------------------------------------------------------------------
Inspection

prefer writing in polymorphic monads to writing in free monads
- can recover the Free data structure by defining an instance of MonadTerm:

> instance MonadTerm TerminalM where
>   getLineM       = Free (GetLine return)
>   printLineM str = liftF (PrintLine str ())
>
> myFreeProgram :: TerminalM ()
> myFreeProgram  = myProgramM

- effects in myProgram constrained to methods of MonadTerm
- can turn into efficient runable code without first constructing
- a data structure and then interpreting it
- can do everything that can be done via writing myProgram in free monad directly

> analyzeGP :: TerminalM a -> (Int, Int, [String])
> analyzeGP t = let (_, g, p, s) = execState (a t) (['a' ..],0,0,[]) in (g,p,s)
>  where
>   a = foldFree $ \case
>     GetLine       next ->
>       get >>= \case
>         (i:is,g,p,ss) -> do
>            put (is,g+1,p  ,    ss)
>            return (next [i])
>         _             -> error "analyzeGP GetLine"
>     PrintLine str next -> do
>         (  is,g,p,ss) <- get
>         put    (is,g  ,p+1,str:ss)
>         return  next

analyzeGP myProgramF
=> (2,1,["ab"])

------------------------------------------------------------------------------
Composability

combine actions from two different type classes : merge constraints:

> myProgramMC :: (MonadTerm m, MonadLog m) => m ()
> myProgramMC  = do
>   a <- getLineM
>   logM "got a"
>   b <- getLineM
>   logM "got b"
>   printLineM (a ++ b)

------------------------------------------------------------------------------
Inspection when composed

can recover data structure as if written in free monad
i.e., transform from type class-based representation to free representation

https://github.com/gallais gave example:

> newtype LogTerm f a = LogTerm { runLogTerm :: Free f a }
>   deriving (Functor, Applicative, Monad)
>
> instance Inject Terminal f => MonadTerm (LogTerm f) where
>   getLineM       = LogTerm (Free  $ inject (GetLine return))
>   printLineM str = LogTerm (liftF $ inject (PrintLine str ()))
>
> instance Inject Log      f => MonadLog  (LogTerm f) where
>   logM str       = LogTerm (liftF $ inject (Log str ()))
>
> --toFree :: (Inject Terminal f, Inject Log f)
> --       => (forall m. (MonadTerm m, MonadLog m) => m a)
> --       -> Free f a
> toFree :: LogTerm f a -> Free f a
> toFree  = runLogTerm


------------------------------------------------------------------------------

> test :: IO ()
> test  = do
>   putStrLn "interpretFIO myProgramF"
>   interpretFIO myProgramF
