> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE LambdaCase                 #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE RankNTypes                 #-}
> {-# LANGUAGE UndecidableInstances       #-}
>
> module F3_TypeClass where
>
> import           Control.Monad.Free    (Free (Free), foldFree, liftF)
> import           Control.Monad.State
> import           Prelude   as P
> import qualified System.IO as SIO

------------------------------------------------------------------------------
A better solution

> -- | the specific functor algebra
> data Terminal a
>   = GetLine  (String -> a)
>   | PrintLine String    a
>   deriving Functor
>
> instance Show a => Show (Terminal a) where
>   show (GetLine    _f) = "GetLine f"
>   show (PrintLine s a) = "PrintLine " ++ s ++ " " ++ show a
>
> type TerminalM = Free Terminal
>
> class Monad m => MonadTerm m where
>   getLineM   :: m String
>   printLineM :: String -> m ()
>
> myProgramM :: MonadTerm m => m () -- TerminalM ()
> myProgramM  = do
>   printLineM "enter two lines"
>   a <- getLineM
>   b <- getLineM
>   printLineM (a ++ b)

------------------------------------------------------------------------------
Efficiency

efficient IO impl:

> instance MonadTerm IO where
>   getLineM   = SIO.getLine
>   printLineM = P.print
>
> runMyProgramMIO :: IO ()
> runMyProgramMIO  = do
>   putStrLn "example of running MonadTerm in IO"
>   myProgramM

F3_TypeClass.runMyProgramMIO

------------------------------------------------------------------------------
Inspection

prefer writing in polymorphic monads to writing in free monads
- can recover the Free data structure by defining an instance of MonadTerm:

> instance MonadTerm TerminalM where
>   getLineM       = Free  (GetLine pure)
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
>            pure (next [i])
>         _             -> error "analyzeGP GetLine"
>     PrintLine str next -> do
>         (  is,g,p,ss) <- get
>         put    (is,g  ,p+1,str:ss)
>         pure     next

F3_TypeClass.analyzeGP F3_TypeClass.myFreeProgram
=> (2,2,["ab","enter two lines"])

------------------------------------------------------------------------------
Composability

> class Monad m => MonadLog m where
>   logM :: String -> m ()
>
> instance MonadLog IO where
>   logM  = P.print
>
> myProgramMC :: (MonadTerm m, MonadLog m) => m ()
> myProgramMC  = do
>   printLineM "enter a line"
>   a <- getLineM
>   logM ("got : " ++ a)
>   printLineM "enter another line"
>   b <- getLineM
>   logM ("got : " ++ b)
>   printLineM (a ++ b)
>
> runMyProgramMCIO :: IO ()
> runMyProgramMCIO  = myProgramMC

F3_TypeClass.runMyProgramMCIO

------------------------------------------------------------------------------
Inspection when composed

https://github.com/gallais gave example:

> data Log a
>   = Log String a
>   deriving Functor
>
> class (Functor f, Functor g) => Inject f g where
>   inject  :: f a -> g a
>   project :: g a -> Maybe (f a)
>
> newtype LogTerm f a = LogTerm { runLogTerm :: Free f a }
>   deriving (Functor, Applicative, Monad)
>
> instance Inject Terminal f => MonadTerm (LogTerm f) where
>   getLineM       = LogTerm (Free  $ inject (GetLine pure))
>   printLineM str = LogTerm (liftF $ inject (PrintLine str ()))
>
> instance Inject Log f => MonadLog (LogTerm f) where
>   logM str = LogTerm (liftF $ inject (Log str ()))
>
> --liberate :: (Inject Terminal f, Inject Log f)
> --  => (forall m. (MonadTerm m, MonadLog m) => m a)
> --  -> Free f a
> --liberate = _
>
> inspectComposed :: TerminalM a -> (Int, Int, [String])
> inspectComposed t = let (_, g, p, s) = execState (a t) (['a' ..],0,0,[]) in (g,p,s)
>  where
>   a = foldFree $ \case
>     GetLine       next ->
>       get >>= \case
>         (i:is,g,p,ss) -> do
>            put (is,g+1,p  ,    ss)
>            pure (next [i])
>         _             -> error "analyzeGP GetLine"
>     PrintLine str next -> do
>         (  is,g,p,ss) <- get
>         put    (is,g  ,p+1,str:ss)
>         pure     next


------------------------------------------------------------------------------

> test :: IO ()
> test  = do
>   runMyProgramMIO
>   print (analyzeGP myFreeProgram)
>   runMyProgramMCIO

F3_TypeClass.test
