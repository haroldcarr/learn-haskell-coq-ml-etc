> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE LambdaCase                 #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE RankNTypes                 #-}
> {-# LANGUAGE UndecidableInstances       #-}
>
> module F2_Free where
>
> import           Control.Monad.Free    (Free (Free, Pure), foldFree, MonadFree, liftF)
> import           Control.Monad.State
> import           Data.List (isPrefixOf)
> import           Prelude   as P
> import qualified System.IO as SIO

Same as F.lhs except uses Control.Monad.Free (instead of hand-rolling).

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
> getLineF :: TerminalM String
> getLineF  = Free (GetLine return)
>
> printLineF :: MonadFree Terminal m => String -> m ()
> printLineF str = liftF (PrintLine str ())
>
> myProgramF :: TerminalM ()
> myProgramF  = do
>   a <- getLineF
>   b <- getLineF
>   printLineF (a ++ b)
>
> interpretFIO :: TerminalM a -> IO a
> interpretFIO  = foldFree $ \case
>   GetLine       next -> next <$> SIO.getLine
>   PrintLine str next -> next <$  putStrLn str
>
> runTrace :: TerminalM () -> ([String],[String]) -> ([String],[String])
> runTrace  (Free (GetLine   f    )) (i:is, os) = runTrace (f    i) (is, os++["GetLine/"++i])
> runTrace  (Free (PrintLine str t))   (is, os) = runTrace t        (is, os++["PurStrLn/"++str])
> runTrace  (Free (GetLine   f    ))   ([], os) = runTrace (f "[]") ([], os++["GetLine/[]"])
> runTrace  (Pure               _r )   (is, os) =                   (is, os++["Pure"])

------------------------------------------------------------------------------
Composability

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

> instance Inject Terminal Log where
>   inject  (     GetLine       x) = Log "GetLine" (x "GetLine")
>   inject  (     PrintLine str x) = Log ("PrintLine" ++ str) x
>   project (Log "GetLine"      x) = Just (GetLine  (const x))
>   project (Log msg            x) =
>     if "PrintLine" `isPrefixOf` msg
>     then Just (PrintLine (drop (length "PrintLine") msg) x)
>     else Nothing

> instance Inject Terminal Maybe where
>   inject  (     GetLine       x) = Just (x "GetLine")
>   inject  (     PrintLine _str x) = Just x
>   project = undefined

> instance Inject Log Maybe where
>   inject  (     Log _str     x) = Just x
>   project = undefined

> getLineC      :: Inject Terminal f =>           Free f String
> getLineC       = Free (inject $ GetLine pure)
>
> printLineC    :: Inject Terminal f => String -> Free f ()
> printLineC str = liftF (inject $ PrintLine str ())
>
> logC          :: Inject Log      f => String -> Free f ()
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

> test :: IO ()
> test  = do
>   putStrLn "interpretFIO myProgramF"
>   interpretFIO myProgramF
>   putStrLn "runTrace myProgramF"
>   let trace = runTrace myProgramF (["1","2","3","4"],[])
>   print trace
