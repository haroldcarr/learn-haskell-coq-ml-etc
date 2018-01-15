> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TemplateHaskell   #-}
>
> module E2 where
>
> import qualified E1_1
> import qualified E1_2
> import qualified E1_3
> ------------------------------------------------------------------------------
> import qualified Control.Exception.Safe     as S
> import qualified Control.Monad.Except       as E
> import           Control.Monad.Free         as F
> import           Control.Monad.Free.Church  as C
> import           Control.Monad.Free.TH      as FTH
> import qualified Control.Monad.State.Strict as ST
> import qualified Control.Monad.Trans.Except as EX (throwE)
> import qualified Data.Text                  as T
> import qualified Data.Text.IO               as T
> import qualified Prelude
> import           Protolude

> {-# ANN module ("HLint: Reduce duplication" :: Prelude.String) #-}


http://www.andrevdm.com/posts/2018-01-08-refactor-free.html

Refactoring exception handling using a free monad
January 8, 2018

free monad to achieve same goal as E1_*

A quick overview of free monads

free monad : get monad from any functor

free monad gives function that builds free monad structure and one or more functions that interpret/run that structure (i.e., AST)
- record based approach : vary implementation by choosing which record of functions to pass in
- free: structure passed to different interpreters

don’t need free monads to implement : could create AST using sum types
- free advantage : use 'do' notation

> data OpsF m next
>   = OpRead                             (Text -> next)
>   | OpWrite Text                                next
>   | OpLog   Text                                next
>   | OpRun   Text (Text -> m Text) Text (Text -> next)
>   deriving (Functor)
>
> makeFree ''OpsF
> type Ops m = Free (OpsF m)

template haskell/DeriveFunctor : creates types that lift operations into Free monad

last type in data constructor is "return type"
next enables chaining

TODO : no OpRun in this example

> ex :: (Monad m) => T.Text -> (Ops m) T.Text
> ex x = do
>   opLog $ "starting: " <> x
>   r <- opRead
>   opWrite $ r <> x
>   opRead

------------------------------------------------------------------------------
Interpreting

ex gives an AST.
opRead, etc., do nothing on their own.
magic : write normal, pure, code : end up with an AST

ensure only catching asynchronous exceptions

> doFile :: (Ops IO) T.Text -> E.ExceptT E1_3.OpsError IO T.Text
> doFile o =
>   case o of
>     Pure a -> pure a         -- no next action
>     (Free (OpRead n)) -> do
>       r <- liftIO $ T.readFile "/tmp/data.txt"
>       doFile $ n r  -- run next
>      `S.catch`
>       handler E1_3.ErrRead
>     (Free (OpWrite t n)) -> do
>       liftIO $ T.writeFile "/tmp/data.txt" t
>       doFile n
>      `S.catch`
>       handler E1_3.ErrWrite
>     (Free (OpRun _name fn t n)) -> do
>       r <- lift $ fn t
>       doFile $ n r
>      `S.catch`
>       handler E1_3.ErrRunning
>     (Free (OpLog t n)) -> do
>       putStrLn $ "log: " <> t
>       doFile n
>  where
>   handler :: (Monad m) => (T.Text -> E1_3.OpsError) -> SomeException -> E.ExceptT E1_3.OpsError m T.Text
>   handler ope e = EX.throwE . ope $ show e  -- catch exception and use ExceptT's throwE

> eFile :: IO (Either E1_3.OpsError Text)
> eFile = runExceptT $ doFile (ex "test run")

operations  run
synchronous exceptions caught and handled in the ExceptT
similar but simpler to record based approach

------------------------------------------------------------------------------
Testing

> data TestState = TestState
>  { tstValue :: T.Text
>  , tstLog   :: [T.Text]
>  } deriving (Show, Eq)
>
> interpreterState :: (Ops (ST.State TestState)) T.Text -> (ST.State TestState) T.Text
> interpreterState o =
>   case o of
>     Pure a -> do
>       modify (\s -> s { tstValue = a })
>       tstValue <$> get
>     (Free (OpRead n)) -> do
>       st <- ST.get
>       interpreterState $ n (tstValue st)
>     (Free (OpWrite t n)) -> do
>       ST.modify (\s -> s { tstValue = t } )
>       interpreterState n
>     (Free (OpRun _ fn t n)) -> do
>       r <- fn t
>       interpreterState $ n r
>     (Free (OpLog t n)) -> do
>       ST.modify (\(TestState s ls) -> TestState s $ ls <> [t])
>       interpreterState n

> eTest :: (T.Text, TestState)
> eTest = (ST.runState $ interpreterState (ex "test run"))
>         (TestState "foo" [])


Compare to previous approach
- testPipeline :: [I2.Job (S.State Text)] -> Text -> S.State Text (Either I3.E1_3.OpsError Text)
- advantage : tests not forced to use ExceptT. Each interpreter can usee whatever stack is appropriate

Problems

Free monad considered harmful : https://markkarpov.com/post/free-monad-considered-harmful.html
- worth considering alternatives

Even if the Haskell runtime optimizes some of Free overhead through
laziness and generational garbage collection, the asymptotic runtime
is still quadratic.

------------------------------------------------------------------------------
Church encoding

Control.Monad.Free.Church
- church encoding of a free monad
- then only need to constructed tree once (instead of quadratic)

Free monad and church encoding example : https://github.com/queertypes/free-tutorial

use MonadFree constraint (rather than more specific data type) for function that generates DSL

Previous

ex :: (Monad m) => T.Text -> (Ops m) T.Text

Update:

TODO : NO OpRun in this example

> exC  :: (MonadFree (OpsF m1) m2) => T.Text -> m2 T.Text
> exC x = do
>   opLog $ "starting: " <> x
>   r <- opRead
>   opWrite $ r <> x
>   opRead

run withOUT Church encoding

> withoutChurch :: IO ()
> withoutChurch = do
>   let _ioJobs = [ E1_2.Job "j1" E1_1.job1, E1_2.Job "j2" E1_1.job2, E1_2.Job "j3" E1_1.job3 ]
>   a <- runExceptT $ doFile (exC "test")
>   print a

> withChurch :: IO ()
> withChurch = do
>   let _ioJobs = [ E1_2.Job "j1" E1_1.job1, E1_2.Job "j2" E1_1.job2, E1_2.Job "j3" E1_1.job3 ]
>   -- Note that exC must be run inline here to avoid an error about the monad constraints
>   a <- runExceptT $ doFile (C.improve $ exC "test1")
>   print a

use free without O(n^2) concerns

Conclusion

Free : separate pure and impure code AND handling exceptions
