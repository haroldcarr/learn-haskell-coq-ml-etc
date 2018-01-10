> {-# LANGUAGE NoImplicitPrelude   #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module E1_3 where
>
> import qualified E1_1                       (DemoException(..), job1, job2, job3)
> import qualified E1_2                       (Operations, Job(Job), opRead, opWrite, opLog, mkFileOps)
> ------------------------------------------------------------------------------
> import qualified Control.Exception.Safe     as S
> import qualified Control.Monad.Except       as E
> import qualified Control.Monad.State.Strict as ST
> import qualified Data.Text                  as T
> import           Protolude
> import           Test.Hspec

Step 3 - Exceptions

see : Exceptions best practices : https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
- mixing ExceptT and exceptions mean need to deal with multiple failure modes
- IO means anything can fail

Accept jobs that can fail with exceptions : treat this as normal : pipeline should handle them

Do not want pure code to have to deal with exceptions.
Rather use Either
need something to bridge worlds

> data OpsError
>   = ErrRead    T.Text
>   | ErrWrite   T.Text
>   | ErrLogging T.Text
>   | ErrRunning T.Text
>   deriving (Show, Eq)
>
> -- wrapper function for each function in Operations record
> -- catch synchronous exceptions and covert to ExceptT transformer
> data OperationsWrapper m = OperationsWrapper
>   { opRead  :: ExceptT OpsError m T.Text
>   , opWrite :: T.Text -> ExceptT OpsError m ()
>   , opLog   :: T.Text -> ExceptT OpsError m ()
>   , opRun   :: (T.Text -> m T.Text) -> T.Text -> ExceptT OpsError m T.Text
>   }

> mkOpsWrapper :: (S.MonadCatch m) => E1_2.Operations m -> OperationsWrapper m
> mkOpsWrapper o = OperationsWrapper
>   { opRead  =          E.ExceptT ((Right <$> E1_2.opRead  o)   `S.catch` readError)
>   , opWrite = \t    -> E.ExceptT ((Right <$> E1_2.opWrite o t) `S.catch` writeError)
>   , opLog   = \t    -> E.ExceptT ((Right <$> E1_2.opLog   o t) `S.catch` logError)
>   , opRun   = \fn t -> E.ExceptT ((Right <$> fn t)             `S.catch` logError)
>   }
>  where
>   readError,writeError,logError  :: (Monad m) => SomeException -> m (Either OpsError b)
>   readError  e = pure . Left . ErrRead    $ "Error reading: " <> show e
>   writeError e = pure . Left . ErrWrite   $ "Error writing: " <> show e
>   logError   e = pure . Left . ErrLogging $ "Error logging: " <> show e

Pipeline using the wrapper

> runPipeline :: (Monad m) => OperationsWrapper m -> T.Text -> [E1_2.Job m] -> m (Either OpsError T.Text)
> runPipeline ops init jobs = runExceptT $ do
>   opWrite ops init
>   id <- foldlM runJob 0 jobs
>
>   opLog ops $ "\nfinal job id = " <> show id
>   opRead ops
>  where
>   runJob (id :: Int) (E1_2.Job name fn) = do
>     opLog ops $ "running job: " <> name
>
>     prev <- opRead ops
>     r <- opRun ops fn prev -- don't just lift, use opRun
>     opWrite ops r
>
>     opLog ops $ "  = " <> r
>     opLog ops "  ----"
>
>     pure $ id + 1

changes from Step2

- result is Either (failure now explicit)
- using OperationsWrapper (not Operations)
- using ExceptT , so runExceptT needed
- Each wrapper function abort the monad if it returns a Left
- added an opRun fun to wrap running of the jo
  - If just lift the job’s run function, then exception would not be handled. So need wrapper.

Running

> rune13 :: IO ()
> rune13 = do
>   let jobs = [ E1_2.Job "j1" E1_1.job1, E1_2.Job "j2" E1_1.job2, E1_2.Job "j3" E1_1.job3 ]
>   let ops = E1_2.mkFileOps "/tmp/after"
>
>   r <- runPipeline (mkOpsWrapper ops) "0" jobs
>
>   case r of
>     Right x -> putStrLn $ "Success: "   <> x
>     Left e  -> putStrLn   ("Exception: " <> show e :: T.Text)

will catch job 2 exception, correctly report the error, i.e. no runtime failure

Testing

test pipeline using only pure code

:set -XOverloadedStrings
import qualified Control.Monad.State.Strict as ST
import qualified E1_2
let jobs = [ E1_2.Job "j1" E1_3.job1, E1_2.Job "j2" E1_3.job2 ]
ST.runState (testPipeline jobs "0") ""

> spec :: Spec
> spec =
>   describe "simple pipeline" $
>     it "should run in correct order" $ do
>       let jobs = [ E1_2.Job "j1" E1_3.job1, E1_2.Job "j2" E1_3.job2 ]
>       let (r, _) = ST.runState (testPipeline jobs "0") ""
>       r `shouldBe` Right "2:1:0"
>
> testPipeline :: [E1_2.Job (ST.State T.Text)] -> T.Text -> ST.State T.Text (Either OpsError T.Text)
> testPipeline jobs initial = do
>   let ops = OperationsWrapper
>               { opRead = E.ExceptT $ do
>                   r <- get
>                   pure . Right $ r
>
>               , opWrite = \t -> E.ExceptT $ do
>                   put t
>                   pure . Right $ ()
>
>               , opRun = \fn t -> E.ExceptT $ do
>                   r <- fn t
>                   pure . Right $ r
>
>               , opLog = \_ -> E.ExceptT . pure . Right $ ()
>               }
>   runPipeline ops initial jobs
>
>
> job1 :: T.Text -> (ST.State T.Text) T.Text
> job1 v = pure $ "1:" <> v
>
> job2 :: T.Text -> (ST.State T.Text) T.Text
> job2 v = pure $ "2:" <> v

Notes on exceptions

Catching all exceptions generally considered to be a bad idea.
- See docs for Control.Exception : https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Exception.html#g:4
- See Catching all exceptions : https://www.schoolofhaskell.com/user/snoyberg/general-haskell/exceptions/catching-all-exceptions

Safe-exceptions

above uses safe-exceptions package (e.g., catch) so only synchronous exceptions are caught.

Using async

Another approach : use async library : gives me timeout and cancellation control.

> demoAsyncCatch :: IO ()
> demoAsyncCatch = do
>   r <- async jobBad >>= waitCatch
>   case r of
>     Right _ -> putStrLn ("demo async - Right"::T.Text)
>     Left  e -> putStrLn $ "demo async - Left: " <> T.pack (show e)
>  where
>   jobBad = do
>     putStrLn ("in jobBad"::T.Text)
>     void . throwIO $ E1_1.DemoException "oops"

Conclusion

wrapper record
- separates the pure and effectful worlds
- converts synchronous IO exceptions into Eithers.
- passing record of functions : alternative to using a typeclass

see: ReaderT design pattern : https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
