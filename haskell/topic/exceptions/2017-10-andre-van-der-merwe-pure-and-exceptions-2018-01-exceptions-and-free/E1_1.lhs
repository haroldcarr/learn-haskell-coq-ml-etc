> {-# LANGUAGE NoImplicitPrelude   #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module E1_1 where
>
> import qualified Data.Text    as T
> import qualified Data.Text.IO as T
> import qualified Prelude
> import           Protolude

http://www.andrevdm.com/posts/2017-10-31-refactor-away-io.html

Refactoring to pure code and dealing with exceptions.
October 31, 2017
Overview

use record of functions & record of wrapper functions that catch all synchronous exceptions and convert to ExceptT for the pure code

application
- pipeline of jobs/actions
- where each job in pipeline is considered user code (i.e., plugin)
- job : can run any IO action : no constraints : full IO access
- pipeline : runs impure jobs, but should be pure itself
  - Must be able to handle job failure (exceptions)
- supports different storage mechanisms (e.g. disk, cloud)
- jobs and pipeline should be testable

Step 1 - Just use IO and refactor later

> data Job = Job
>   { jobName :: T.Text
>   , jobFn   :: T.Text -> IO T.Text
>   }
>
> -- record of operations (instead of type class) that pipeline uses to persist job results: pluggable storage
> data Operations = Operations
>   { opRead  :: IO T.Text
>   , opWrite :: T.Text -> IO ()
>   }

discussion of records vs typeclass

- ReaderT design pattern : https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
- mtl-style-example : using mtl to unit test effectful code in a pure way : https://github.com/lexi-lambda/mtl-style-example
- Java interfaces map to Haskell records : https://chris-martin.org/2017/interfaces-and-records

> runPipeline :: Operations -> T.Text -> [Job] -> IO T.Text
> runPipeline ops init jobs = do
>   opWrite ops init
>   -- run jobs, pass a unique (for the run) id to each job
>   id <- foldlM runJob 0 jobs
>   putText $ "\nfinal job id = " <> show id
>   opRead ops
>  where
>   runJob (id :: Int) (Job name fn) = do
>     putStrLn $ "running job: " <> name
>
>     prev <- opRead ops -- load last data
>     r <- fn prev       -- do the job
>     opWrite ops r      -- store the result
>
>     putStrLn $ "  = " <> r
>     putStrLn ("  ----"::T.Text)
>
>     pure $ id + 1

Storage

> readFileOp :: FilePath -> IO T.Text
> readFileOp = T.readFile
>
> writeFileOp :: FilePath -> T.Text -> IO ()
> writeFileOp = T.writeFile
>
> mkFileOps :: FilePath -> Operations
> mkFileOps p = Operations
>   { opRead  = readFileOp p
>   , opWrite = writeFileOp p
>   }

Example jobs

> job1 :: T.Text -> IO T.Text
> job1 v = do
>   putText "in job1"
>   pure $ "1:" <> v
>
> job2 :: T.Text -> IO T.Text
> job2 v = do
>   putText "in job2"
>   void . throwIO $ DemoException "oops"
>   pure $ "2:" <> v
>
> job3 :: Text -> IO Text
> job3 v = do
>   putText "in job3"
>   pure $ "3:" <> v
>
> newtype DemoException = DemoException T.Text
>
> instance Prelude.Show DemoException where
>   show (DemoException s) = T.unpack s
>
> instance Exception DemoException

Running

> rune11 :: IO ()
> rune11 = do
>   let jobs = [ Job "j1" job1, Job "j2" job2, Job "j3" job3 ]
>   let ops = mkFileOps "/tmp/after"
>   r <- runPipeline ops "0" jobs
>   putStrLn r

running gets an exception in job 2, result in the application terminating
