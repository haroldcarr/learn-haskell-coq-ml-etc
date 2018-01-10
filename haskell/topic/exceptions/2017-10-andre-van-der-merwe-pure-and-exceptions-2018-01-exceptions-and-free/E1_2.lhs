> {-# LANGUAGE NoImplicitPrelude   #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module E1_2 where
>
> import           E1_1         (job1, job2, job3)
> ------------------------------------------------------------------------------
> import qualified Data.Text    as T
> import qualified Data.Text.IO as T
> import           Protolude

Step 2 - (Monad m)

remove some IO constraints

> data Operations m = Operations
>   { opRead  :: m T.Text
>   , opWrite :: T.Text -> m ()
>   , opLog   :: T.Text -> m ()
>   }
>
> data Job m = Job
>   { jobName :: T.Text
>   , jobFn   :: T.Text -> m T.Text
>   }
>
> -- monad constraint so monadic type class functions can be used (i.e., pure, >>= etc)
> runPipeline :: (Monad m) => Operations m -> T.Text -> [Job m] -> m T.Text
> runPipeline ops init jobs = do
>   opWrite ops init
>   id <- foldlM runJob 0 jobs
>
>   opLog ops $ "\nfinal job id = " <> show id
>   opRead ops
>  where
>   runJob (id :: Int) (Job name fn) = do
>     opLog ops $ "running job: " <> name
>
>     prev <- opRead ops
>     r <- fn prev
>     opWrite ops r
>
>     opLog ops $ "  = " <> r
>     opLog ops "  ----"
>
>     pure $ id + 1

Storage

pipeline no longer requires IO

storage implementations need IO : so specialize m to IO (otherwise no changes)

> readFileOp :: FilePath -> IO T.Text
> readFileOp = T.readFile
>
> writeFileOp :: FilePath -> T.Text -> IO ()
> writeFileOp = T.writeFile
>
> mkFileOps :: FilePath -> Operations IO
> mkFileOps p = Operations
>   { opRead  = readFileOp p
>   , opWrite = writeFileOp p
>   , opLog   = putStrLn -- for logging rather than calling putStrLn : which can not be done since there is no IO or MonadIO constraint
>   }

Running

> rune12 :: IO ()
> rune12 = do
>   let jobs = [ Job "j1" job1, Job "j2" job2, Job "j3" job3 ]
>   let ops = mkFileOps "/tmp/after"
>   r <- runPipeline ops "0" jobs
>   putStrLn r

improvements
- pipeline is pure
  - means can be tested as pure code
  - can be specialized to IO and run IO jobs
- types compatible with monad transformers since concrete monad type not specified
- jobs can be used as is.

