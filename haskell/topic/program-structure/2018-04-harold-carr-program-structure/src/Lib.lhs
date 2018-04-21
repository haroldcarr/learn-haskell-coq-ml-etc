> {-# LANGUAGE ConstraintKinds            #-}
> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE NoImplicitPrelude          #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE TypeSynonymInstances       #-}
>
> module Lib where
>
> import qualified Data.Map.Strict as M
> import           Universum

------------------------------------------------------------------------------

> newtype Config = Config { filenames :: [Text] }
>
> newtype AppT m a = AppT { unAppT :: ReaderT Config m a }
>   deriving (Functor, Applicative, Monad)

------------------------------------------------------------------------------

> class MonadFileReader m where
>   readFile :: FilePath -> m Text
>
> class MonadFileWriter m where
>   writeFile :: FilePath -> Text -> m ()
>
> type MonadFiles m = (MonadFileReader m, MonadFileWriter m)
>
> type TestReaderWriter = State (Map FilePath Text)
>
> instance MonadFileReader TestReaderWriter where
>   readFile fileName = do
>     files <- get
>     let contents = fromMaybe "" (M.lookup fileName files)
>     return contents
>
> instance MonadFileWriter TestReaderWriter where
>   writeFile fileName contents = modify (M.insert fileName contents)

------------------------------------------------------------------------------

> wordOccurences :: [[Text]] -> Map Text Int
> wordOccurences = foldl' go M.empty
>  where
>   go = foldl' (flip (M.alter go'))
>    where
>     go' Nothing  = Just 1
>     go' (Just i) = Just (i + 1)

------------------------------------------------------------------------------

> c :: Config
> c = Config ["F1","F2"]

> main = runReaderT top c

> -- top :: ReaderT Config TestReaderWriter (Map Text Int)
> -- top :: (MonadFiles m, MonadIO m) => ReaderT Config m [Text] -- (Map Text Int)
> top :: ReaderT Config IO [Text] -- (Map Text Int)
> top = do
>   fns <- asks filenames
>   -- getWords fns
>   -- return M.empty
>   return fns
>
> getWords :: MonadFileReader m => fns -> m [[Text]]
> getWords _ = return [["X"]]
