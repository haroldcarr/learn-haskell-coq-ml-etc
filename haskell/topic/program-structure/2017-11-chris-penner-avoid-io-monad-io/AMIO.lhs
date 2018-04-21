> {-# LANGUAGE ConstraintKinds      #-}
> {-# LANGUAGE FlexibleContexts     #-}
> {-# LANGUAGE FlexibleInstances    #-}
> {-# LANGUAGE TypeSynonymInstances #-}
>
> module AMIO where
>
> import Control.Monad.Reader
> import Control.Monad.State
> import Data.Map as M
> import Data.Maybe (fromMaybe)
>
> {-# ANN module "HLint: ignore Reduce duplication" #-}

MonadIO Considered Harmful
Chris Penner
Sep 11, 2017
http://chrispenner.ca/posts/monadio-considered-harmful

IO and MonadIO are too general.

Alternative : Free or Freeer monads

Alternative discussed here:
- splitting up IO into granular monad type classes

mtl popularized the idea of type classes e.g. MonadReader, MonadState, MonadIO, ...

abstracts away concrete monad used to enable portability and reusability

example using both concrete monad types and abstract monad typeclasses:

> concreteUserIncrement :: StateT Int IO Int
> concreteUserIncrement = do
>   incAmount <- liftIO readLn
>   modify (+ incAmount)
>   return incAmount
>
> classUserIncrement :: (MonadState Int m, MonadIO m) => m Int
> classUserIncrement = do
>   incAmount <- liftIO readLn
>   modify (+ incAmount)
>   return incAmount

They do the same thing.  Recommend class-based approach.
- Enables reusing the function in other monad stacks.
- Extensible, e.g.,:

need access to options, then add:

    ReaderT Options (StateT Int IO)

If using concrete StateT, need to rewrite all signatures.
No need in class-based signature.

- class-based sigs specifiy what effects a function plans to use --- e.g.,:

> data Options
>
> concreteReset :: ReaderT Options (StateT Int IO) ()
> concreteReset = put 0
>
> classbasedReset :: (MonadState Int m) => m ()
> classbasedReset = put 0

Both impls are the same.  What do types say?

class-based can only interact with Int stored in StateT.

concrete case gives no hints
- impl could do IO, logging, ...

------------------------------------------------------------------------------
Breaking up MonadIO

sig with 'MonadState Int m' limited scope of actions.

What does 'MonadIO m' class constraint say?

Says need access to IO.  But what?  Reading/writing/launching?  TOO GENERAL

Compare to 'MonadRead' 'ask' or 'MonadState' 'modify'
- clear scope

> class MonadFiles m where
>   readAFile  :: FilePath -> m String
>   writeAFile :: FilePath -> String -> m ()
>
> instance MonadFiles IO where
>   readAFile  = readFile
>   writeAFile = writeFile

Use above to clarly specify only file system read/write

> getDiary :: MonadFiles m => m String
> getDiary = readAFile "my-diary.txt"

Can NOT launch nukes.

Also : different instance for testing:

> instance MonadFiles (State (M.Map String String)) where
>   readAFile fileName = do
>     files <- get
>     let contents = fromMaybe "" (M.lookup fileName files)
>     return contents
>   writeAFile fileName contents =
>     modify (M.insert fileName contents)

More granularity.

> class MonadFileReader m where
>   readAFileX :: FilePath -> m String -- X to avoid dup in this file
>
> class MonadFileWriter m where
>   writeAFileX :: FilePath -> String -> m ()

Can combine via ConstraintKinds
  https://kseo.github.io/posts/2017-01-13-constraint-kinds.html

> type MonadFilesX m = (MonadFileReader m, MonadFileWriter m)
