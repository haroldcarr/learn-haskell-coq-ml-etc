> {-# OPTIONS_GHC -Wno-warnings-deprecations #-}
> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE NoImplicitPrelude          #-}
>
> module Lib where
>
> import Data.Conduit
> import Data.Thyme
> import Universum

Three Layer Haskell Cake
22 Mar 2018
http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

------------------------------------------------------------------------------
Layer 1: ReaderT Design Pattern : orchestration : imperative programming

> data Config = Config
>
> newtype AppT m a = AppT { unAppT :: ReaderT Config m a }
>     deriving (Functor, Applicative, Monad)

backbone of app
- Config has info, state, explicit effect, ...
- IOHK calls it "capability"
  https://github.com/input-output-hk/cardano-sl/blob/develop/docs/monads.md
- sucks to test
  - put business logic up into next two layers
- this layer should be small

Shift
- factor out inputs
- represent output of effects as data returned from pure functions

------------------------------------------------------------------------------
Layer 2: specify effects : object oriented programming

bridge between 1 and 2

mock external services/dependencies
- via mtl

> class MonadTime m where
>   getCurrentTime :: m UTCTime

"purify" an action that uses IO only
makes unit testing easier

> data Lock = Lock
> data KKey = KKey
> data UserQuery = UserQuery
> data User = User
> data UserId = UserId
> data Dog = Dog

> -- | given UTCTime, returns it
> instance MonadTime ((->) UTCTime) where
>  getCurrentTime = id

> class Monad m => MonadLock m where
>   acquireLock :: NominalDiffTime -> KKey -> m (Maybe Lock)
>   renewLock   :: NominalDiffTime -> Lock -> m (Maybe Lock)
>   releaseLock :: Lock                    -> m ()

can mock this with IORef (Map ByteString ByteString)

DSL for working with data : represents some SQL queries/data

> class (Monad m) => AcquireUser m where
>   getUserBy      :: UserQuery -> m [User]
>   getUser        :: UserId    -> m (Maybe User)
>   getUserWithDig :: UserId    -> m (Maybe (User, Dog))
>
> class AcquireUser m => UpdateUser m where
>   deleteUser     :: UserId    -> m ()
>   insertUser     :: User      -> m ()

use this to mock database
to swap out production impls

This level higher than Level 1/App
- specifies effects used
- but lower than business logic
- heavy-weight: mtl classes need newtypes and instance boilerplate
- this layer should thing

------------------------------------------------------------------------------
Layer 3: pure/business logic : functional programming

acquire effectful data beforehand
effectful post-processing done afterwards

for streaming, implement "pure"

> -- | expresses no depedency on where data comes from, nor how output is handled
> pureConduit :: Monad m => Conduit i m o
> pureConduit = undefined

if computation needs an effect, encode effect as a datatype, candidates
- free monads
- non-recursive "command" sum type to serve as interface between pure and effectful functions
  - list commands

before monads, think about
- monoidal constructing/reading data
- Free Applicative
- limited recursive sum type
- GADT





