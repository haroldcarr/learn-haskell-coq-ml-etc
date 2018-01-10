> {-# LANGUAGE DeriveFunctor     #-}
> {-# LANGUAGE FlexibleContexts  #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TemplateHaskell   #-}
>
> module E2 where
>
> import qualified E1_3
> ------------------------------------------------------------------------------
> import qualified Control.Exception.Safe     as S
> import qualified Control.Monad.Except       as E
> import           Control.Monad.Free         as F
> import           Control.Monad.Free.TH      as FTH
> import qualified Control.Monad.State.Strict as ST
> import qualified Control.Monad.Trans.Except as EX (throwE)
> import qualified Data.Text                  as T
> import qualified Data.Text.IO               as T
> import           Protolude

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
If the last type is a function returning next, means can bind the value

-- data OpsF m next = OpWrite Text next
--                  | ...
do
  opWrite "param1"
  opWrite "param2"

opWrite is function created by template Haskell that constructs a OpWrite.
opWrite takes a single param, the Text from “OpWrite Text next”

Since there is a next you can have multiple statements in the do block

-- data OpsF m next = OpRead (Text -> next)
--                  | ...
do
  r <- opRead

opRead is function created by template Haskell that constructs a OpRead.
opRead takes no parameters

can bind to Text result the (Text -> next) from “OpRead (Text -> next)”

> createAst :: (Monad m) => T.Text -> (Ops m) T.Text
> createAst x = do
>   opLog $ "starting: " <> x
>   r <- opRead
>   opWrite $ r <> x
>   opRead

Interpreting

createAst gives an AST.
opRead etc do nothing on their own.
magic : write normal, pure, code : end up with an AST

ensure only catching asynchronous exceptions

> interpreterFile :: (Ops IO) T.Text -> E.ExceptT E1_3.OpsError IO T.Text
> interpreterFile o =
>   case o of
>     Pure a -> pure a   -- no next action
>     (Free (OpRead n)) -> do
>       r <- liftIO $ T.readFile "data.txt"
>       interpreterFile $ n r  -- run next
>      `S.catch`
>       handler E1_3.ErrRead
>     (Free (OpWrite t n)) -> do
>       liftIO $ T.writeFile "data.txt" t
>       interpreterFile n  -- run next
>      `S.catch`
>       handler E1_3.ErrWrite
>     (Free (OpRun _name fn t n)) -> do
>       r <- lift $ fn t
>       interpreterFile $ n r  -- run next
>      `S.catch`
>       handler E1_3.ErrRunning
>     (Free (OpLog t n)) -> do
>       putText $ "log: " <> t
>       interpreterFile n  -- run next
>  where
>   handler :: (Monad m) => (T.Text -> E1_3.OpsError) -> SomeException -> E.ExceptT E1_3.OpsError m T.Text
>   handler ope e = EX.throwE . ope $ show e  -- catch exception and use ExceptT's throwE

operations  run
synchronous exceptions caught and handled in the ExceptT
similar but simpler to record based approach

Testing

> data TestState = TestState
>   { tstValue :: T.Text
>   , tstLog   :: [T.Text]
>   } deriving (Show, Eq)
>
> interpreterState :: (Ops (ST.State TestState)) T.Text -> (ST.State TestState) T.Text
interpreterState o =
  case o of
    Pure a -> do
      modify (\s -> s { tstValue = a })
      tstValue <$> get

    (Free (OpRead n)) -> do
      st <- S.get 
      interpreterState $ n (tstValue st)

    (Free (OpWrite t n)) -> do
      S.modify (\s -> s { tstValue = t } )
      interpreterState n
      
    (Free (OpRun _ fn t n)) -> do
      r <- fn t
      interpreterState $ n r
      
    (Free (OpLog t n)) -> do
      S.modify (\(TestState s ls) -> TestState s $ ls <> [t])
      interpreterState n
Compare that to the previous approach’s tests

testPipeline :: [I2.Job (S.State Text)] -> Text -> S.State Text (Either I3.E1_3.OpsError Text)
testPipeline jobs initial = do
  let ops = I3.OperationsWrapper { I3.opRead = E.ExceptT $ do
                                     r <- get
                                     pure . Right $ r
The big advantage here is that the tests are no longer forced to use ExceptT. Each interpreter, for testing or otherwise, can use whatever stack is appropriate

Problems
As always there are trade offs, see the Free monad considered harmful article for example. While some of these issues can be address (e.g. see church encoding below) it is worth considering alternatives.

Personally, so far, I’ve found free to be a great fit for what I need (e.g. selecting implementation not based on type), but its definitely worth deciding on a case by case basis

Church encoding
The Control.Monad.Free.Church package handles church encoding of a free monad. This can be important to do because, as it says in Control.Monad.Free.Church:

Even if the Haskell runtime optimizes some of the overhead through laziness and generational garbage collection, the asymptotic runtime is still quadratic. On the other hand, if the Church encoding is used, the tree only needs to be constructed once.

Given how easy this package makes church encoding, and how bad O(n^2) performance can be, it is almost always a good idea to do the encoding.

(I originally found getting the types correct for Church encoding a bit tricky. This Free monad and church encoding example helped clear up a lot of the confusion for me. Be sure to look at it as well if my explanation below does not help you).

To get Church encoding, the only requirement is that you use a MonadFree constraint rather than your more specific data type for the function that generates the DSL.

In the example above createAst looked like this.

createAst :: (Monad m) => Text -> (Ops m) Text
createAst x = do
The problem is that I’ve used the “Ops m” type, rather than MonadFree.

Here is what it should look like

createAst :: (Monad m, MonadFree (OpsF m) a) => Text -> [Job m] -> a Text
createAst x = do
The important parts being

createAst :: (Monad m, MonadFree (OpsF m) a) => ............... -> a Text
createAst x = do
Change from Ops to OpsF
Add “MonadFree (...) a”
This is how it would be run without Church encoding

  --------------------------------------------------
  -- Example in IO with exception
  --------------------------------------------------
  let ioJobs = [ Job "j1" ioJob1
               , Job "j2" ioJob2
               , Job "j3" ioJob3
               ]
  
  a <- runExceptT $ interpreterFile $ createAst "test1" ioJobs
  print a
And this is how its run with Church encoding using improve from Control.Monad.Free.Church

  --------------------------------------------------
  -- Example in IO with exception
  --------------------------------------------------
  let ioJobs = [ Job "j1" ioJob1
               , Job "j2" ioJob2
               , Job "j3" ioJob3
               ]
  
        -- Note that createAst must be run inline here to avoid an error about the monad constraints
  ai <- runExceptT $ interpreterFile (C.improve $ createAst "test1" ioJobs)
  print ai
That is all it takes, we can now use free without O(n^2) concerns

Conclusion
Free monads give us a nice way to separate pure and impure code while also handling exceptions. Overall I think this approach is more flexible and easier to read that the record of functions approach.

Links
Code on github (gist)

Free monad tutorials
Purify code using free monads
Why free monads matter
What does Free buy us?
Control.Monad.Free
Control.Monad.Free.Church
Free monad considered harmful
Free monad and church encoding example
Exceptions best practices in Haskell

Port of example code to use operational monad by @brandonhamilton

Site proudly generated by Hakyll




