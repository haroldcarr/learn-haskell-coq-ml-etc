> {-# LANGUAGE ScopedTypeVariables #-}
>
> module ExceptionSafetyTutorial where

> import qualified Control.Exception.Safe  as S
> import qualified System.IO              as SIO

https://haskell-lang.org/tutorial/exception-safety

Exception Safety

Explain
- different types of exceptions
- how exceptions generated
- how interruptible actions work
  - trade off between correctness and deadlock prevention
- exception safety in
  - Normal code
  - Resource acquisition code
  - Resource cleanup code
- guidelines on exception safe code
- Point out existing violations of these rules in Haskell ecosystem

Types of exceptions

READ : SafeExceptions.lhs first

Summary:

difference in type hierarchy for a/sync (via SomeAsyncException type)
- distinction not enforced by  Control.Exception
- despite this, assume can distinguish a/sync (enforced by Control.Exception.Safe)

impure exceptions present in pure code
- evaluating a pure value results in exception
- treated as sync

Defining exception safety

Following David Abrahams's exception safety guarantees
- https://en.wikipedia.org/wiki/Exception_safety
- looking for level 3 "basic exception safety"
  - "no-leak guarantee"

    Partial execution of failed operations can cause side effects, but
    all invariants are preserved and there are no resource leaks
    (including memory leaks). Any stored data will contain valid
    values, even if they differ from what they were before the
    exception.

examples of functions that require exception safety
- withFile : ensures file is  closed, regardless of exceptions thrown
- withMVar/modifyMVar : MVar is filled when function exits

Motivating case: withFile

1st impl of withFile

> withFile1 :: FilePath -> (SIO.Handle -> IO b) -> IO b
> withFile1 fp inner = do
>   h <- SIO.openFile fp SIO.ReadMode
>   result <- inner h  -- if inner throw, hClose never called
>   SIO.hClose h
>   return result

> withFile2 :: FilePath -> (SIO.Handle -> IO b) -> IO b
> withFile2 fp inner = do
>   h <- SIO.openFile fp SIO.ReadMode
>   inner h `S.finally` SIO.hClose h

Better, but
- asynchronous exceptions
- openFile throws an exception
- closeFile throws an exception
- which IO actions can throw an exception

Asynchronous exceptions : can occur anywhere: IO, pure code, during monadic bind, etc

in withFile, async exception can be thrown
- before call to openFile
- during call to openFile
- between openFile and S.finally
- after S.finally before exiting withFile
- inside inner

MASKING : region of code where asyn guaranteed not to be thrown

> withFile3 :: FilePath -> (SIO.Handle -> IO b) -> IO b
> withFile3 fp inner = S.mask $ \restore -> do
>   h <- SIO.openFile fp SIO.ReadMode
>   restore (inner h) `S.finally` SIO.hClose h

restore : restore original masking state : usually means "turn async exceptions back on."

restore async exceptions for inner action
- to avoid creating an impossible-to-kill thread
- but make sure allocation and cleanup in a masked state
  - to avoid getting exceptions at unexpected places
- now know that only IO actions have the potential to throw an exception
  and ensure that code does not leak in the case of any of those actions leaking exceptions

with2Files
 - open and guarantee close of two files

> with2Files :: FilePath -> FilePath -> (SIO.Handle -> SIO.Handle -> IO b) -> IO b
> with2Files fp1 fp2 inner = S.mask $ \restore -> do
>   h1 <- SIO.openFile fp1 SIO.ReadMode
>   h2 <- SIO.openFile fp2 SIO.ReadMode `S.onException` SIO.hClose h1
>   restore (inner h1 h2) `S.finally` SIO.hClose h2 `S.finally` SIO.hClose h1

MAIN POINT:
- masking prevents asynchronous exceptions from running
- does NOT prevent synchronous exceptions : must deal with those explicitly

Exceptions in an allocation function

openFile may throw exception (e.g., permission denied)
- so, must not leak any resources : do cleanup

> open2Files :: FilePath -> FilePath -> IO (SIO.Handle, SIO.Handle)
> open2Files fp1 fp2 = do
>   h1 <- SIO.openFile fp1 SIO.ReadMode
>   h2 <- SIO.openFile fp2 SIO.ReadMode `S.onException` SIO.hClose h1
>   return (h1, h2)

- well written allocation function
- h1 will be closed if fp2 throws exception
- only on success : both handles remain open
- do not mask exceptions : rely on caller to mask
  - no way for allocation fun to mask exceptions outside of itself
  - doing so necessary for proper exception safety
  - since allocator has no power, demands masking as precondition to being called

Interruptible actions
- described in Control.Exception docs
- actions which, even when async masked, can throw async
- exceptions thrown from IO
  - exceptions generated as kill signal from outside of our current subroutine
  - therefore treated as async/unrecoverable
- these are allowed to avoid deadlocks
- but more difficult to write exception safe code
- one approach : assume all IO actions may throw an exception
- can be masked using 'uninterruptibleMask'
  - inside uninterrupibleMask: still possible for sync to be generated (via throwIO, ...)
    but no async

Cleanup functions

need guarantees that async will not be thrown before it starts executing
- therefore necessary for functions like bracket or withFile to mask async
- like allocation functions, do NOT need to mask inside cleanup functions
  - assume to already be in place

less clear that uninterruptible masking is the right thing
- better to use uninterruptible masking
  - at risk of introducing delays or deadlocks

> close2Files :: (SIO.Handle, SIO.Handle) -> IO ()
> close2Files (h1, h2) = SIO.hClose h1 `S.finally` SIO.hClose h2

- if closeFile h1 throws, closeFile h2 still called
- relies on closeFile providing appropriate cleanup guarantees
  - when called in a masked state, guarantees will not return until resources freed
- since a cleanup handler is run in such a deadlock-friendly mode (uninterruptible masking)
  should avoid performing long-running actions whenever possible

Difference between allocation and cleanup

In safe-exceptions
- allocations run in mask
  - if allocation interrupted, no damage is done (if allocation atomic)
  - allocations can perform long-running, blocking actions
- cleanups    run in uninterruptibleMask
  - an interruptible cleanup is a possible source of leaks since acquired resources not released
  - cleanups should be quick
  - if cleanup deadlocks (i.e., waits forever) the thread executing it will be unkillable

interrupting threads
- is normal and expected
- does not leads to program termination
- e.g., async package 'race' : spawn two threads, terminate one that takes longer
  - if use race used with computations using interruptible cleanups
    no guarantees the cleanups will be run, thus can leak

Summary of guarantees

allocation
- assume called in masked (interruptible) state
- must either successfully allocate a set of resources (file descriptor, mutex, etc)
  or throw an exception and allocate no resources at all

cleanup
- assume called in masked (uninterruptible) state
- for conformance with Control.Exception, should assume called in interruptible state
- Must release all resources allocated by allocation function
- avoid long-running or blocking actions

when guarantees met, can use allocation/cleanup function combination in a bracket call
and know that, when bracket exits, no resources will be leaked

hClose and flushing

more complicated corner case

actual withFile function from System.IO is defined as:

> withFileIO :: FilePath -> SIO.IOMode -> (SIO.Handle -> IO c) -> IO c
> withFileIO name mode = S.bracket (SIO.openFile name mode) SIO.hClose

hClose requirements:
- flush remaining data in buffer to file descriptor
  - flushing when thread dying from async exception
    - can cause delay in exit (violating "avoid long-running or blocking actions")
- close file descriptor
  - absolutely required
  - there is no way to define bracket and hClose to make withFile behave exactly the way described here.
  - need to separate out the required and blocking actions from each other

assuming hCloseWithoutFlush

> hCloseWithoutFlush :: SIO.Handle -> IO ()
> hCloseWithoutFlush _ = undefined
>
> withFileX :: FilePath -> SIO.IOMode -> (SIO.Handle -> IO c) -> IO c
> withFileX name mode inner = S.mask $ \restore -> do
>   h <- SIO.openFile name mode
>   (eres :: Either S.SomeException c) <- S.try $ restore $ do
>     eres <- S.try (inner h)
>     case eres of
>       Left (e :: S.SomeException)
>         -- when an async exception was thrown,
>         -- don't block in order to flush
>         | S.isAsyncException e -> return ()
>       _ -> SIO.hFlush h
>     either S.throwIO return eres
>   _ :: Either S.SomeException () <- S.try $ S.uninterruptibleMask_ $ hCloseWithoutFlush h
>   either S.throwIO return eres

inherent complexity to give exception guarantees while avoiding blocking calls

recommendation
- to choose between deadlock or resource leak, choose deadlocking
- deadlocking will be a more explicit bug
- typically triggered by another mistake in code
- if uncertain between mask or uninterruptibleMask, use uninterruptibleMask

