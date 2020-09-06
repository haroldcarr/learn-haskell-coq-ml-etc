{-# LANGUAGE Strict        #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}

module Control.Monad.RWSIO.StrictX where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Data.IORef
-- import Debug.Trace
------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
------------------------------------------------------------------------------

type RWSRef r w s = (r, IORef (w, s))

newtype RWSTIO r w s a = RWSTIO { unRWSTIO :: RWSRef r w s -> IO (a, RWSRef r w s) }

instance Functor (RWSTIO r w s) where
  fmap f m = RWSTIO $ \x -> do
    (a, ref) <- unRWSTIO m x
    pure (f a, ref)
  {-# INLINE fmap #-}

instance Monoid w => Applicative (RWSTIO r w s) where
  pure a = RWSTIO $ \x -> return (a, x)
  {-# INLINE pure #-}

  RWSTIO mf <*> RWSTIO mx = RWSTIO $ \x -> do
    (f,   _) <- mf x
    (arg, _) <- mx x
    return (f arg, x)
  {-# INLINE (<*>) #-}

instance Monoid w => Monad (RWSTIO r w s) where
  return = pure
  {-# INLINE return #-}

  m >>= k  = RWSTIO $ \x -> do
    (a, _)   <- unRWSTIO m x
    (b, _)   <- unRWSTIO (k a) x
    return (b, x)
  {-# INLINE (>>=) #-}

------------------------------------------------------------------------------
-- Reader

ask :: RWSTIO r w s r
ask = RWSTIO $ \x@(r,_) -> return (r, x)
{-# INLINE ask #-}

asks :: (r -> a) -> RWSTIO r w s a
asks f = RWSTIO $ \x@(r,_) -> return (f r, x)
{-# INLINE asks #-}

------------------------------------------------------------------------------
-- Writer

tell :: Monoid w => w -> RWSTIO r w s ()
tell w = RWSTIO $ \x@(_r, ref) -> do
  {-
  liftIO (modifyIORef' ref (\(w',s) -> ( trace ("\nwtell " ++ show w' ++ " " ++ show w)
                                         w'<>w
                                       , s)))
  -}
  (w', s)    <- readIORef ref
  writeIORef ref (w' <> w, s)
  return ((), x)
{-# INLINE tell #-}

------------------------------------------------------------------------------
-- State

get :: RWSTIO r w s s
get = RWSTIO $ \x@(_,ref) -> do
  (_w, s)    <- readIORef ref
  return (s, x)
{-# INLINE get #-}

gets :: (s -> a) -> RWSTIO r w s a
gets f = RWSTIO $ \x@(_,ref) -> do
  (_w, s)    <- readIORef ref
  return (f s, x)
{-# INLINE gets #-}

put :: s -> RWSTIO r w s ()
put s = RWSTIO $ \x@(_,ref) -> do
  -- Using modifyIORef' causes a space leak. The function given to modify isn't run until the end.
  -- trace "StateputliftIO" $ liftIO (modifyIORef' ref (\(w,_s) -> (trace "Stateput" w,s)))
  (w, _s)    <- readIORef ref
  writeIORef ref (w, s)
  return ((), x)
{-# INLINE put #-}

modify :: (s -> s) -> RWSTIO r w s ()
modify f = RWSTIO $ \x@(_,ref) -> do
  (w, s)     <- readIORef ref
  writeIORef ref (w, f s)
  return ((), x)
{-# INLINE modify #-}

------------------------------------------------------------------------------

initRWSTIO :: (MonadIO m, Monoid w) => r -> s -> m (RWSRef r w s)
initRWSTIO r s = (r,) <$> liftIO (newIORef (mempty, s))

resetRWSTIO :: (MonadIO m, Monoid w) => RWSRef r w s -> s -> m ()
resetRWSTIO (_, ref) s =
  liftIO (writeIORef ref (mempty, s))

resetRWSTIOWriter :: (MonadIO m, Monoid w) => RWSRef r w s -> m ()
resetRWSTIOWriter (_, ref) =
  liftIO (modifyIORef' ref (\(_, s) -> (mempty, s)))

runRWSTIO0
  :: (MonadIO m, Monoid w)
  => RWSTIO r w s a -> r -> s
  -> m (a, s, w, RWSRef r w s)
runRWSTIO0 act r s = do
  x@(_,ref) <- initRWSTIO r s
  liftIO $ do
    (a, _)    <- unRWSTIO act x
    (w, s')   <- readIORef ref
    pure (a, s', w, x)

-- | Typical usage: 'initRWSTIO' followed by one or more 'runRWSTIO'
runRWSTIO
  :: (MonadIO m, Monoid w)
  => RWSTIO r w s a -> RWSRef r w s
  -> m (a, s, w, RWSRef r w s)
runRWSTIO act x@(_,ref) =
  liftIO $ do
    resetRWSTIOWriter x
    (a, _)    <- unRWSTIO act x
    (w, s')   <- readIORef ref
    pure (a, s', w, x)

runRWSTIO'
  :: (MonadIO m, Monoid w)
  => RWSTIO r w s a -> s -> RWSRef r w s
  -> m (a, s, w, RWSRef r w s)
runRWSTIO' act s x@(_,ref) =
  liftIO $ do
    resetRWSTIO x s
    (a, _)    <- unRWSTIO act x
    (w, s')   <- readIORef ref
    pure (a, s', w, x)

