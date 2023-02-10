{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module V1_Mtl where

-- http://reasonablypolymorphic.com/dont-eff-it-up/

import           Control.Monad.Except
import           Control.Monad.Identity
--import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.RWS
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.IORef

------------------------------------------------------------------------------
-- BAD : IO exposed, test/real interspersed, no compiler guarantee of mocked IO

data Mode
  = ForReal
  | Test (IORef Int)

getCurrentBalanceMTL :: (MonadIO m{-, MonadLogger m-}) => m Int
getCurrentBalanceMTL = return 10

putCurrentBalanceMTL :: (MonadIO m{-, MonadLogger m-}) => Int -> m ()
putCurrentBalanceMTL i = liftIO $ print i

withdraw :: (MonadIO m, MonadLogger m)
         => Mode
         -> Int
         -> m (Maybe Int)
withdraw mode desired = do
  amount <- case mode of
              ForReal    -> getCurrentBalanceMTL
              Test ioref -> liftIO $ readIORef ioref
  if amount < desired then do
    $(logWarn) "not enough funds"
    return Nothing
  else do
    let putAction = case mode of
                      ForReal    -> putCurrentBalanceMTL
                      Test ioref -> liftIO . writeIORef ioref
    putAction $ amount - desired
    return $ Just amount

------------------------------------------------------------------------------

-- to abstract over IO, add new constraint
class Monad m => MonadBank m where
  getCurrentBalance :: m Int
  putCurrentBalance :: Int -> m ()

-- | app/test code can swap out different monads
withdrawMTL2 :: (MonadBank m, MonadLogger m)
             => Int
             -> m (Maybe Int)
withdrawMTL2 desired = do
  amount <- getCurrentBalance
  if amount < desired then do
    $(logWarn) "not enough funds"
    return Nothing
  else do
    putCurrentBalance $ amount - desired
    return $ Just amount

-- comes with a heavy costcomes with a cost
-- need a carrier that works with MTL
newtype IOBankT m a = IOBankT { runIOBankT :: IdentityT m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError e
           , MonadIO
           , MonadRWS r w s
           , MonadReader r
           , MonadState s
           , MonadTrans
           , MonadWriter w
           )

-- which implements our monad...

instance MonadIO m => MonadBank (IOBankT m) where
  getCurrentBalance = getCurrentBalanceMTL
  putCurrentBalance = putCurrentBalanceMTL

-- then LOTs of boilerplate

instance MonadBank m => MonadBank (ReaderT r m) where
  getCurrentBalance = lift getCurrentBalance
  putCurrentBalance = lift . putCurrentBalance

instance (MonadBank m, Monoid w) => MonadBank (WriterT w m) where
  getCurrentBalance = lift getCurrentBalance
  putCurrentBalance = lift . putCurrentBalance

instance MonadBank m => MonadBank (StateT s m) where
  getCurrentBalance = lift getCurrentBalance
  putCurrentBalance = lift . putCurrentBalance
