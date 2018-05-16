{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

-- https://dspace.library.uu.nl/bitstream/handle/1874/355676/sessiontypes-thesis.pdf
-- https://github.com/Ferdinand-vW/sessiontypes
-- https://github.com/Ferdinand-vW/sessiontypes-distributed


module ST where

import           Control.Monad.IO.Class
import qualified Control.Distributed.Process as CH
import           Control.Distributed.Process.Serializable
import           Data.Binary
import           GHC.Generics (Generic)
import           GHC.Types

-- p 11

data ST
  = (:!>) Type ST -- send
  | (:?>) Type ST -- recv
  | Eps           -- end (i.e., pure)
infixr 5 :!>
infixr 5 :?>

data Ping = Ping deriving Generic
data Pong = Pong deriving Generic
instance Binary Ping
instance Binary Pong

-- ppex1,ppex2 :: ST
-- ppex1 = Eps
-- ppex2 = Ping :!> Pong :?> Eps

-- p 12

type family Dual (a :: ST) where
  Dual (a ':!> r) = a ':?> Dual r
  Dual (a ':?> r) = a ':!> Dual r
  Dual 'Eps       = 'Eps

-- p 13

-- shallow DSL : wrapper function around CH that carry session type

--          phantom type : type of session
--              v
newtype Session s a = Session { runSession :: CH.ProcessId -> CH.Process a }

sendS :: Serializable a => a -> Session (a ':!> r) ()
sendS a = Session $ \pid -> CH.send pid a

recvS :: Serializable a => Session (a ':?> r) a
recvS = Session $ const CH.expect

epsS :: a -> Session 'Eps a
epsS a = Session $ \_ -> return a

-- deep DSL : builds AST : decouples evaluation

data STTerm1 :: ST -> Type -> Type where
  -- :!> takes  'a' and 'r'
  -- Send takes 'a' and 'STTerm1' annotated with 'r'
  Send1 :: Serializable a
        => (a :: Type)        -> STTerm1 r b -> STTerm1 (a ':!> r) b
  -- cannot give Recv an 'a' until eval
  -- so give continuation that given an 'a'
  -- produces 'STTerm1' annotated with continuation of receive session type
  Recv1 :: Serializable a
        => (a -> STTerm1 r b)                -> STTerm1 (a ':?> r) b
  --  given an 'a' used as result of protocol
  Pure1 ::  a                                -> STTerm1 'Eps       a

-- p 14

-- Example shows why continuation needed in Recv1.
-- session typed program does not have access to value that will eventually be received.
-- That only given during evaluation.
prog1 :: STTerm1 (Ping ':!> Pong ':?> 'Eps) Pong
prog1 = Send1 Ping $ Recv1 $ \pong -> Pure1 pong

-- effect of deep+eval same as shallow
eval1 :: CH.ProcessId -> STTerm1 s a -> CH.Process a
eval1 pid (Send1 a r) = CH.send pid a >>  eval1 pid   r
eval1 pid (Recv1   r) = CH.expect     >>= eval1 pid . r
eval1 _   (Pure1 a  ) = return a

-- composing session typed programs

-- p 17

-- bind two indexed monads : final state of 1st must equal initial state of 2nd
class IxMonad (m :: p -> p -> Type -> Type) where
  bind :: m s t a -> (a -> m t k b) -> m s k b
  pure ::       a                   -> m i i a

-- p 18

data STTerm2 :: ST -> ST -> Type -> Type where
  Send2 :: (a :: Type) -> STTerm2 r r' b  -> STTerm2 (a ':!> r) r' b
  Recv2 :: (a          -> STTerm2 r r' b) -> STTerm2 (a ':?> r) r' b
  Pure2 :: a                              -> STTerm2 s          s  a

instance IxMonad STTerm2 where
  pure                 = Pure2
  (Send2 a r) `bind` f = Send2 a       (r   `bind` f)
  (Recv2   r) `bind` f = Recv2   (\a -> r a `bind` f)
  (Pure2 a  ) `bind` f = f a

send2 :: a -> STTerm2 (a ':!> r) r ()
send2 a = Send2 a $ Pure2 ()

recv2 :: STTerm2 (a ':?> r) r a
recv2 = Recv2 Pure2

end2 :: a -> STTerm2 'Eps 'Eps a
end2 = Pure2

prog2 :: STTerm2 (Ping ':!> Pong ':?> 'Eps) 'Eps ()
prog2 =
  send2 Ping `bind` \_ ->
  recv2 `bind` \Pong ->
  end2 ()

-- p 19 : indexed monad transformer - so can do other computations too

class IxMonad (t m) => IxMonadT t m where
  lift :: m a -> t m s s a

data STTerm :: (Type -> Type) -> ST -> ST -> Type -> Type where
  Send :: Serializable a
       => (a :: Type) -> STTerm m r r' b           -> STTerm m (a ':!> r) r' b
  Recv :: Serializable a
       => (a          -> STTerm m r r' b)          -> STTerm m (a ':?> r) r' b
  Pure ::  a                                       -> STTerm m s          s  a
  Lift ::            (m (STTerm m s r  a) :: Type) -> STTerm m s          r  a

instance Monad m => IxMonad (STTerm m) where
  pure                = Pure
  (Send a r) `bind` f = Send a       (r   `bind` f)
  (Recv   r) `bind` f = Recv   (\a -> r a `bind` f)
  (Pure a  ) `bind` f = f a
  (Lift m)   `bind` f = Lift $ m >>= \st -> return (st `bind` f)

instance Monad m => IxMonadT STTerm m where
  lift m = Lift $ fmap Pure m

send :: (Monad m, Serializable a) => a -> STTerm m (a ':!> r) r ()
send a = Send a $ Pure ()

recv :: (Monad m, Serializable a) => STTerm m (a ':?> r) r a
recv = Recv Pure

end :: Monad m => a -> STTerm m 'Eps 'Eps a
end = Pure

io :: MonadIO m => IO a -> STTerm m s s a
io = lift . liftIO
progio :: (Monad m, MonadIO m) => STTerm m (String ':?> 'Eps) 'Eps ()
progio =
  recv `bind` \x ->
  io (putStrLn x) `bind` \_ ->
  end ()

eval :: Monad m => CH.ProcessId -> STTerm m s r a -> CH.Process a
eval pid (Send a r) = CH.send pid a >>  eval pid   r
eval pid (Recv   r) = CH.expect     >>= eval pid . r
eval _   (Pure a  ) = return a
eval _pid (Lift _m) = undefined
