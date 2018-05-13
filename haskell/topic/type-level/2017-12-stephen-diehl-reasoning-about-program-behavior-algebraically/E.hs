{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module E where

import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import qualified Data.Text as T
import           GHC.TypeLits
import           Network.Transport hiding (send)
import           Network.Transport.TCP
import qualified Prelude
import           Protolude hiding (Chan, Map, Seq, (:+) )

-- http://dev.stephendiehl.com/types_behavior.pdf

data Chan (n :: Symbol) = forall a . MkChan (Chan a)

data Session where
  (:!)  :: forall a. a ->            Session -> Session -- Send
  (:?)  :: forall a. a ->            Session -> Session -- Receive
  (:*!) :: forall a. a ->            Session -> Session -- Output
  (:+)  ::                Session -> Session -> Session -- Branch
  End   ::                                      Session -- Terminate

type family Seq s t where
  Seq  (a ':!  s)  t =  a         ':!  (Seq s  t)
  Seq  (a ':?  s)  t =  a         ':?  (Seq s  t)
  Seq  (a ':*! s)  t =  a         ':*! (Seq s  t)
  Seq (s1 ':+  s2) t = (Seq s1 t) ':+  (Seq s2 t)
  Seq         'End s =  s

type family Map (f :: a -> b) (as :: [a]) :: [b] where
  Map f      '[]  = '[]
  Map f (x ': xs) = f x ': Map f xs

data Mapping k v = k :-> v

-- send :: Chan c -> t -> Process '[c :-> t :! End] ()
-- send :: Chan c -> t -> Process '[c :-> t :! End]
-- send = undefined
-- recv :: Chan c ->      Process '[c :-> t :? End] t
-- recv :: Chan c ->      Process '[c :-> t :? End]
-- recv = undefined

x :: IO ()
x = do
  et <- createTransport "127.0.0.1" "10501" (const ("127.0.0.1", "10501")) defaultTCPParameters
  case et of
    Right t -> do
      node <- newLocalNode t initRemoteTable
      runProcess node (loop t node)
    Left e -> Prelude.error (show e)
 where
  loop t node = do
    yorn <- liftIO getLine
    case T.toLower yorn of
      "q" -> liftIO (closeTransport t) >> liftIO (closeLocalNode node)
      _   -> doit yorn >> loop t node
  doit s = do
    self <- getSelfPid
    send self s
    hello <- expect :: Process Text
    liftIO $ putStrLn hello

