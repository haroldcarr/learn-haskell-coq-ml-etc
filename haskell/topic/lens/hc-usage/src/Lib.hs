{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Lib where

import           Control.Lens
import           Control.Monad.Trans.RWS.Strict
import qualified Prelude
import           Protolude

{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}

data Bar = Bar
  { _barX :: Int
  , _barY :: Int
  } deriving (Eq, Show)
makeClassy ''Bar
-- makeFields ''Bar

data Foo = Foo
  { _fooX :: Int
  , _fooY :: Int
  , _fooZ :: Bar
  } deriving (Eq, Show)
makeClassy ''Foo
-- makeFields ''Foo

instance HasBar Foo where
  bar = lens _fooZ (\foo' bar' -> foo' { _fooZ = bar' })

xxx :: (Monad m, HasFoo x) => RWST () [Text] x m Int
xxx  = do
  x'   <- use fooX
  fooX .= (-1)
  y'   <- use fooY
  fooY .= (-2)
  z'   <- use (fooZ.barX)
  pure (x' + y' + z')

xxx' :: (Monad m, HasFoo x, HasBar x) => RWST () [Text] x m Int
xxx'  = do
  x'   <- use fooX
  fooX .= (-1)
  y'   <- use fooY
  fooY .= (-2)
  z'   <- yyy
  pure (x' + y' + z')

yyy :: (Monad m, HasBar x) => RWST () [Text] x m Int
yyy  = do
  x'   <- use barX
  barX .= (-4)
  y'   <- use barY
  barY .= (-5)
  pure (x' + y')

r :: Monad m => m (Int, Foo, [Text])
r = runRWST xxx () (Foo 1 2 (Bar 3 4))

-- r' :: (MonadState x m, HasFoo x, HasBar Foo) => m (Int, Foo, [Text])
r' :: (Monad m, HasFoo Foo, HasBar Foo) => m (Int, Foo, [Text])
r' = runRWST xxx' () (Foo 1 2 (Bar 3 4))

{-
:set -XFlexibleContexts
-}
