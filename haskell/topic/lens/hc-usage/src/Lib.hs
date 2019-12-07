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
import           Protolude hiding (get, gets)

{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}
{-# ANN module ("HLint: ignore Redundant return" :: Prelude.String) #-}

data Bar = Bar
  { _barX :: Text
  , _barY :: Text
  } deriving (Eq, Show)
makeClassy ''Bar
makeFields ''Bar

data Foo = Foo
  { _fooX :: Text
  , _fooY :: Text
  , _fooZ :: Bar
  } deriving (Eq, Show)
makeClassy ''Foo
makeFields ''Foo

instance HasBar Foo where
  bar = lens _fooZ (\foo' bar' -> foo' { _fooZ = bar' })

xxx :: (Monad m, HasFoo x) => RWST () [Text] x m Text
xxx  = do
  x'   <- use fooX
  fooX .= "fx/xxx"
  y'   <- use fooY
  fooY .= "fy/xxx"
  z'   <- use (fooZ.barX)
  pure (x' <> y' <> z')

xxx' :: (Monad m, HasFoo x, HasBar x) => RWST () [Text] x m Text
xxx'  = do
  x'   <- use fooX
  fooX .= "fx/xxx'"
  y'   <- use fooY
  fooY .= "fy/xxx'"
  yyy' <- yyy
  pure (x' <> y' <> yyy')

yyy :: (Monad m, HasBar x) => RWST () [Text] x m Text
yyy  = do
  x'   <- use barX
  barX .= "bx/yyy"
  y'   <- use barY
  barY .= "by/yyy"
  pure (x' <> y')

xxx'' :: (Monad m, HasFoo x, HasBar x, HasX x Text) => RWST () [Text] x m Text
xxx''  = do
  x'   <- use fooX
  fooX .= "fx/xxx''"
  y'   <- use fooY
  fooY .= "fy/xxx''"
  yyy' <- yyy
  zzz' <- zzz
  pure (x' <> y' <> yyy' <> zzz')

zzz :: (Monad m, HasX t Text) => RWST () [Text] t m Text
zzz  = do
  x' <- use x
  x .= "x/zzz"
  pure x'

fooey :: Foo
fooey  = Foo "fx" "fy" (Bar "bx" "by")

r :: Monad m => m (Text, Foo, [Text])
r = runRWST xxx () fooey

r' :: (Monad m, HasFoo Foo, HasBar Foo) => m (Text, Foo, [Text])
r' = runRWST xxx' () fooey

r'' :: (Monad m, HasFoo Foo, HasBar Foo) => m (Text, Foo, [Text])
r'' = runRWST xxx'' () fooey
