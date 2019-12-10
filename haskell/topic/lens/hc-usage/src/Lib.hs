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

hasFoo :: (Monad m, HasFoo x) => RWST () [Text] x m Text
hasFoo  = do
  fx   <- use fooX
  fooX .= "fx/hasFoo"
  fy   <- use fooY
  fooY .= "fy/hasFoo"
  fzbx <- use (fooZ.barX)
  pure (fx <> fy <> fzbx)

hasFooHasBar :: (Monad m, HasFoo x, HasBar x) => RWST () [Text] x m Text
hasFooHasBar  = do
  fx   <- use fooX
  fooX .= "fx/hasFooHasBar"
  fy   <- use fooY
  fooY .= "fy/hasFooHasBar"
  hb   <- hasBar
  pure (fx <> fy <> hb)

hasBar :: (Monad m, HasBar x) => RWST () [Text] x m Text
hasBar  = do
  fx   <- use barX
  barX .= "bx/hasBar"
  fy   <- use barY
  barY .= "by/hasBar"
  pure (fx <> fy)

hasFooHasBarHasX :: (Monad m, HasFoo x, HasBar x, HasX x Text) => RWST () [Text] x m Text
hasFooHasBarHasX  = do
  fx   <- use fooX
  fooX .= "fx/hasFooHasBarHasX"
  fy   <- use fooY
  fooY .= "fy/hasFooHasBarHasX"
  hb   <- hasBar
  hx   <- hasX
  pure (fx <> fy <> hb <> hx)

hasXHasY :: (Monad m, HasX x Text, HasY x Text) => RWST () [Text] x m Text
hasXHasY  = do
  hx   <- hasX
  hy   <- hasY
  pure (hx <> hy)

hasX :: (Monad m, HasX t Text) => RWST () [Text] t m Text
hasX  = do
  x' <- use x
  x  .= "x/hasX"
  pure x'

hasY :: (Monad m, HasY t Text) => RWST () [Text] t m Text
hasY  = do
  y' <- use y
  y  .= "y/hasY"
  pure y'

fooey :: Foo
fooey  = Foo "fx" "fy" (Bar "bx" "by")

rHasFoo :: Monad m => m (Text, Foo, [Text])
rHasFoo  = runRWST hasFoo () fooey

rHasFooHasBar :: Monad m => m (Text, Foo, [Text])
rHasFooHasBar  = runRWST hasFooHasBar () fooey

rHasFooHasBarHasX :: Monad m => m (Text, Foo, [Text])
rHasFooHasBarHasX  = runRWST hasFooHasBarHasX () fooey

rHasY :: Monad m => m (Text, Foo, [Text])
rHasY  = runRWST hasY () fooey

rHasY' :: Monad m => m (Text, Bar, [Text])
rHasY'  = runRWST hasY () (Bar "bx" "by")

rHasY'' :: Monad m => m (Text, Bar, [Text])
rHasY''  = runRWST hasY () (Bar "bx" "by")
