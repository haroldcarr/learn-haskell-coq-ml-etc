{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

------------------------------------------------------------------------------
import           Control.Monad.Trans.RWS.Strict as RWST
import           Protolude
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- library

ok   :: Monad m => r -> m (Either l r)
ok    = pure . Right

bail :: Monad m => l -> m (Either l r)
bail  = pure . Left

ensure :: Monad m => Bool -> l -> r -> m (Either l r)
ensure b l r = if b then ok r else bail l

(.?.) :: Monad m
      =>        m (Either l r1)
      -> (r1 -> m (Either l r2))
      ->        m (Either l r2)
(.?.) er1 f = er1 >>= \case
  Right r -> f r
  Left  l -> pure (Left l)

(.^.) :: Monad m
      => m (Either l r)
      -> (l -> l)
      -> m (Either l r)
(.^.) er1 fl = er1 >>= \case
  Left e -> pure (Left (fl e))
  r      -> pure r

------------------------------------------------------------------------------
-- application

type    HashValue       = ByteString
newtype ExecutedBlock a = ExecutedBlock a deriving (Eq, Show)

data ErrLog a
  = ErrBlockNotFound           ![Text] !HashValue
  | ErrExistingBlock           ![Text] !(ExecutedBlock a) !HashValue !(ExecutedBlock a)
  | ErrL                       ![Text]
  deriving (Eq, Show)

withErrCtx :: [Text] -> ErrLog a -> ErrLog a
withErrCtx ctx = \case
  ErrBlockNotFound       txt hv         -> ErrBlockNotFound       (ctx <> txt) hv
  ErrExistingBlock       txt eb1 hv eb2 -> ErrExistingBlock       (ctx <> txt) eb1 hv eb2
  ErrL                   txt            -> ErrL                   (ctx <> txt)

data Output a
  = Foo a
  | Bar
  deriving (Eq, Show)

type LBFT m e s a r = RWST e [Output a] s m r

runLBFT :: RWS e w s a -> e -> s -> (a, s, w)
runLBFT  = runRWS

foo :: Int -> (Either (ErrLog a) (), (), [Output a])
foo i = runLBFT (fooM i) () ()

fooM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) ())
fooM i =
  barM i .^. withErrCtx ["fooM"] .?.
  ok

barM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) ())
barM i =
  bazM i .?.
  ensure (i==i) (ErrL ["barM"]) .?.
  ok

bazM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) ())
bazM i =
  ok () .?.
  ensure (i==1) (ErrL ["bazM"]) .?.
  const (bail (ErrBlockNotFound ["bazM"] "bad"))

quxM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) Int)
quxM i =
  ensure (i==i) (ErrL ["quxM1"]) () .?.
  ensure (i==i) (ErrL ["quxM2"])    .?.
  ensure (i==i) (ErrL ["quxM3"])    .?.
  const (ok 3)

x :: (Either (ErrLog a) (), (), [Output a])
x = runLBFT (xxxM 1) () ()

xxxM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) ())
xxxM i =
  ensure (i==i) (ErrL ["xxxM1"]) () .?.
  ensure (i==i) (ErrL ["xxxM2"])    .?.
  ensure (i==i) (ErrL ["xxxM3"])

y :: (Either (ErrLog a) (), (), [Output a])
y = runLBFT (yyyM 1) () ()

yyyM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) ())
yyyM i =
  xxxM i .?.
  const (bail (ErrBlockNotFound ["yyyM"] "bad"))

z :: Int -> (Either (ErrLog a) (), (), [Output a])
z i = runLBFT (zzzM i) () ()

zzzM :: Monad m => Int -> LBFT m e s a (Either (ErrLog a) ())
zzzM i = do
  void (xxxM i)
  ensure (i==1) (ErrL ["zzzM1"]) () .?.
   const (yyyM i) .?.
   ensure (i==i) (ErrL ["zzzM2"]) .?.
   const (bail (ErrBlockNotFound ["zzzM3"] "bad"))
