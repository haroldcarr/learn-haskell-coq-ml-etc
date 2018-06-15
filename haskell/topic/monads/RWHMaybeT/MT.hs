module MT where

import           Data.Functor.Classes
import           Test.HUnit           (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util      as U (t,tt)

-- http://book.realworldhaskell.org/read/monad-transformers.html

-- modify behaviour of underlying monad 'm a'
--                  wraps UNDERLYING monad's        type parameter with Maybe
--                                         v        v
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


instance (Functor m) => Functor (MaybeT m) where
    -- (a -> b) -> MaybeT m a -> MaybeT m b
  fmap f              = mapMaybeT (fmap (fmap f))
mapMaybeT            :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f           = MaybeT . f . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  mtf <*> mta = MaybeT $ do
    mf <- runMaybeT mtf
    case mf of
      Nothing -> return Nothing
      Just f  -> do
        ma <- runMaybeT mta
        case ma of
          Nothing -> return Nothing
          Just a  -> return (Just (f a))

-- The thing to be made a Monad instance is the partial type 'MaybeT m'.
-- This has a single type parameter 'a' that satisfies Monad typeclass constraint.
instance Monad m => Monad (MaybeT m) where
  -- Monad m => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  --          m (Maybe b) -> MaybeT m b
  --          v
  (>>=) x f = MaybeT $ do -- everything inside do block executes in underlying "unknown" monad m
    -- Maybe a
    --         m (Maybe a)
    --                     MaybeT m a
    -- v       v           v
    unwrapped <- runMaybeT x
    case unwrapped of
      --         m (Maybe b)
      --                Maybe b
      --         v      v
      Nothing -> return Nothing
      --      m (Maybe b)
      --         MaybeT m b -> m (Maybe b)
      -- Maybe a            a -> MaybeT m b
      --   a                  a
      -- v v  v  v          v v
      Just y  -> runMaybeT (f y)
  -- Monad m => a -> MaybeT m a
  return = MaybeT . return . Just
  -- Monad m => t -> MaybeT m a
  fail _ = MaybeT $ return Nothing

bindMT' :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT' x f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

newtype IdentityT f a     = IdentityT { runIdentityT :: f a }
instance (Functor m)     => Functor (IdentityT m) where
    fmap f                = mapIdentityT (fmap f)
mapIdentityT             :: (m a -> n b) -> IdentityT m a -> IdentityT n b
mapIdentityT f            = IdentityT . f . runIdentityT
instance (Applicative m) => Applicative (IdentityT m) where
    pure x                = IdentityT (pure x)
    (<*>)                 = lift2IdentityT (<*>)
lift2IdentityT           :: (m a -> n b -> p c) -> IdentityT m a -> IdentityT n b -> IdentityT p c
lift2IdentityT f a b      = IdentityT (f (runIdentityT a) (runIdentityT b))
instance (Monad m)       => Monad (IdentityT m) where
    m >>= k               = IdentityT $ runIdentityT . k =<< runIdentityT m
    fail msg              = IdentityT $ fail msg

------------------------------------------------------------------------------

-- loops
-- xx  = runIdentityT (runMaybeT (fmap (+1) e1))

te01,te02,te03,te04::[Test]
tfe01,tfe02,tfe03,tfe04::[Test]
tr01,tr02::[Test]

e01  :: MaybeT (IdentityT []) Int
e01  = return 3
e02 :: MaybeT            []  Int
e02  = return 3
e03 :: MaybeT            []  Int
e03  = MaybeT ([       ]::[Maybe Int])
e04 :: MaybeT            []  Int
e04  = MaybeT ([Nothing]::[Maybe Int])
te01  = U.t  "te01"            e01  (MaybeT (IdentityT [Just 3 ]))
te02  = U.t  "te02"            e02  (MaybeT            [Just 3 ])
te03  = U.t  "te03"            e03  (MaybeT            [       ])
te04  = U.t  "te04"            e04  (MaybeT            [Nothing])
tfe01 = U.t "tfe01" (fmap (+1) e01) (MaybeT (IdentityT [Just 4 ]))
tfe02 = U.t "tfe02" (fmap (+1) e02) (MaybeT            [Just 4 ])
tfe03 = U.t "tfe03" (fmap (+1) e03) (MaybeT            [       ])
tfe04 = U.t "tfe04" (fmap (+1) e04) (MaybeT            [Nothing])

r01 :: [Maybe Int]
r01 = runIdentityT (runMaybeT e01)
r02 :: [Maybe Int]
r02 =               runMaybeT e02
tr01 = U.t "tr01" r01 [Just 3]
tr02 = U.t "tr02" r02 [Just 3]

tfm :: [Test]
tfm = U.tt "tfm"
  [                         fmap (+1)                       (MaybeT (IdentityT [Just (3::Int)])) -- MaybeT fmap def: rm outer fmap; add 2 fmap ->
  , mapMaybeT             (fmap (fmap (+1)))                (MaybeT (IdentityT [Just (3::Int)])) -- mapMaybeT def -> wrap result with MaybeT
  , (MaybeT .              fmap (fmap (+1)) . runMaybeT)    (MaybeT (IdentityT [Just (3::Int)])) -- runMaybeT def -> rm MaybeT from data
  , (MaybeT .              fmap (fmap (+1)))                        (IdentityT [Just (3::Int)])  -- replace . with explicit call ->
  ,  MaybeT (              fmap (fmap (+1))                         (IdentityT [Just (3::Int)])) -- IdentityT fmap def: rm outer fmap; add 1 fmap ->
  ,  MaybeT (mapIdentityT (fmap (fmap (+1)))                        (IdentityT [Just (3::Int)])) -- mapIdentityT def -> wrap result with IdentityT
  ,  MaybeT ((IdentityT .  fmap (fmap (+1)) . runIdentityT)         (IdentityT [Just (3::Int)])) -- runIdentityT def --> rm IdentityT from data
  ,  MaybeT ((IdentityT .  fmap (fmap (+1)))                                   [Just (3::Int)])  -- replace . with explicit call ->
  ,  MaybeT ( IdentityT (  fmap (fmap (+1))                                    [Just (3::Int)])) -- [] fmap def ->
  ,  MaybeT ( IdentityT (   map (fmap (+1))                                    [Just (3::Int)])) -- map def : apply fun '(fmap (+1))' to list elements ->
  ,  MaybeT ( IdentityT                                             [fmap (+1) (Just (3::Int))]) -- Maybe fmap def: rm fmap; apply (+1) to arg; wrap in Just ->
  ,  MaybeT ( IdentityT                                                  [Just ((+1) (3::Int))]) -- apply (+1) to value ->
  ,  MaybeT ( IdentityT                                                        [Just (4::Int)])
  ]
  (MaybeT (IdentityT [Just 4]))

test :: IO Counts
test =
  runTestTT $ TestList $
    te01 ++ te02 ++ te03 ++ te04 ++
    tfe01 ++ tfe02 ++ tfe03 ++ tfe04 ++
    tr01 ++ tr02 ++
    tfm

------------------------------------------------------------------------------

instance (Eq1 m) => Eq1 (MaybeT m) where
    liftEq eq (MaybeT x) (MaybeT y) = liftEq (liftEq eq) x y
    {-# INLINE liftEq #-}

instance (Ord1 m) => Ord1 (MaybeT m) where
    liftCompare comp (MaybeT x) (MaybeT y) = liftCompare (liftCompare comp) x y
    {-# INLINE liftCompare #-}

instance (Read1 m) => Read1 (MaybeT m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "MaybeT" MaybeT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 m) => Show1 (MaybeT m) where
    liftShowsPrec sp sl d (MaybeT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "MaybeT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (MaybeT m a) where compare = compare1
instance (Read1 m, Read a) => Read (MaybeT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (MaybeT m a) where showsPrec = showsPrec1

------------------------------------------------------------------------------

instance (Eq1 f) => Eq1 (IdentityT f) where
    liftEq eq (IdentityT x) (IdentityT y) = liftEq eq x y
    {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (IdentityT f) where
    liftCompare comp (IdentityT x) (IdentityT y) = liftCompare comp x y
    {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (IdentityT f) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp rl) "IdentityT" IdentityT

instance (Show1 f) => Show1 (IdentityT f) where
    liftShowsPrec sp sl d (IdentityT m) =
        showsUnaryWith (liftShowsPrec sp sl) "IdentityT" d m


instance (Eq1 f, Eq a) => Eq (IdentityT f a) where (==) = eq1
instance (Ord1 f, Ord a) => Ord (IdentityT f a) where compare = compare1
instance (Read1 f, Read a) => Read (IdentityT f a) where readsPrec = readsPrec1
instance (Show1 f, Show a) => Show (IdentityT f a) where showsPrec = showsPrec1
