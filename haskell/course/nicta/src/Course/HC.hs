
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.HC  where

import           Course.Applicative
import           Course.Apply
import           Course.Bind
import           Course.Core
import           Course.Functor
import           Course.Monad
import           Course.Optional    (Optional (Full, Empty))
import           Course.State
import qualified Prelude            as P

data HC a = HC a deriving (Eq, Show)

-- |
-- >>> HC $ (+1) (runHC (HC 3))
-- HC 4
--
-- >>> applyFHC (+1) (HC 3)
-- HC 4
--
runHC :: HC a -> a
runHC (HC a) = a

applyFHC :: (a -> b) -> HC a -> HC b
applyFHC f (HC a) = HC (f a)

instance Functor HC where
    f <$> (HC x) = HC (f x)

-- |
-- >>> (+1) <$> (HC 3)
-- HC 4

-- |
-- >>> runHC (HC (+1)) <$> (HC 3)
-- HC 4

instance Apply HC where
    HC f <*> HC x = HC (f x)

-- |
-- >>> HC (+1) <*> (HC 3)
-- HC 4

-- |
-- >>> exampleHCSpecific (HC 'a') 3
-- (HC 'a',HC 3)
--
-- exampleHCSpecific (Full 'a') 3 -- gets type error
--
exampleHCSpecific :: HC t -> a -> (HC t, HC a)
exampleHCSpecific x y = (x, HC y)

instance Applicative HC where
    pure = HC

-- |
-- >>> examplePure (HC 'a') 3
-- (HC 'a',HC 3)
--
-- >>> examplePure (Full 'a') 3
-- (Full 'a',Full 3)
--
examplePure :: Applicative f => (f a) -> b -> (f a, f b)
examplePure x y = (x, pure y)

data HCM a = HCN | HCJ a deriving (Eq, Show)

-- |
-- >>> exampleContextUsage HCN      (\a -> HCJ a)  (\b -> b)
-- HCN
--
-- >>> exampleContextUsage (HCJ 1)  (\_ -> HCN  )  (\b -> b)
-- HCN
--
-- >>> exampleContextUsage (HCJ 1)  (\a -> HCJ a)  (\b -> b)
-- HCJ 1
--
exampleContextUsage :: HCM a -> (a -> HCM b) -> (b -> c) -> HCM c
exampleContextUsage f1 f2 f3 =
    case f1 of
        HCN    -> HCN
        HCJ a1 -> case f2 a1 of
                     HCN    -> HCN
                     HCJ a2 -> HCJ (f3 a2)

instance Functor HCM where
    (<$>) _  HCN    = HCN
    (<$>) f (HCJ x) = HCJ (f x)

instance Apply HCM where
    (<*>)  HCN    _       = HCN
    (<*>)  _        HCN   = HCN
    (<*>) (HCJ f) (HCJ x) = HCJ (f x)

instance Applicative HCM where
    pure = HCJ

instance Bind HCM where
    (=<<) _  HCN    = HCN
    (=<<) f (HCJ x) = (f x)

instance Monad HCM where

-- |
-- >>> exampleContextUsageMonadic HCN       (\a -> HCJ a)   (\b -> b)
-- HCN
--
-- >>> exampleContextUsageMonadic Empty     (\a -> pure a)  (\b -> b)
-- Empty
--
--
-- >>> exampleContextUsageMonadic (HCJ 1)   (\_ -> HCN)     (\b -> b)
-- HCN
--
-- >>> exampleContextUsageMonadic (Full 1)  (\_ -> Empty)   (\b -> b)
-- Empty
--
--
-- >>> exampleContextUsageMonadic (HCJ 1)   (\a -> HCJ a)   (\b -> b)
-- HCJ 1
--
-- >>> exampleContextUsageMonadic (Full 1)  (\a -> pure a)  (\b -> b)
-- Full 1
--
exampleContextUsageMonadic :: Monad m => m a -> (a -> m b) -> (b -> c) -> m c
exampleContextUsageMonadic f1 f2 f3 =
    pure . f3 =<< f2 =<< f1

-- |
-- >>> stateExampleMonadic "hello world" shiftM upperM
-- ("!E","llo world")
--
stateExampleMonadic   :: [Char] -> ([Char] -> State [Char] Char) -> ([Char] -> State [Char] Char) -> ([Char], [Char])
stateExampleMonadic i f1 f2 =
  runState
   (get          >>=
    \s0 -> f1 s0 >>=
    \r1 -> get   >>=
    \s1 -> f2 s1 >>=
    \r2 -> pure (r1:r2:[]) ) i

shiftM :: [Char] -> State [Char] Char
shiftM (s:ss) = put ss >>= \_ -> pure $ chr (ord s - 71)
shiftM [] = error "SHOULD NOT HAPPEN IN THIS EXAMPLE"

upperM :: [Char] -> State [Char] Char
upperM (s:ss) = put ss >>= \_ -> pure $ toUpper s
upperM [] = error "SHOULD NOT HAPPEN IN THIS EXAMPLE"

-- End of file.
