{-# LANGUAGE LambdaCase #-}

module Ch01 where

import Prelude hiding (curry, uncurry)

------------------------------------------------------------------------------
{-
p 19
(a^b)^c = a^(b*c)
-}

uncurry :: (b -> c -> a) -> (b, c) -> a
uncurry f (b, c) = f  b  c

curry   :: ((b, c) -> a) -> b -> c -> a
curry   f  b  c  = f (b, c)

uc  :: ((b, c) -> a) -> (b, c) -> a
uc   = uncurry . curry
uc' :: ((b, c) -> a) -> (b, c) -> a
uc'  = id

cu  :: (b -> c -> a) -> b -> c -> a
cu   = curry . uncurry
cu' :: (b -> c -> a) -> b -> c -> a
cu'  = id

------------------------------------------------------------------------------
{-
p 19
a^b * a^c = a^(b+c)
-}

ee  :: (b -> a, c -> a) -> Either b c -> a
ee (fba, fca) = \case
  Left  b -> fba b
  Right c -> fca c

ee' :: (Either b c -> a) -> (b -> a, c -> a)
ee'  f = (f . Left, f . Right)

xx  :: (Either b c -> a) -> Either b c -> a
xx   = ee . ee'

xx' :: (b -> a, c -> a) -> (b -> a, c -> a)
xx'  = ee' . ee

------------------------------------------------------------------------------
{-
p 19
(a * b)^c = a^c * b^c
-}

ll :: (c -> (a, b)) -> (c -> a, c -> b)
ll  f = (fst . f, snd . f)

rr :: (c -> a, c -> b) -> c -> (a, b)
rr (fca, fcb) = \c -> (fca c, fcb c)

llrr :: (c -> a, c -> b) -> (c -> a, c -> b)
llrr = ll . rr

rrll :: (c -> (a, b)) -> c -> (a, b)
rrll = rr . ll
{-# ANN rr ("HLint: ignore Redundant lambda") #-}
