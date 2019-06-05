{-# LANGUAGE GADTs      #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Phantom_GADT where

------------------------------------------------------------------------------
-- existential types

data G1 where
  G1X :: forall a. a -> G1 -- a is existential (only appear to left of 'where')
  G1Y :: Char        -> G1
  G1Z :: Int         -> G1

g1 :: G1 -> (Maybe Char, Maybe Int)
g1  = \case
  G1X _ -> (Nothing, Nothing) -- no way to work with or RETURN the value in G1X
  G1Y c -> (Just c, Nothing)
  G1Z i -> (Nothing, Just i)

data G2 a where
  G2X :: forall b. b -> G2 b
  G2Y :: Char        -> G2 Char
  G2Z :: Int         -> G2 Int

g2 :: G2 a -> Maybe a
g2  = \case
  G2X a -> Just a  -- no way to work with it, but it can be returned
  G2Y c -> Just (if c == 'c' then 'd' else c)
  G2Z i -> Just (i + 1)

------------------------------------------------------------------------------
-- phantom types

data P1 a = P1

p1Int :: P1 Int
p1Int  = P1

p1G1  :: P1 G1
p1G1   = P1

p1i   :: P1 Int -> String
p1i _  = "P1 Int"
p1i'  :: String
p1i'   = p1i p1Int

p1g1  :: P1 G1 -> String
p1g1 _ = "P1 G1"
p1g1' :: String
p1g1'  = p1g1 p1G1
