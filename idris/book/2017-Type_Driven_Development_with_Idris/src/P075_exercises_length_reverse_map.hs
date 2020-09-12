{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module P075_exercises_length_reverse_map where

import           Data.Maybe
import qualified Data.List          as L
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector        as V0
import           Data.Vector.Sized  as V
import           GHC.TypeLits

len0
  :: forall n a.
     KnownNat n
  => Vector n a
  -> Integer
len0 x = natVal (Proxy @n)

len1
  :: forall n a.
     KnownNat n
  => Vector n a
  -> Proxy n
len1 x = Proxy @n

(Just x) = V.fromList [1,3,9,7,6,4,5,8] :: Maybe (Vector 8 Int)
l0 = len0 x
l1 = len1 x

rev0
  :: forall n a.
     KnownNat n
  => Vector n a
  -> Vector n a
rev0 = V.reverse

rev1
  :: forall n a.
     KnownNat n
  => Vector n a
  -> Vector n a
rev1 x =
  let v0 = V0.reverse (V.fromSized x)
  in fromMaybe (error "NO") (toSized v0)

rev2
  :: forall n a.
     KnownNat n
  => Vector n a
  -> Vector n a
rev2 x =
  let v0 = V0.reverse (V.fromSized x)
  in withSized v0 $ \(v :: Vector n' elem) ->
    case sameNat (Proxy @n) (Proxy @n') of
      Nothing   -> error "NO"
      Just Refl -> v :: Vector n elem

map0
  :: forall n a b.
     KnownNat n
  => (a -> b)
  -> Vector n a
  -> Vector n b
map0 f x =
  let v0 = V0.fromList (L.map f (V.toList x))
  in fromMaybe (error "NO") (toSized v0)




