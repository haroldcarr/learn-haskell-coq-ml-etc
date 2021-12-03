{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module P075_exercises_length_reverse_map where

import           Data.Maybe
import qualified Data.List          as L
import           Data.Proxy
import           Data.Type.Equality
import qualified Data.Vector        as V
import qualified Data.Vector.Sized  as VS
import           GHC.TypeLits

len0
  :: forall n a
   . KnownNat n
  => VS.Vector n a
  -> Integer
len0 _ = natVal (Proxy @n)

len1
  :: forall n a
   . VS.Vector n a
  -> Proxy n
len1 _ = Proxy @n

exx :: VS.Vector 8 Int
exx = case VS.fromList [1,3,9,7,6,4,5,8] :: Maybe (VS.Vector 8 Int) of
        Just vs -> vs
        Nothing -> error "NO"
l0 :: Integer
l0  = len0 exx
l1 :: (Proxy (8 :: Nat))
l1  = len1 exx

rev0
  :: forall n a
   . VS.Vector n a
  -> VS.Vector n a
rev0 = VS.reverse

rev1
  :: forall n a
   . KnownNat n
  => VS.Vector n a
  -> VS.Vector n a
rev1 x =
  let v0 = V.reverse (VS.fromSized x)
  in fromMaybe (error "NO") (VS.toSized v0)

rev2
  :: forall n a
   . KnownNat n
  => VS.Vector n a
  -> VS.Vector n a
rev2 x =
  let v0 = V.reverse (VS.fromSized x)
  in VS.withSized v0 $ \(v :: VS.Vector n' elem) ->
    case sameNat (Proxy @n) (Proxy @n') of
      Nothing   -> error "NO"
      Just Refl -> v :: VS.Vector n elem

map0
  :: forall n a b
   . KnownNat n
  => (a -> b)
  -> VS.Vector n a
  -> VS.Vector n b
map0 f x =
  let v0 = V.fromList (L.map f (VS.toList x))
  in fromMaybe (error "NO") (VS.toSized v0)




