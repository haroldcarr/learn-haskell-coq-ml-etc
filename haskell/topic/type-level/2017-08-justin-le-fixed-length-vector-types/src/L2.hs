{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE EmptyCase            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module L2 where

------------------------------------------------------------------------------
import qualified Data.Kind              as DK
import qualified Data.Singletons        as S
import qualified Data.Singletons.Decide as SD
import           Data.Singletons.TH     as STH hiding (Disproved, Proved)
import           Prelude                hiding ((++))
------------------------------------------------------------------------------
{-
The Structural Way

TypeNats have no internal structure.

imagine Int as

data Int = .. -2 | -1 | 0 | 1 | 2 ...

and Nat as

data Nat = 0 | 1 | 2 | 3 | 4

Each constructor is completely distinct.

Fine for most applications.

The structure of Vec above and structure of Nat have nothing in common.
So can’t take advantage of shared structure to help with type-safety in impl.
Everything we wrote was pretty much implemented using “unsafe” functions.

inductive type-level nats:

data Nat = Z | S Nat deriving Eq

get singletons for Nat:
-}
$(STH.singletons [d|
  data Nat = Z | S Nat
    deriving Eq
  |])
{-
creates:

data instance S.Sing :: Nat -> DK.Type where
  SZ :: S.Sing 'Z
  SS :: S.Sing n -> S.Sing ('S n)

now one Sing n for every n
-}

data Vec :: Nat -> DK.Type -> DK.Type where
  VNil ::                 Vec  'Z    a
  (:+) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :+
{-
usage of nil/cons keeps track of length

------------------------------------------------------------------------------
Type-level Guarantees are Structurally Free

There is no "unsafe" way to construct a Vec.

non-structural mapVec required implementing it correctly.

Here it is guaranteed to preserve the lengths
-}
mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f = \case
  VNil    -> VNil
  a :+ as -> f a :+ mapVec f as

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec  = \case
  VNil -> \case
    VNil -> VNil
  a :+ as -> \case
    b :+ bs -> (a,b) :+ zipVec as bs
{-
------------------------------------------------------------------------------
Type-Level Arithmentic

TypeNats provided +

Now need
-}
type family (n :: Nat) + (m :: Nat) :: Nat where
  'Z   + m = m
  'S n + m = 'S (n + m)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
(++)  = \case
  VNil    -> id
  x :+ xs -> \ys -> x :+ (xs ++ ys)
{-
Must ensure GHC can verify final vector has length n + m.
Possible here because + and ++ have the same structure.
- if 1st item is a Z-y thing   : return 2nd
- if 2nd item is a consy thing : return 2nd item consed with rest of 1st item

Since their structures align then structural properties can be proved
by exploiting that shared inductive structure.

This will not work for functions that do not match structure of type family.
Sometimes structural properties get in the way of proving.
E.g., : snoc

------------------------------------------------------------------------------
Indexing

previously
index via an abstract Finite type, where Finite n represented type of all indices to a Vec n a
-}
data Fin :: Nat -> DK.Type where
  FZ ::          Fin ('S n) -- ^ can create 0th index if Vec n is non-empty
  FS :: Fin n -> Fin ('S n) -- ^ given an ith index into a vector of size n
                            -- ... then have an i+1th index into a vector of size ('S n)
deriving instance Show (Fin n)
{-
no inhabitants of Fin 'Z
so cannot index an empty vector
-}
index :: Fin n -> Vec n a -> a
index  = \case
  FZ -> \case
    x :+  _ -> x
  FS i -> \case
    _ :+ xs -> index i xs
{-
------------------------------------------------------------------------------
Generating

replicate :: a -> Vec n a

not possible : need to pattern match on 'n', but types are erased

previous vector type needed KnownNat n constraint to reflect 'n' to value

do something similar using singletons

Given a value of type Sing n, can now pattern match
-}
singSize :: S.Sing (n :: Nat) -> String
singSize  = \case
  -- here, n is 'Z
  SZ        -> "zero"
  -- here, n is ('S 'Z)
  SS SZ     -> "one"
  -- here, n is ('S ('S n))
  SS (SS _) -> "big"
{-
general technique: use a singleton to pattern match based on type

because Nat is inductive the singletons are also inductive as well
-}
-- explicit
replicateS :: S.Sing n -> a -> Vec n a
replicateS  = \case
  SZ   -> const VNil
  SS l -> \x -> x :+ replicateS l x

-- implicit
replicateS' :: S.SingI n => a -> Vec n a
replicateS'  = replicateS S.sing
{-
KnownNat : reflect GHC.TypeNats.Nat to a Sing
SingI    : reflect any type that has singletons defined to its corresponding Sing

------------------------------------------------------------------------------
Generating with indices

generate using inductive Fin and Nat
pattern when working with inductive types
-}
generateS :: S.Sing n -> (Fin n -> a) -> Vec n a
generateS  = \case
  SZ   -> const VNil
  SS l -> \f -> f FZ :+ generateS l (f . FS) -- <- deconstruct DOWN; construct UP

generateS' :: S.SingI n => (Fin n -> a) -> Vec n a
generateS'  = generateS S.sing
{-
------------------------------------------------------------------------------
Between Sized and Unsized
-}
toList :: Vec n a -> [a]
toList  = \case
  VNil      -> []
  (a :+ as) -> a : toList as
{-
API of converting unsized to sized vectors is the same as sized-vectors
-}
withVec :: [a] -> (forall n. S.Sing n -> Vec n a -> r) -> r
withVec  = \case
  -- give f the zero singleton and empty vector
  []   -> \f -> f SZ VNil
  -- convert tail 'as' into a vector 'asv' and its corresponding length-singleton 'l'
  -- give f the "correct" length singleton of the complete vector 'SS l'
  -- and the complete vector (x :+ asv)
  a:as -> \f -> withVec as $ \l asv -> f (SS l) (a :+ asv)
{-
compared to non-structural withVec, GHC ensures length of vector given to f is correct

------------------------------------------------------------------------------
Verifying properties
-}
vecLength :: Vec n a -> S.Sing n
vecLength  = \case
  VNil    -> SZ
  _ :+ xs -> SS (vecLength xs)
{-
note: length of non-structural Vec required unsafe operations
-}
-- explicit
exactLengthS :: S.Sing m -> Vec n a -> Maybe (Vec m a)
exactLengthS sM v = case sM SD.%~ vecLength v of
  SD.Proved SD.Refl -> Just v
  SD.Disproved   _  -> Nothing

-- implicit
exactLengthS' :: S.SingI m => Vec n a -> Maybe (Vec m a)
exactLengthS'  = exactLengthS S.sing

-- Maybe in case Sing m and Vec n do not align
exactLengthInductiveS :: S.Sing m -> Vec n a -> Maybe (Vec m a)
exactLengthInductiveS= \case
  SZ -> \case
    VNil   -> Just VNil
    _ :+ _ -> Nothing
  SS l -> \case
    VNil    -> Nothing
    x :+ xs -> (x :+) <$> exactLengthInductiveS l xs

exactLengthInductiveS' :: S.SingI m => Vec n a -> Maybe (Vec m a)
exactLengthInductiveS' = exactLengthInductiveS S.sing
{-
get a witness of a property, then pattern match on witness

e.g., witness n <= m (and construct such a witness)
-}
data LTE :: Nat -> Nat -> DK.Type where
  LEZ ::            LTE  'Z        n
  LES :: LTE n m -> LTE ('S n) ('S m)

isLTE :: Sing n -> Sing m -> Decision (LTE n m)
isLTE = \case
  SZ   -> \_ -> SD.Proved LEZ
  SS n -> \case
    SZ   -> SD.Disproved $ \case       -- EmptyCase
    SS m -> case isLTE n m of
      SD.Proved l    -> SD.Proved $ LES l
      SD.Disproved p -> SD.Disproved $ \case
        LES l -> p l

-- check property
-- return given vector if its length is at least the length by sN

-- explicit
atLeastS :: S.Sing n -> Vec m a -> Maybe (LTE n m, Vec m a)
atLeastS sN v = case isLTE sN (vecLength v) of
    SD.Proved l    -> Just (l, v)
    SD.Disproved _ -> Nothing

atLeastS' :: SingI n => Vec m a -> Maybe (LTE n m, Vec m a)
atLeastS' = atLeastS S.sing

-- given proof, then take

takeVec :: LTE n m -> Vec m a -> Vec n a
takeVec = \case
  LEZ   -> const VNil
  LES l -> \case
    x :+ xs -> x :+ takeVec l xs

takeVecMaybeS :: Sing n -> Vec m a -> Maybe (Vec n a)
takeVecMaybeS sN v = uncurry takeVec <$> atLeastS sN v

takeVecMaybeS' :: SingI n => Vec m a -> Maybe (Vec n a)
takeVecMaybeS' v = uncurry takeVec <$> atLeastS' v
{-
------------------------------------------------------------------------------
In the Real World

Structural vector is more like a list than a vector.

Use-case: lazy streaming (but cases where you know length in advance are rare)
GHC can’t handle infinite structural Vecs

If using man nductive types, then structural Vec works well alongside them.

RECOMMEND: canonical package offering structual Vec : TYPE-COMBINATORS
-}
