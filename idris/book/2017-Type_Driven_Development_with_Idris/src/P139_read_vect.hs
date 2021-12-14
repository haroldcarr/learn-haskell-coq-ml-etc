{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeInType         #-}

module P139_read_vect where

-- import Data.Singletons
-- import Data.Singletons.Sigma

data Nat = Z | S Nat deriving Show

data Vec (a :: *) (n :: Nat) where
  V0   ::                 Vec a  Z
  (:>) :: a -> Vec a n -> Vec a (S n)
infixr 5 :>

deriving instance Eq   a => Eq   (Vec a n)
deriving instance Show a => Show (Vec a n)

data Natty :: Nat -> * where
  Zy ::            Natty  Z
  Sy :: Natty n -> Natty (S n)

deriving instance Show (Natty a)

------------------------------------------------------------------------------
-- read a vector of known length

readVecLen :: Natty m -> IO (Vec String m)
readVecLen  Zy    = pure V0
readVecLen (Sy k) = do
  x  <- getLine
  xs <- readVecLen k
  pure (x :> xs)


-- ------------------------------------------------------------------------------
-- -- read a vector of unknown length

data VecUnknown (a :: *) :: * where
   MkVect :: Natty len -> Vec a len -> VecUnknown a

deriving instance Show a => Show (VecUnknown a)

readVec :: IO (VecUnknown String)
readVec  = readVec' Zy
 where
  readVec' n = do
    x <- getLine
    if x == ""
      then pure (MkVect n V0)
      else do
        MkVect len xs <- readVec
        pure (MkVect (Sy len) (x :> xs))
-- ------------------------------------------------------------------------------
-- dependent pairs : general solution to above
-- when needing unknown input to define a dependent type



{-
-- type of 2nd element computed from value of 1st
anyVect :: Sigma Nat (TyCon (Vec String))
anyVect = S (S (S Z)) :&: "Rod" :> "Jane" :> "Freddy" :> V0

sndAnyVect :: Vec String (S (S (S Z)))
sndAnyVect = snd anyVect

-- ------------------------------------------------------------------------------

{-# TERMINATING #-}
readVec' : IO (Σ Nat (λ n -> Vec String n))
readVec' = do
  x <- getLine
  if (x == "")
    then return (_ , [])
    else do
      (_ , xs) <- readVec'
      return (_ , x ∷ xs)

------------------------------------------------------------------------------

exactLength : {a : Set} {m : Nat} -> (len : Nat) -> (input : Vec a m) -> Maybe (Vec a len)
exactLength {_} {m} len input
  with m Data.Nat.≟ len
... | yes p rewrite p = just input
... | no  _           = nothing

printVecStringString : {len : Nat} -> Vec (String × String) len -> String
printVecStringString      [] = "[]"
printVecStringString ((l , r) ∷ xs) =
 "(" Data.String.++ l Data.String.++ " , " Data.String.++ r Data.String.++ ")"
 Data.String.++ " ∷ " Data.String.++ printVecStringString xs

zipInputs : IO ⊤
zipInputs = do
  IO.putStrLn "Enter 1st vector (blank line to end):"
  (len1 , vec1) <- readVec'
  IO.putStrLn "Enter 2nd vector (blank line to end):"
  (len2 , vec2) <- readVec'
  case exactLength len1 vec2 of λ where
    nothing  -> IO.putStrLn "nothing"
    (just v) ->
      let vz = Data.Vec.zip vec1 v
       in IO.putStrLn (printVecStringString vz)

------------------------------------------------------------------------------
-}
main :: IO ()
main  = do
  putStrLn "--------------------------------------------------"
  putStrLn "readVecLen 4"
  putStrLn "enter 4 lines:"
  vl <- readVecLen (Sy (Sy (Sy (Sy Zy))))
  putStrLn "result:"
  print vl
  putStrLn "--------------------------------------------------"
  putStrLn "readVec"
  putStrLn "enter lines, then a blank line:"
  vu <- readVec
  putStrLn "result:"
  print vu
{-
  IO.putStrLn "--------------------------------------------------"
  IO.putStrLn "readVec'"
  IO.putStrLn "enter lines, then a blank line:"
  (_ , v) <- readVec'
  IO.putStrLn "result:"
  IO.putStrLn (printVecString v)
  IO.putStrLn "--------------------------------------------------"
  IO.putStrLn "zipInputs"
  zipInputs
-}
