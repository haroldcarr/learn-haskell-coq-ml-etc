module Main

import Data.Vect

------------------------------------------------------------------------------
-- read a vector of known length

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen  Z    = pure []
readVectLen (S k) = do
  x  <- getLine
  xs <- readVectLen k
  pure (x :: xs)

{-
:exec readVectLen 4
-}

------------------------------------------------------------------------------
-- read a vector of unknown length

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (VectUnknown String)
readVect = do
  x <- getLine
  if (x == "")
    then pure (MkVect _ [])
    else do
      MkVect _ xs <- readVect
      pure (MkVect _ (x :: xs))

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) =
  putStrLn (show xs ++ " (length " ++ show len ++ ")")

{-
:exec readVect >>= printVect
-}

------------------------------------------------------------------------------
-- dependent pairs : general solution to above
-- when needing unknown input to define a dependent type

-- type of 2nd element computed from value of 1st
anyVect : (n : Nat ** Vect n String)
anyVect = (3 ** ["Rod", "Jane", "Freddy"])

sndAnyVect : Vect 3 String
sndAnyVect = snd anyVect

------------------------------------------------------------------------------

readVect' : IO (len ** Vect len String)
readVect' = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do
      (_ ** xs) <- readVect'
      pure (_ ** x :: xs)

{-
:exec readVect' >>= printLn
-}

------------------------------------------------------------------------------

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector (blank line to end):"
  (len1 ** vec1) <- readVect'
  putStrLn "Enter second vector (blank line to end):"
  (len2 ** vec2) <- readVect'
  case exactLength len1 vec2 of
    Nothing    => putStrLn "Vectors are different lengths"
    Just vec2' => printLn (zip vec1 vec2')

{-
:exec zipInputs
-}
