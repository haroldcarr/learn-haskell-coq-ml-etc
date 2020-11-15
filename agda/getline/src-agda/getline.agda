module getline where

-- https://github.com/alhassy/AgdaCheatSheet#interacting-with-the-real-world-compilation-haskell-and-io

open import Data.Nat                 using (ℕ; suc)
open import Data.Nat.Show            using (show)
open import Data.Char                using (Char)
open import Data.List                as L using (map; sum; upTo)
open import Function                 using (_$_; const; _∘_)
open import Data.String              as S using (String; _++_; fromList)
open import Agda.Builtin.Unit        using (⊤)
open import Codata.Musical.Colist    using (take)
open import Codata.Musical.Costring  using (Costring)
open import Data.Vec.Bounded         as B using (toList)
open import Agda.Builtin.Coinduction using (♯_)
open import IO                       as IO using (run ; putStrLn ; IO)
import      IO.Primitive             as Primitive

infixr 1 _>>=_ _>>_

_>>=_ : ∀ {ℓ} {α β : Set ℓ} → IO α → (α → IO β) → IO β
this >>= f = ♯ this IO.>>= λ x → ♯ f x

_>>_ : ∀{ℓ} {α β : Set ℓ} → IO α → IO β → IO β
x >> y = x >>= const y

postulate
  getLine∞ : Primitive.IO Costring

{-# FOREIGN GHC
  toColist :: [a] -> MAlonzo.Code.Codata.Musical.Colist.AgdaColist a
  toColist []       = MAlonzo.Code.Codata.Musical.Colist.Nil
  toColist (x : xs) =
    MAlonzo.Code.Codata.Musical.Colist.Cons x (MAlonzo.RTE.Sharp (toColist xs))
#-}

{- Haskell's prelude is implicitly available; this is for demonstration. -}
{-# FOREIGN GHC import Prelude as Haskell #-}
{-# COMPILE GHC getLine∞  = fmap toColist Haskell.getLine #-}

-- (1)
-- getLine : IO Costring
-- getLine = IO.lift getLine∞

getLine : IO String
getLine = IO.lift
  $ getLine∞ Primitive.>>= (Primitive.return ∘ S.fromList ∘ B.toList ∘ take 100)

postulate readInt  : L.List Char → ℕ
{-# COMPILE GHC readInt = \x -> read x :: Integer  #-}

main : Primitive.IO ⊤
main = run do putStrLn "Hello, world! I'm a compiled Agda program!"

              putStrLn "What is your name?"
              name ← getLine

              putStrLn "Please enter a number."
              num ← getLine
              let tri = show $ sum $ upTo $ suc $ readInt $ S.toList num
              putStrLn $ "The triangle number of " ++ num ++ " is " ++ tri

              putStrLn "Bye, "
              -- IO.putStrLn∞ name  {- If we use approach (1) above. -}
              putStrLn $ "\t" ++ name
