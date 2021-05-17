module hcio where

-- https://github.com/alhassy/AgdaCheatSheet#interacting-with-the-real-world-compilation-haskell-and-io

open import Agda.Builtin.Coinduction using (♯_)
open import Agda.Builtin.Unit        using (⊤)
open import Codata.Musical.Colist    as CL using (take)
open import Codata.Musical.Costring  using (Costring)
open import Data.String              as S using (String; fromList)
open import Data.Vec.Bounded         as B using (toList)
open import Function                 using (const; _∘_)
open import IO                       as IO using ()
import      IO.Primitive             as Primitive

infixr 1 _>>=_ _>>_

_>>=_ : ∀ {ℓ} {α β : Set ℓ} → IO.IO α → (α → IO.IO β) → IO.IO β
this >>= f = ♯ this IO.>>= λ x → ♯ f x

_>>_ : ∀{ℓ} {α β : Set ℓ} → IO.IO α → IO.IO β → IO.IO β
x >> y = x >>= const y

postulate
  getLine∞ : Primitive.IO Costring

{-# FOREIGN GHC
  toColist :: [a] -> MAlonzo.Code.Codata.Musical.Colist.AgdaColist a
  toColist []       = MAlonzo.Code.Codata.Musical.Colist.Nil
  toColist (x : xs) = MAlonzo.Code.Codata.Musical.Colist.Cons x (MAlonzo.RTE.Sharp (toColist xs))
#-}

{-# FOREIGN GHC import Prelude as Haskell #-} -- rename for clarity
{-# COMPILE GHC getLine∞ = fmap toColist Haskell.getLine #-}

getLine : IO.IO String
getLine =
  IO.lift (getLine∞ Primitive.>>= (Primitive.return ∘ S.fromList ∘ B.toList ∘ CL.take 100))


