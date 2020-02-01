{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}

module E where

import           Control.Monad.Catch
import           Data.Kind
import           Prelude             hiding (abs)
import           Test.Hspec

{-
Mark Karpov
Struggling to forget
January 3, 2020
https://markkarpov.com/post/struggling-to-forget.html

use case and a way to implement conditional fixing
of existentially quantified variables
by universally quantified variables

example : typed file paths, like https://hackage.haskell.org/package/path
flaw of path is that it forces the users to know too much about the paths:

-- | Path of some base and type.
--
-- The type variables are:
--
--   * @b@ — base, the base location of the path; absolute or relative.
--   * @t@ — type, whether file or directory.
newtype Path b t = Path FilePath
  deriving (Data, Typeable, Generic)

To create a value of the type Path one has to choose what to expect (choose b and t):

parseAbsDir  :: MonadThrow m => FilePath -> m (Path Abs Dir)
parseRelDir  :: MonadThrow m => FilePath -> m (Path Rel Dir)
parseAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseRelFile :: MonadThrow m => FilePath -> m (Path Rel File)

instead:
-}

data Platform = Posix | Win
data Base     = Abs   | Rel
data PType    = Dir   | File

data Path (p :: Platform) (b :: Base) (t :: PType) where
  PosixAbsDir  :: FilePath -> Path 'Posix 'Abs 'Dir
  PosixAbsFile :: FilePath -> Path 'Posix 'Abs 'File
  PosixRelDir  :: FilePath -> Path 'Posix 'Rel 'Dir
  PosixRelFile :: FilePath -> Path 'Posix 'Rel 'File
  WinAbsDir    :: FilePath -> Path 'Win   'Abs 'Dir
  WinAbsFile   :: FilePath -> Path 'Win   'Abs 'File
  WinRelDir    :: FilePath -> Path 'Win   'Rel 'Dir
  WinRelFile   :: FilePath -> Path 'Win   'Rel 'File

{-
The 3 type indices lead (on purpose) to a combinatorial explosion.
Want to make some indices existential while leaving others fixed.
Could use existential wrappers but there are too many possible combinations:

data PathSomePlatform b t = <...>
data PathSomeBase     p t = <...>
<...>
data PathSomePlatformSomeBase t = <...> -- both platform and base are existential
<...>

so

make a smart constructor which fixes arbitrary subsets of type indices
i.e., tell the type system that if absolute path expected then b ~ 'Abs,
but leave unknown things existential, to to be discovered later by case-analysis.

Solution

use CPS (because GHC does not have proper existential quantification)
-}
{-
mk0
  :: MonadThrow m
  => -- ...
     FilePath
  -> (forall p b t. Path p b t -> m r)
  -> m r
-}
-- conditional constraining may have something to do with constraints

data ((x :: k) `Or` (y :: k)) (c :: k -> Constraint) where
  Any    :: (x `Or` y) Unconstrained
  First  :: (x `Or` y) ((~) x)
  Second :: (x `Or` y) ((~) y)

-- HC
instance Show (Or x y z) where
  show Any    = "Any"
  show First  = "First"
  show Second = "Second"

class Unconstrained (x :: k)
instance Unconstrained x

mk :: MonadThrow m
  => ('Posix `Or` 'Win)  pc
  -> ('Abs   `Or` 'Rel)  bc
  -> ('Dir   `Or` 'File) tc
  -> FilePath
  -> (forall p b t. (pc p, bc b, tc t) => Path p b t -> m r)
  -> m r
mk First  First  First  fp f = f (PosixAbsDir  fp)
mk First  First  Second fp f = f (PosixAbsFile fp)
mk First  Second First  fp f = f (PosixRelDir  fp)
mk First  Second Second fp f = f (PosixRelFile fp)
mk Second First  First  fp f = f (WinAbsDir    fp)
mk Second First  Second fp f = f (WinAbsFile   fp)
mk Second Second First  fp f = f (WinRelDir    fp)
mk Second Second Second fp f = f (WinRelFile   fp)
mk Any    _      _      _  _ = throwM AnyPlatform
mk _      Any    _      _  _ = throwM AnyBase
mk _      _      Any    _  _ = throwM AnyPType

data MyException = AnyPlatform | AnyBase | AnyPType deriving Show
instance Exception MyException

t01 :: Spec
t01  = do
  it "t01a" $ mk First First First "/tmp" (\_ -> pure 1) `shouldBe` Just 1
  it "t01b" $ mk posix abs   dir   "/tmp" (\_ -> pure 1) `shouldBe` Just 1
  it "t01c" $ case mk posix abs   dir   "/tmp" (\_ -> throwM (SomeException AnyBase)) of
    Left (SomeException _) -> True  `shouldBe` True
    Right ()               -> False `shouldBe` True

-- erogonomics:

any   :: (x `Or` y) Unconstrained
any    = Any

posix :: ('Posix `Or` 'Win)  ((~) 'Posix)
posix  = First

win   :: ('Posix `Or` 'Win)  ((~) 'Win)
win    = Second

abs   :: ('Abs   `Or` 'Rel)  ((~) 'Abs)
abs    = First

rel   :: ('Abs   `Or` 'Rel)  ((~) 'Rel)
rel    = Second

dir   :: ('Dir   `Or` 'File) ((~) 'Dir)
dir    = First

file  :: ('Dir   `Or` 'File) ((~) 'File)
file   = Second

{-
In 'any' case : type index fixed during parsing, but not immediately known at the type level.

Case analysis on the 'Path p b t' type
-------------------------------------

The types are not constrained unnecessarily,
instead discovered when needed by performing case-analysis.

E.g.,
-}

pattern IsPosix :: Path 'Posix b t -> Path p b t
pattern IsPosix path <- (isPosix -> Just path)

isPosix :: Path p b t -> Maybe (Path 'Posix b t)
isPosix = \case
  PosixAbsDir  path -> Just (PosixAbsDir  path)
  PosixAbsFile path -> Just (PosixAbsFile path)
  PosixRelDir  path -> Just (PosixRelDir  path)
  PosixRelFile path -> Just (PosixRelFile path)
  _                 -> Nothing

{-
using above enables establishing that p ~ 'Posix in one branch of execution
while handing the opposite case in the other branch.

This does the same thing done routinely with sum types, but for type-level indices.

This is similar to using bare GADTs except the correspondence between types and data constructors
is less precise and you do not discover more than necessary unless you want to
--- patterns like this can also be nested

the main difficulty is now resolved

but still not clear how to handle the any case for something like Dir vs File.

A trailing slash may guarantee that you have a directory path,
but if it is not there and the user doesn’t say what he/she wants,
it is hard to guess what you have—a directory or a file.
-}
