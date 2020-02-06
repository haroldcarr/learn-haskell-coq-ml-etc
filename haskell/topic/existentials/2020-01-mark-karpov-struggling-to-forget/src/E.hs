{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
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

mkHc :: MonadThrow m
  => ('Posix `Or` 'Win)  pc
  -> ('Abs   `Or` 'Rel)  bc
  -> ('Dir   `Or` 'File) tc
  -> FilePath
  -> (forall p b t. (pc p, bc b, tc t) => m (Path p b t))
mkHc First  First  First  fp = pure (PosixAbsDir  fp)
mkHc First  First  Second fp = pure (PosixAbsFile fp)
mkHc First  Second First  fp = pure (PosixRelDir  fp)
mkHc First  Second Second fp = pure (PosixRelFile fp)
mkHc Second First  First  fp = pure (WinAbsDir    fp)
mkHc Second First  Second fp = pure (WinAbsFile   fp)
mkHc Second Second First  fp = pure (WinRelDir    fp)
mkHc Second Second Second fp = pure (WinRelFile   fp)
mkHc Any    _      _      _  = throwM AnyPlatform
mkHc _      Any    _      _  = throwM AnyBase
mkHc _      _      Any    _  = throwM AnyPType

data MyException = AnyPlatform | AnyBase | AnyPType deriving (Eq, Show)
instance Exception MyException

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

t01 :: Spec
t01  = do
  it "t01a" $      mk First First First "/tmp" (\_ -> pure 1) `shouldBe` Just 1
  it "t01b" $      mk posix abs   dir   "/tmp" (\_ -> pure 1) `shouldBe` Just 1
  it "t01c" $ case mk posix abs   dir   "/tmp" (\_ -> throwM AnyBase) of
    Left  e  -> fromException e `shouldBe` Just AnyBase
    Right () -> True            `shouldBe` False
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
isPosix  = \case
  a@PosixAbsDir  {} -> Just a
  a@PosixAbsFile {} -> Just a
  a@PosixRelDir  {} -> Just a
  a@PosixRelFile {} -> Just a
  _                 -> Nothing

pattern IsWin :: Path 'Win b t -> Path p b t
pattern IsWin path <- (isWin -> Just path)

isWin :: Path p b t -> Maybe (Path 'Win b t)
isWin  = \case
  a@WinAbsDir  {} -> Just a
  a@WinAbsFile {} -> Just a
  a@WinRelDir  {} -> Just a
  a@WinRelFile {} -> Just a
  _                 -> Nothing

pattern IsAbs :: Path p 'Abs t -> Path p b t
pattern IsAbs path <- (isAbs -> Just path)

isAbs :: Path p b t -> Maybe (Path p 'Abs t)
isAbs  = \case
  a@PosixAbsDir  {} -> Just a
  a@PosixAbsFile {} -> Just a
  a@WinAbsDir    {} -> Just a
  a@WinAbsFile   {} -> Just a
  _                 -> Nothing

pattern IsRel :: Path p 'Rel t -> Path p b t
pattern IsRel path <- (isRel -> Just path)

isRel :: Path p b t -> Maybe (Path p 'Rel t)
isRel  = \case
  a@PosixRelDir  {} -> Just a
  a@PosixRelFile {} -> Just a
  a@WinRelDir    {} -> Just a
  a@WinRelFile   {} -> Just a
  _                 -> Nothing

pattern IsDir :: Path p b 'Dir -> Path p b t
pattern IsDir path <- (isDir -> Just path)

isDir :: Path p b t -> Maybe (Path p b 'Dir)
isDir  = \case
  a@PosixAbsDir  {} -> Just a
  a@PosixRelDir  {} -> Just a
  a@WinAbsDir    {} -> Just a
  a@WinRelDir    {} -> Just a
  _                 -> Nothing

pattern IsFile :: Path p b 'File -> Path p b t
pattern IsFile path <- (isFile -> Just path)

isFile :: Path p b t -> Maybe (Path p b 'File)
isFile  = \case
  a@PosixAbsFile {} -> Just a
  a@PosixRelFile {} -> Just a
  a@WinAbsFile   {} -> Just a
  a@WinRelFile   {} -> Just a
  _                 -> Nothing

handleHc :: Monad m => Path p b t -> m String
handleHc path = case (path, path, path) of
  (IsPosix _x, IsAbs _y, IsDir  _z) -> pure "posix abs dir"
  (IsPosix _x, IsAbs _y, IsFile _z) -> pure "posix abs file"
  (IsPosix _x, IsRel _y, IsDir  _z) -> pure "posix rel dir"
  (IsPosix _x, IsRel _y, IsFile _z) -> pure "posix rel file"
  (IsWin   _x, IsAbs _y, IsDir  _z) -> pure "win   abs dir"
  (IsWin   _x, IsAbs _y, IsFile _z) -> pure "win   abs file"
  (IsWin   _x, IsRel _y, IsDir  _z) -> pure "win   rel dir"
  (IsWin   _x, IsRel _y, IsFile _z) -> pure "win   rel file"

handleHc' :: Monad m => Path p b t -> m String
handleHc' path = case path of
  (PosixAbsDir  fp) -> pure ("posix abs dir"  ++ fp)
  (PosixAbsFile fp) -> pure ("posix abs file" ++ fp)
  (PosixRelDir  fp) -> pure ("posix rel dir"  ++ fp)
  (PosixRelFile fp) -> pure ("posix rel file" ++ fp)
  (WinAbsDir    fp) -> pure ("win   abs dir"  ++ fp)
  (WinAbsFile   fp) -> pure ("win   abs file" ++ fp)
  (WinRelDir    fp) -> pure ("win   rel dir"  ++ fp)
  (WinRelFile   fp) -> pure ("win   rel file" ++ fp)
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
