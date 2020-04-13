module Lib where

import Control.Monad (join, void)
import Data.Functor (($>))
import Data.Either.Validation

{-
Phil Freeman
https://twitter.com/paf31/status/1108518386877628416

My new favorite Haskell trick is using Compose (Validation e) IO to
implement poor-man's transactions. It's Applicative for free, and you
can interpret it in ExceptT e IO easily. Thanks to @fried_brice for
implementing this pattern in our code at @Lumi lately!

In more detail: suppose you want to perform many tasks in IO, and want
it to be everything-or-nothing. Absent a transaction coordinator, you
can at least do some initial verification and prepare as much work as
possible, if you push the Either outside the IO.

Daniel Brice:
https://gist.github.com/friedbrice/18f15dad17b8fbe524c1c994823d8aeb

Phil Freeman
And if you used newtypes for refinements you could use Applicative to compose them:
checkPickles :: Pickles -> Compose _ _ AdequatePickles
checkTurkey :: Turkey -> Compose _ _ FreshTurkey
... <$> checkPickles pickles <*> checkTurkey turkey
and so on :)
-}

doIt :: IO ()
doIt = do
  putStrLn "\n\nDOING"
  void (sudoMakeSandwich Person)
  putStrLn "DONE"

----
-- Validation (Imagine this comes from a library.)
----

runValidation :: (e -> b) -> (a -> b) -> Validation e a -> b
runValidation f _ (Failure e) = f e
runValidation _ g (Success x) = g x

assert :: Semigroup e => e -> Bool -> Validation e ()
assert e p = if p then pure () else Failure e

-- Whereas `Either` holds up to one error, `Validation` can hold many, because "Left" is a Monoid.

----
-- Data Layer (Imagine this talks to your database, or an API.)
----

data    Person   = Person deriving Show
newtype Pickles  = Pickles  { numPickles :: Int } deriving Show
newtype Mustard  = Mustard  { isSpicy    :: Bool } deriving Show
data    Turkey   = Turkey   { freshness  :: Double, numTurkey :: Int  } deriving Show
data    Bread    = Bread    { moldy      :: Bool  , sliced    :: Bool } deriving Show
data    Sandwich = Sandwich

lookupBread :: Person -> IO Bread
lookupBread _ = return $ Bread False False

lookupMustard :: Person -> IO Mustard
lookupMustard _ = return $ Mustard True

lookupTurkey :: Person -> IO Turkey
lookupTurkey _ = return $ Turkey 0.9 3

lookupPickles :: Person -> IO Pickles
lookupPickles _ = return $ Pickles 2

-- These get called if values returned from lookup* are valid.
-- They charge the person and update the inventory.
sliceBread :: Person -> Bread -> IO ()
sliceBread  = say

updateMustard :: Person -> Mustard -> IO ()
updateMustard  = say

updateTurkey :: Person -> Turkey -> IO ()
updateTurkey  = say

updatePickles :: Person -> Pickles -> IO ()
updatePickles  = say

say :: (Show a, Show b) => a -> b -> IO ()
say a b = do putStr (show a); putStr " "; print b; return ()

----
-- Application Layer (Here's the stuff your program does!)
----

-- `sudoMakeSandwich` gathers the data needed by `makeSandwich` and will
-- either make the sandwich according to `makeSandwich`s instructions or
-- will print the errors listed by `makeSandwich`.
sudoMakeSandwich :: Person -> IO Sandwich
sudoMakeSandwich person = do
  bread   <- lookupBread person
  mustard <- lookupMustard person
  turkey  <- lookupTurkey person
  pickles <- lookupPickles person
  join (runValidation logErrorsAndPanic pure
        (makeSandwich person pickles mustard turkey bread))

-- `makeSandwich` describes making a sandwich (or lists the reasons we can't)
makeSandwich :: Person -> Pickles -> Mustard -> Turkey -> Bread
             -> Validation [String] (IO Sandwich)
makeSandwich person pickles mustard turkey bread =
  (  assert
      ["Bread must not be moldy"]
      (not . moldy $ bread)
  *> assert
      ["Mustard must be spicy"]
      (isSpicy mustard)
  *> assert
      ["Turkey must be fresh"]
      (freshness turkey >= 0.7)
  *> assert
      ["Must have at least 2 slices of turkey"]
      (numTurkey turkey >= 2)
  *> assert
      ["Must have at least 2 pickles"]
      (numPickles pickles >= 2)
  )
  $>
  (do
    if not (sliced bread)
      then sliceBread person bread -- if not already sliced, slice it
      else pure ()
    updateMustard person mustard -- TODO needs an amount
    updateTurkey  person turkey { numTurkey = numTurkey turkey - 2
                                , freshness = freshness turkey - 0.05  }
    updatePickles person pickles { numPickles = numPickles pickles - 2 }
    pure Sandwich
  )

-- Don't do this part in real life!
logErrorsAndPanic :: [String] -> IO a
logErrorsAndPanic errs = do
  putStrLn ""
  mapM_ putStrLn errs
  error "I was made to understand there were sandwiches here."
