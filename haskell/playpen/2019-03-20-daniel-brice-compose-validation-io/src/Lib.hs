module Lib where

import Control.Monad (join, void)
import Data.Functor (($>))
-- import Control.Monad.Except
-- import Data.Either.Validation
-- import Data.Functor.Compose

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
  void $ sudoMakeSandwich Person
  putStrLn "DONE"

----
-- Validation (Imagine this comes from a library.)
----

data Validation e a = Errors [e] | Ok a
    deriving (Eq, Ord, Read, Show)

runValidation :: ([e] -> b) -> (a -> b) -> Validation e a -> b
runValidation f _ (Errors errs) = f errs
runValidation _ g (Ok x) = g x

assert :: e -> Bool -> Validation e ()
assert e p = if p then pure () else Errors [e]

instance Functor (Validation e) where
    fmap _ (Errors errs) = Errors errs
    fmap f (Ok a) = Ok (f a)

-- Whereas `Either` holds up to one error, `Validation` can hold many.
-- For example:
--     assert "This" False *> assert "That" False == Errors ["This", "That"]
instance Applicative (Validation e) where
    pure = Ok

    Errors errs <*> Errors errs' = Errors (errs <> errs')
    Errors errs <*> _            = Errors errs
    _           <*> Errors errs  = Errors errs
    Ok f        <*> Ok x         = Ok (f x)

----
-- Data Layer (Imagine this talks to your database, or an API.)
----

data Person = Person
newtype Pickles = Pickles { numPickles :: Int }
newtype Mustard = Mustard { isSpicy :: Bool }
data Turkey = Turkey { freshness :: Double, numTurkey :: Int }
data Bread = Bread { moldy :: Bool, sliced :: Bool }
data Sandwich = Sandwich

lookupPickles :: Person -> IO Pickles
lookupPickles _ = return $ Pickles 1

lookupMustard :: Person -> IO Mustard
lookupMustard _ = return $ Mustard True

lookupTurkey :: Person -> IO Turkey
lookupTurkey _ = return $ Turkey 0.9 3

lookupBread :: Person -> IO Bread
lookupBread _ = return $ Bread False True

sliceBread :: Person -> Bread -> IO ()
sliceBread  _ _ = return ()

updateTurkey :: Person -> Turkey -> IO ()
updateTurkey _ _ = return ()

updateMustard :: Person -> Mustard -> IO ()
updateMustard _ _ = return ()

updatePickles :: Person -> Pickles -> IO ()
updatePickles _ _  = return ()

----
-- Application Layer (Here's the stuff your program does!)
----

-- `sudoMakeSandwich` gathers the data needed by `makeSandwich` and will
-- either make the sandwich according to `makeSandwich`s instructions or
-- will print the errors listed by `makeSandwich`.
sudoMakeSandwich :: Person -> IO Sandwich
sudoMakeSandwich person = do
    pickles <- lookupPickles person
    mustard <- lookupMustard person
    turkey  <- lookupTurkey person
    bread   <- lookupBread person

    join (runValidation logErrorsAndPanic pure $
          makeSandwich person pickles mustard turkey bread)

-- `makeSandwich` describes making a sandwich (or lists the reasons we can't)
makeSandwich :: Person -> Pickles -> Mustard -> Turkey -> Bread
             -> Validation String (IO Sandwich)
makeSandwich person pickles mustard turkey bread =
    (assert
        "Must have at least 2 pickles"
        (numPickles pickles >= 2)
    *> assert
        "Mustard must be spicy"
        (isSpicy mustard)
    *> assert
        "Turkey must be fresh"
        (freshness turkey >= 0.7)
    *> assert
        "Must have at least 2 slices of turkey"
        (numTurkey turkey >= 2)
    *> assert
        "Bread must not be moldy"
        (not . moldy $ bread))
    $> (do
        -- We must have sliced bread to make a sandwich, but it's not
        -- a validation because we can slice it ourselves if necessary.
        if sliced bread
            then pure ()
            else sliceBread person bread

        -- Spread the mustard.
        updateMustard person mustard

        -- Use up the pickles.
        updatePickles
            person
            pickles
                { numPickles = numPickles pickles - 2
                }

        -- Use up the turkey.
        updateTurkey
            person
            turkey
                { numTurkey = numTurkey turkey - 2
                , freshness = freshness turkey - 0.05
                }

        pure Sandwich
        )

-- Don't do this part in real life!
logErrorsAndPanic :: [String] -> IO a
logErrorsAndPanic errs = do
    putStrLn ""
    mapM_ putStrLn errs
    error "I was made to understand there were sandwiches here."
