{- Author:     Jeff Newbern
   Maintainer: Jeff Newbern <jnewbern@nomaware.com>
   Time-stamp: <Mon Nov 10 11:58:21 2003>
   License:    GPL

EXERCISES
Created       : 2015 Aug 15 (Sat) 11:51:48 by Harold Carr.
Last Modified : 2015 Aug 15 (Sat) 12:31:21 by Harold Carr.
-}

import           Control.Monad      (MonadPlus (..))
import           Control.Monad.Plus (mfromMaybe)
import           Data.Maybe         (maybeToList)

{- DESCRIPTION

Example 2 - Do notation

Usage: Compile the code and execute the resulting program.
       It will print Dolly's maternal grandfather.
-}

-- everything you need to know about sheep
data Sheep = Sheep {name::String, mother::Maybe Sheep, father::Maybe Sheep}

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- the Maybe type is already declared as an instance of the Monad class
-- in the standard prelude, so we don't actually need to define it here.
-- just remember that it looks something like this:
-- instance Monad Maybe where
--    Nothing  >>= f = Nothing
--    (Just x) >>= f = f x
--    return         = Just

-- we can use do-notation to build complicated sequences
maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s = do m <- mother s
                           father m

{-
5.1 Exercise 1: Do notation

Rewrite maternalGrandfather, fathersMaternalGrandmother, and mothersPaternalGrandfather
using return and >>= (do not use do)
-}

maternalGrandfatherNoDo :: Sheep -> Maybe Sheep
maternalGrandfatherNoDo s = mother s >>= father

fathersMaternalGrandmother :: Sheep -> Maybe Sheep
fathersMaternalGrandmother s = do f  <- father s
                                  gm <- mother f
				  mother gm

fathersMaternalGrandmotherNoDo :: Sheep -> Maybe Sheep
fathersMaternalGrandmotherNoDo s = father s >>= mother >>= mother

mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
				  father gf

mothersPaternalGrandfatherNoDo :: Sheep -> Maybe Sheep
mothersPaternalGrandfatherNoDo s = mother s >>= father >>= father

-- this builds our sheep family tree
breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing Nothing
		 uranus = Sheep "Uranus" Nothing Nothing
		 gaea   = Sheep "Gaea" Nothing Nothing
		 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
	         roger  = Sheep "Roger" (Just eve) (Just kronos)
	         molly  = Sheep "Molly" (Just holly) (Just roger)
	     in Sheep "Dolly" (Just molly) Nothing

{-
5.2 Exercise 2: Combining monadic values

Write parent, grandparent :: Sheep -> Maybe Sheep
They should return one sheep selected from all sheep matching the description, or Nothing.
Use 'mplus'.
-}

parent,grandparent :: Sheep -> Maybe Sheep
parent s = mother s `mplus` father s
grandparent s = (mother s >>= parent) `mplus` (father s >>= parent)

{-
5.3 Exercise 3: Using the List monad

Write parent, grandparent :: Sheep -> [Sheep]
They should return all sheep matching the description, or the empty list.
Use 'mplus'
Use Maybe.maybeToList.
-}

parentL,grandparentL :: Sheep -> [Sheep]
parentL s = maybeToList (mother s) `mplus` maybeToList (father s)
grandparentL s = (maybeToList (mother s) >>= parentL) `mplus`
                 (maybeToList (father s) >>= parentL)

{-
5.4 Exercise 4: Using the Monad class constraint

Monads & modularity
- encapsulate computational strategies into single blocks of code
- vary the monad in which a computation is done
  - via polymorphic functions using (Monad m) =>, (MonadPlus m) =>, class constraints.

Write parent, grandparent :: (MonadPlus m) => Sheep -> m Sheep.
- should work with Maybe and List monads.
-}

parentMP,grandparentMP :: (MonadPlus m) => Sheep -> m Sheep
parentMP s = mfromMaybe (mother s) `mplus` mfromMaybe (father s)
grandparentMP s = (mfromMaybe (mother s) >>= parentMP) `mplus`
                  (mfromMaybe (father s) >>= parentMP)

main :: IO ()
main = do
    let dolly = breedSheep
    print (maternalGrandfather dolly)
    print (maternalGrandfatherNoDo dolly)
    print (fathersMaternalGrandmother dolly)
    print (fathersMaternalGrandmotherNoDo dolly)
    print (mothersPaternalGrandfather dolly)
    print (mothersPaternalGrandfatherNoDo dolly)
    print [mother dolly, father dolly]
    print (parent dolly)
    print (parentL dolly)
    print ((parentMP dolly) >>= show)
    print [mother dolly >>= mother
          ,father dolly >>= mother
          ,mother dolly >>= father
          ,father dolly >>= father]
    print (grandparent dolly)
    print (grandparentL dolly)
    print ((grandparentMP dolly) >>= show)

-- END OF FILE

