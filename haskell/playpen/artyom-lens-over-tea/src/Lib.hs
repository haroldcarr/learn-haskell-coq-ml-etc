{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens

newtype Being = Being
  { _age :: Double }

data Person = Person
  { _personBeing :: Being
  , _name :: String }

data Worker = Worker
  { _workerPerson :: Person
  , _job :: String  }

makeClassy ''Being
makeClassy ''Person
makeClassy ''Worker

instance HasBeing Person where being = personBeing
instance HasPerson Worker where person = workerPerson

instance HasBeing Worker where being = person.being

------------------------------------------------------------------------------

------------------------------------------------------------------------------

someFunc :: IO ()
someFunc = do
  let b = Being 23
      p = Person (Being 24) "HC"
      w = Worker (Person (Being 25) "FCW") "Acadmic"
  foo b p w

foo :: (HasBeing b, HasBeing p, HasBeing w)
    => b -> p -> w
    -> IO ()
foo b p w = print (b^.age, p^.age, w^.age)

