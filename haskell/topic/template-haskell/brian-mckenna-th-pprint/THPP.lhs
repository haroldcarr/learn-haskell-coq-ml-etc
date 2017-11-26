Brian Mckenna
https://asciinema.org/a/149569

> {-# LANGUAGE TemplateHaskell #-}
>
> module THPP where
>
> import Language.Haskell.TH.Cleanup
> import Data.Functor.Deriving
> import Data.Eq.Deriving

> data X a = X a
> data B = T | F

:set -XTemplateHaskell
let df = $(simplifiedTH =<< deriveFunctor ''X)
let de = $(simplifiedTH =<< deriveEq      ''B)
putStrLn df
putStrLn de
