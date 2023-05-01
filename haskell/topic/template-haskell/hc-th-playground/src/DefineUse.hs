{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell          #-}

module DefineUse where

import   Support

hCos, hSin :: Double -> Double
hCos = $(importDoubleToDouble "cos")
hSin = $(importDoubleToDouble "sin")

