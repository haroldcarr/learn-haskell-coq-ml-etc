{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}

module GoNoGo where

------------------------------------------------------------------------------
import           MissionControl
import           ScratchPad.Astro
import           ScratchPad.Weather
------------------------------------------------------------------------------
import           Data.Kind
------------------------------------------------------------------------------

data GoNoGo :: RocketState -> WeatherState -> AstronautState -> Type where
  GO   :: GoNoGo 'NoProblems 'CorrectWeather 'OnBoard

