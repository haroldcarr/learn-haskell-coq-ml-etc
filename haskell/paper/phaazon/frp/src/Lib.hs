{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Arrow
import           Control.Category
import           Control.Lens
import           Data.Function    (fix)
import           Data.Semigroup
import           Linear.V3
import           Prelude          hiding (id, (.))

------------------------------------------------------------------------------
-- POLLING VERSION

data Input = W | S | A | D | R | F | Quit deriving (Eq, Read, Show)

--               one or more simultaneous events
pollEvents :: IO [Input]
pollEvents = fmap treatLine getLine
  where
    treatLine = concatMap (map fst . reads) . words

newtype Camera = Camera { _cameraPosition :: V3 Float } deriving (Eq, Read, Show)

makeLenses ''Camera

-- state that will change on an event
data AppSt = AppSt { _appCamera :: Camera } deriving (Eq, Read, Show)

makeLenses ''AppSt

updateAppSt :: AppSt -> Input -> Maybe AppSt
updateAppSt appSt input = case input of
    W    -> Just $ appSt & appCamera . cameraPosition . _z -~ 0.1
    S    -> Just $ appSt & appCamera . cameraPosition . _z +~ 0.1
    A    -> Just $ appSt & appCamera . cameraPosition . _x -~ 0.1
    D    -> Just $ appSt & appCamera . cameraPosition . _x +~ 0.1
    R    -> Just $ appSt & appCamera . cameraPosition . _y -~ 0.1
    F    -> Just $ appSt & appCamera . cameraPosition . _y +~ 0.1
    Quit -> Nothing

polling :: IO ()
polling = looping AppSt { _appCamera = Camera { _cameraPosition = V3 0.0 0.0 0.0 } }
  where
    looping appSt = do
        input <- pollEvents
        print input
        let newAppSt = updateAppSt appSt (head input) -- only use the "first" of simultaneous events
        case newAppSt of
            (Just as@(AppSt (Camera cp))) -> do { print cp; looping as }
            Nothing -> return ()

------------------------------------------------------------------------------
-- FRP VERSION

-- event-driven programming : consume/transform events over time

-- behavior : reactive relationship : value of `a` that reacts to time `t` with input `b`
newtype Behavior t a b = Behavior { stepBehavior :: t -> a -> (b, Behavior t a b)}

-- event    : values of type `a` that have occurrences in time `t`
newtype Event t a = Event { getEvent :: (t,a) } deriving (Functor)
{-
instance Functor (Event t) where
    fmap f (Event e) = Event (fmap f e)
-}
-- switching : act of changing (i.e., "switching") behavior : better name : `until`
--        initial         event that yields new behavior
switch :: Behavior t a b -> Event t (Behavior t a b) -> Behavior t a b
switch = undefined

-- stepping : act of passing `t` to `Behavior t a` to get value `a` (sometimes called `reactimation`)
-- behaviors connected to each other form a "reactive network"
{-
camera :: Behavior t [Input] Camera
camera = Behavior (const treatEvents)
  where
    treatEvents events
        | W `elem` events = Camera $ V3 0 0 (-0.1)
        | S `elem` events = Camera $ V3 0 0 0.1
        | otherwise       = Camera $ V3 0 0 0

idleing :: Behavior t ([Input],Camera) Camera
idleing = Behavior (const snd)

inputToBehavior :: Input -> Behavior t (t, Camera) Camera
inputToBehavior i = case i of
    W -> Behavior $ \_ (_,cam) -> cam & cameraPosition . _z -~ 0.1
    S -> Behavior $ \_ (_,cam) -> cam & cameraPosition . _z +~ 0.1
    A -> Behavior $ \_ (_,cam) -> cam & cameraPosition . _x -~ 0.1
    D -> Behavior $ \_ (_,cam) -> cam & cameraPosition . _x +~ 0.1
    F -> Behavior $ \_ (_,cam) -> cam & cameraPosition . _y -~ 0.1
    R -> Behavior $ \_ (_,cam) -> cam & cameraPosition . _y +~ 0.1
    _ -> Behavior $ \_ (_,cam) -> cam
-}
newtype Auto a b = Auto { runAuto :: a -> (b, Auto a b) }

instance Arrow (Behavior t) where
    -- arr :: (Arrow a) => (b -> c) -> a b c
    -- used to lift function into arrowized version
    --                              t
    arr f = fix $ \r -> Behavior $ \_ a -> (f a,r)

    -- first :: (Arrow a) => a b c -> a (b,d) (c,d)
    -- takes an arrow which takes a value as input
    -- returns an arrow that takes a pair as input, applying given function on first value of pair
    first f = Behavior $ \t (b,d) ->
        let (c,fn) = stepBehavior f t b
        in ((c,d),first fn)

instance Category (Behavior t) where
    id = arr id
    x . y = Behavior $ \t a ->
        let (yr,yn) = stepBehavior y t a
            (xr,xn) = stepBehavior x t yr
        in (xr,xn . yn)

instance (Semigroup b) => Semigroup (Behavior t a b) where
    x <> y = Behavior $ \t a ->
        let (xr,xn) = stepBehavior x t a
            (yr,yn) = stepBehavior y t a
        in (xr <> yr,xn <> yn)

instance Functor (Behavior t a) where
    fmap f b = Behavior $ \t a ->
        let (br,bn) = stepBehavior b t a
        in (f br,fmap f bn)

instance Applicative (Behavior t a) where
    pure = arr . const
    f <*> x = Behavior $ \t a ->
        let (fr,fn) = stepBehavior f t a
            (xr,xn) = stepBehavior x t a
        in (fr xr,fn <*> xn)

instance Profunctor (Behavior t) where
    dimap l r x = Behavior $ \t a ->
        let (xr,xn) = stepBehavior x t (l a)
        in (r xr,dimap l r xn)
