module Isomorphisms where

data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
             deriving (Enum, Eq, Ord, Show)

data Seven   = One    | Two    | Three   | Four      | Five     | Six    | Seven
             deriving (Enum, Eq, Ord, Show)

transform :: (Enum a, Ord a, Enum b, Ord b) => a -> b
transform = toEnum . fromEnum

i0 :: Seven
i0 = transform Tuesday

i1 :: Weekday
i1 = transform Three


transform2 :: (Enum a, Ord a) => Int -> a
transform2 = toEnum . (`rem` 7)

i2 :: Weekday
i2 = transform2 15

i3 :: Seven
i3 = transform2 15


