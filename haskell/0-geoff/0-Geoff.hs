import Control.Arrow

z1 :: Num b => [a] -> [(a,b)]
z1 as = zip (as++as) (cycle [0,1])
{-
z1 ["a","b","c"]
sort (z1 ["a","b","c"])
-}
z2 :: Num b => [a] -> [(a,b)]
z2  = flip zip (cycle [0,1]) . uncurry (++) . (id &&& id)
{-
z2 ["a","b","c"]
-}

------------
cataL :: (a -> b -> b) -> b -> [a] -> b
cataL f b (a : as) = f a (cataL f b as)
cataL _ b      []  = b

zygoL :: (a -> b -> b)      -- f
      -> (a -> b -> c -> c) -- g depends on f result
      -> b -> c             -- zeroes
      -> [a]                -- input
      -> c                  -- result
zygoL f g b0 c0 =
  snd . cataL (\a (b, c) -> (f a b, g a b c))
              (b0, c0)

z3 :: (Eq b, Num b) => [a] -> [(a,b)]
z3 = zygoL (\_ b -> if b == 0 then 1 else 0)
           (\a b c -> (a,b) : c)
           0
           []
{-
z3 ["a","b","c"]
-}
------------

z4 :: (Foldable t, Num b) => t a -> [(a, b)]
z4  = concatMap (\a -> [(a,0),(a,1)])
{-
z4 ["a","b","c"]
-}
z5 :: (Foldable t, Num b) => t a -> [(a, b)]
z5 = foldr (\x acc -> (x,0):(x,1):acc) []
{-
z5 ["a","b","c"]
-}
z5' :: (Foldable t, Num b) => t a -> [(a, b)]
z5' = foldl (\acc x -> (x,0):(x,1):acc) []
{-
z5' ["a","b","c"]
-}
