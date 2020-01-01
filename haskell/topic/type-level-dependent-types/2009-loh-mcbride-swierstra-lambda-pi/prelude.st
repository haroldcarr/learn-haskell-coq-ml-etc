-- assume adds names to the context

assume (a :: *) (y :: a)
-- ((\x -> x) :: a -> a) y
let id = (\x -> x) :: a -> a
-- id y

assume (b :: *)
-- ((\x y -> x) :: (b -> b) -> a -> b -> b) (\x -> x) y
let const = (\x y -> x) :: (b -> b) -> a -> b -> b
-- const (\x -> x) y

let idB = (\x -> x) :: b -> b
-- const idB

