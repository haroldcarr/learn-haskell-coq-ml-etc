module X_2011_12_31_functors
where
{-
Created       : 2014 Oct 07 (Tue) 08:53:35 by Harold Carr.
Last Modified : 2015 Sep 01 (Tue) 14:44:47 by Harold Carr.

2011-12-31
http://www.haskellforall.com/2011/12/haskell-for-mainstream-programmers-code.html
FUNCTORS

category theory is the mathematician's version of code reuse
- identifies patterns common to every branch of mathematics
- those same patterns are also common in programming languages as well

Avoid null pointer check via Maybe functor.

fmap (*2) (Just 2)
-- Just 4
fmap (*2) Nothing
-- Nothing

Errors (i.e., Nothing) now propagated automatically.

Collection functors:

instance Functor [] where
    fmap :: (a -> b) -> [a] -> [b]
instance Functor Set where
    fmap :: (a -> b) -> Set a -> Set b
instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
instance Functor (Map k) where
    fmap :: (a -> b) -> Map k a -> Map k b

State functors:

-- actually a newtype and I've flipped the output
type State s a = s -> (s, a)
type IO a = State RealWorld a -- actually a newtype

instance Functor (State s) where
    fmap :: (a -> b) -> State a -> State b
    fmap f x s = second f (x s) -- a pure function
instance Functor IO where
    fmap :: (a -> b) -> IO a -> IO b
    fmap f x s = second f (x s) -- no different

getLine      :: IO String  -- i.e. (RealWorld -> (RealWorld, String))
parseInteger :: String -> Int
getInteger   :: IO Integer -- i.e. (RealWorld -> (RealWorld, Int))
getInteger    = fmap parseInteger getLine
-}
