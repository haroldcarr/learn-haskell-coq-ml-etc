{-
Created       : 2014 Oct 09 (Thu) 13:17:04 by Harold Carr.
Last Modified : 2014 Oct 09 (Thu) 14:09:57 by Harold Carr.
-}

data ListS a b = NilS | ConsS a b deriving Show
data Fix   s a = In { out :: s a (Fix s a) }
type List  a   = Fix ListS a

x :: List Int
x = In (ConsS 1 (In (ConsS 2 (In (ConsS 3 (In NilS))))))

hd :: List a -> a -> a
hd (In       NilS ) a = a
hd (In (ConsS a _)) _ = a

tl :: List a -> List a
tl (In (ConsS _ t)) = t
tl x                = x

bimap :: (a -> a') -> (b -> b') -> ListS a b -> ListS a' b'
bimap f g       NilS  = NilS
bimap f g (ConsS a b) = ConsS (f a) (g b)

-- CRITICAL: out is a function
foldList :: (ListS a b -> b) -> List a -> b
foldList f = f . bimap id (foldList f) . out

add :: ListS Int Int -> Int
add NilS = 0
add (ConsS m n) = m + n

y :: Int
y = foldList add x

-- 25'

