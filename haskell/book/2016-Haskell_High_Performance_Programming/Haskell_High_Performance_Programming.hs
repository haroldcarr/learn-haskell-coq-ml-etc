let xs = [1 .. 5] :: [Int]
:print xs
:sprint xs
xs !! 2
:p xs
:sp xs
-- => xs = 1 : 2 : 3 : _

-- Normal Form (NF): fully evaluated
-- Weak Head Normal Form (WHNF): evaluate up to first data constructor
let t = const (Just "a") () :: Maybe String
:sp t
t  `seq` ()
:sp t
-- => t = Just _


