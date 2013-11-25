{-# LANGUAGE LambdaCase #-}

module Chapter3.ParamPoly where

data Client i = GovOrg { clientId :: i
                       , clientName :: String
                       }
              | Company { clientId :: i
                        , clientName :: String
                        , person :: Person
                        , duty :: String
                        }
              | Individual { clientId :: i
                           , person :: Person }
              deriving Show

data Person = Person { firstName :: String
                     , lastName  :: String
                     }
              deriving Show

swapTriple :: (t2, t, t1) -> (t, t1, t2)
swapTriple (x,y,z) = (y,z,x)

duplicate :: t -> (t, t)
duplicate x = (x,x)

nothing :: t -> Maybe t
nothing _ = Nothing

index :: Num t => [t1] -> [(t, t1)]
index []     = []
index [x]    = [(0,x)]
index (x:xs) = let indexed@((n,_):_) = index xs
               in  (n+1,x):indexed

maybeA :: [t] -> Char
maybeA [] = 'a'
maybeA _  = 'b'

-- LambdaCase

-- |
-- >>> sayHello ["Alejandro", "Harold", "Flavia"]
-- ["Hello, writer","Hello, me","Hello, Flavia"]

sayHello :: [String] -> [String]
sayHello names = map (\case "Alejandro" -> "Hello, writer"
                            "Harold"    -> "Hello, me"
                            name        -> "Hello, " ++ name) names
