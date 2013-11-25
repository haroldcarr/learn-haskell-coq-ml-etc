{-# LANGUAGE NamedFieldPuns, RecordWildCards, ViewPatterns #-}

module Chapter2.SimpleFunctions where

import Data.Char (toUpper)

-- |
--
-- >>> firstOrEmpty []
-- "empty"
--
-- >>> firstOrEmpty ["hello", "world"]
-- "hello"

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty l = if not (null l) then head l else "empty"

(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = if null l1 {- check emptyness -}
            then l2 -- base case
            else (head l1) : (tail l1 +++ l2)

-- |
-- >>> reverse2 []
-- []
--
-- >>> reverse2 [1,2,3]
-- [3,2,1]

reverse2 :: [a] -> [a]
reverse2 l = if null l then []
             else reverse2 (tail l) +++ [head l]

-- |
-- >>> maxmin [1,2,3,4,5]
-- (5,1)

maxmin :: Ord a => [a] -> (a, a)
maxmin list =
    if null (tail list)
    then (head list, head list)
    else ( if (head list) > fst (maxmin (tail list))
           then head list
           else fst (maxmin (tail list))
         , 
           if (head list) < snd (maxmin (tail list))
           then head list
           else snd (maxmin (tail list))
         )

data Client = GovOrg String
            | Company String Integer Person String
            | Individual Person Bool
            deriving Show

data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving Show

-- |
-- >>> clientName (GovOrg "NASA")
-- "NASA"
--
-- >>> clientName (Company "ACME" 12 (Person "Some" "One" Female) "Director")
-- "ACME"
--
-- >>> clientName (Individual (Person "H" "C" Male) True)
-- "H C"

clientName :: Client -> String
clientName client =
    case client of
        GovOrg name -> name
        Company name _ _ _ -> name
        Individual person _ ->
            case person of Person fName lName _ -> fName ++ " " ++ lName

-- |
-- >>> responsibility (Company "ACME" 12 (Person "Some" "One" Female) "Director")
-- "Director"

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _ = "Unknown"

-- |
-- >>> specialClient (Company "ACME" 12 (Person "Some" "One" Female) "Director")
-- True

-- ViewPatterns
specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director") = True
specialClient _ = False

-- |
-- >>> IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } }
-- IndividualR {person = PersonR {firstName = "John", lastName = "Smith"}}
--
-- >>> GovOrgR "NATO"
-- GovOrgR {clientRName = "NATO"}
--
-- >>> clientRName (GovOrgR "NATO")
-- "NATO"

data ClientR = GovOrgR     { clientRName :: String }
             | CompanyR    { clientRName :: String
                           , companyId   :: Integer
                           , person      :: PersonR
                           , duty        :: String
                           }
             | IndividualR { person      :: PersonR }
             deriving Show

data PersonR = PersonR     { firstName   :: String
                           , lastName    :: String
                           }
             deriving Show

-- |
-- >>> greet (IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } })
-- "Hi, John"

greet :: ClientR -> String
greet GovOrgR { } = "Welcome"
greet CompanyR { clientRName = c } = "Hello, " ++ c
greet IndividualR { person = PersonR { firstName = fn } } = "Hi, " ++ fn

-- NamedFieldPuns

-- |
-- >>> greet' (IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } })
-- "Hi, John"

greet' :: ClientR -> String
greet' GovOrgR { } = "Welcome"
greet' CompanyR { clientRName } = "Hello, " ++ clientRName
greet' IndividualR { person = PersonR { firstName } } = "Hi, " ++ firstName

-- RecordWildCards

-- |
-- >>> greet'' (IndividualR { person = PersonR { lastName = "Smith", firstName = "John" } })
-- "Hi, John"

greet'' :: ClientR -> String
greet'' GovOrgR { } = "Welcome"
greet'' CompanyR { .. } = "Hello, " ++ clientRName
greet'' IndividualR { person = PersonR { .. } } = "Hi, " ++ firstName

-- |
-- >>> nameInCapitals (PersonR { lastName = "smith", firstName = "john" })
-- PersonR {firstName = "John", lastName = "smith"}

nameInCapitals :: PersonR -> PersonR
nameInCapitals p@(PersonR { firstName = initial:rest }) =
    let newName = (toUpper initial):rest
    in p { firstName = newName }
nameInCapitals p@(PersonR { firstName = "" }) = p
