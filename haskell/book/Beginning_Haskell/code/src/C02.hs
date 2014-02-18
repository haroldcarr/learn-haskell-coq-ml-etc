{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module C02 where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

-- ADT - p. 31

            --           name
data Client = GovOrg     String
            --           name   id      contact duty
            | Company    String Integer Person  String
            | Individual Person Bool
            deriving (Eq, Ord, Show)

            --       first  last
data Person = Person String String Gender
            deriving (Eq, Ord, Show)

data ClientR = GovOrgR  { clientRName :: String }
             | CompanyR { clientRName :: String
                        , companyId   :: Integer
                        , person      :: PersonR
                        , duty        :: String }
             | IndividualR { person :: PersonR
                           , huh    :: Bool }
             deriving Show

data PersonR = PersonR { firstName :: String
                       , lastName  :: String
                       , gender    :: Gender
                       } deriving Show

data Gender = Male | Female | Unknown deriving (Eq, Ord, Show)

-- pattern matching - p. 33

clientName :: Client -> String
clientName (GovOrg name)                         = name
clientName (Company name _ _ _)                  = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

clientNameR :: ClientR -> String
clientNameR x@(GovOrgR {})                       = clientRName x
clientNameR x@(CompanyR {})                      = clientRName x
clientNameR (IndividualR { person = p})          = firstName p ++ " " ++ lastName p

clientGender :: Client -> Maybe Gender
clientGender (Individual     (Person _ _ gender) _) = Just gender
clientGender (Company    _ _ (Person _ _ gender) _) = Just gender
clientGender _                                      = Nothing

-- exercise 2-5 - p. 36

numClientsOfGender :: [Client] -> (Integer, Integer, Integer)
numClientsOfGender = foldr (\x (m,f,u) -> case clientGender x of
                                              Nothing      -> (m,   f,   u)
                                              Just Male    -> (m+1, f,   u)
                                              Just Female  -> (m,   f+1, u)
                                              Just Unknown -> (m,   f,   u+1))
                           (0,0,0)

clients :: [Client]
clients =
    [ Individual (Person "Harold" "Carr"         Male)    True
    , Company    "Acme" 3 (Person "First" "Last" Unknown) "Director"
    , Individual (Person "Flavia" "Cervino-Wood" Female)  True
    , GovOrg     "NSA"
    , Individual (Person "Suni"   "Coke"         Unknown) True
    , Individual (Person "Mr."    "Alejandro"    Male)    True
    ]

clientsR :: [ClientR]
clientsR =
    [ IndividualR (PersonR "Harold" "Carr"         Male)    True
    , IndividualR (PersonR "Flavia" "Cervino-Wood" Female)  True
    , IndividualR (PersonR "Suni"   "Coke"         Unknown) True
    , IndividualR (PersonR "Mr."    "Alejandro"    Male)    True
    , CompanyR    "Acme" 3 (PersonR "First" "Last" Unknown) "Director"
    , GovOrgR     "NSA"
    ]

-- numClientsOfGender clients

data Travel = Past | Future | PastAndFuture deriving Show

--                             manufacturer  model  name    travel  price
data TimeMachine = TimeMachine String        Int    String  Travel  Float deriving Show

data TimeMachineR = TimeMachineR { manufacturer :: String
                                 , model        :: Int
                                 , name         :: String
                                 , travel       :: Travel
                                 , price        :: Float
                                 } deriving Show

discountMachines :: Float -> [TimeMachine] -> [TimeMachine]
discountMachines adjust = foldr (\(TimeMachine man md nam tra pri) a -> (TimeMachine man md nam tra (pri*adjust)):a) []

timeMachines :: [TimeMachine]
timeMachines =
    [ TimeMachine "Foo" 1 "Fooey" Past           99.99
    , TimeMachine "Bar" 2 "Barry" Future        199.99
    , TimeMachine "Baz" 3 "Bazzy" PastAndFuture 299.99
    ]

-- discountMachines 0.90 timeMachines

-- exercise 2-6 - p. 40

ack :: (Num a1, Num a, Ord a1, Ord a) => a1 -> a -> a
ack m n
    | m  > 0 && n  > 0 = ack (m - 1) (ack m (n - 1))
    | m  > 0 && n == 0 = ack (m -1) 1
    | m == 0           = n + 1
ack _ _                = error "bad args to ack"

unzip' :: [(a, a1)] -> ([a], [a1])
unzip' = foldr (\(x,y) (a,b) -> (x:a, y:b)) ([],[])

-- unzip' [(1,2),(3,4)]

-- view patterns - p. 40

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName     -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")      = True
specialClient _                                   = False

-- specialClient (clients !! 0)
-- specialClient (clients !! 3)
-- specialClient (clients !! 4)

-- records - p. 41

greet :: ClientR -> [Char]
greet IndividualR { person = PersonR { .. } } = "Hi " ++ firstName
greet CompanyR    { .. }                      = "Hello " ++ clientRName
greet GovOrgR     { }                         = "Welcome"

-- map greet clients

-- default values idiom - p. 43

-- p. 59 : "smart constructors": only export type and default - no way to create others
-- module Chapter2.DataTypes (ConnOptions(), connDefault) where

data ConnType = TCP | UDP
data UseProxy = NoProxy | Proxy String
data TimeOut = NoTimeOut | TimeOut Integer
data Connection = Connection String ConnOptions

data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UseProxy
                               , connCaching   :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut   :: TimeOut
                               }
connect :: String -> ConnOptions -> Connection
connect url options = Connection url options

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut

-- connect "http://apress.com" connDefault
-- connect "http://apress.com" connDefault { connType = UDP }

-- End of file.
