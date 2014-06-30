{-
Created       : 2014 Feb                   by Harold Carr.
Last Modified : 2014 Jun 30 (Mon) 02:44:28 by Harold Carr.
-}

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module C02 where

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 2-1 - p 19

-- 2

nullOrFirstIsNull :: [[t]] -> Bool
nullOrFirstIsNull []     = True
nullOrFirstIsNull ([]:_) = True
nullOrFirstIsNull _      = False

-- 3

hasOneEl :: [t] -> Bool
hasOneEl (_:[]) = True
hasOneEl _      = False

-- 4

mcc :: [[t]] -> [t]
mcc []    = []
mcc (h:t) = h ++ mcc t

e21 :: T.Test
e21 = T.TestList
    [
      U.teq "e2120" (nullOrFirstIsNull [])      True
    , U.teq "e2121" (nullOrFirstIsNull [[],[]]) True
    , U.teq "e2130" (hasOneEl [1::Int])         True
    , U.teq "e2131" (hasOneEl [1,2::Int])       False
    , U.teq "e2132" (hasOneEl [])               False
    , U.teq "e2140" (mcc []::[Int])             []
    , U.teq "e2140" (mcc [[1::Int]])            [1]
    , U.teq "e2140" (mcc [[1],[2],[3]])         [1,2,3::Int]
    ]

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- Exercise 2-5 - p. 36

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

data Travel = Past | Future | PastAndFuture deriving (Eq, Show)

--                             manufacturer  model  name    travel  price
data TimeMachine = TimeMachine String        Int    String  Travel  Float deriving (Eq, Show)

data TimeMachineR = TimeMachineR { manufacturer :: String
                                 , model        :: Int
                                 , name         :: String
                                 , travel       :: Travel
                                 , price        :: Float
                                 } deriving (Eq, Show)

discountMachines :: Float -> [TimeMachine] -> [TimeMachine]
discountMachines adjust = foldr (\(TimeMachine man md nam tra pri) a -> (TimeMachine man md nam tra (pri*adjust)):a) []

timeMachines :: [TimeMachine]
timeMachines =
    [ TimeMachine "Foo" 1 "Fooey" Past           99.99
    , TimeMachine "Bar" 2 "Barry" Future        199.99
    , TimeMachine "Baz" 3 "Bazzy" PastAndFuture 299.99
    ]

e25 :: T.Test
e25 = T.TestList
    [
      U.teq "e2510" (numClientsOfGender clients)         (2,1,2)
    , U.teq "e2520" (discountMachines 0.90 timeMachines)
                    [
                      TimeMachine "Foo" 1 "Fooey" Past           89.991
                    , TimeMachine "Bar" 2 "Barry" Future        179.991
                    , TimeMachine "Baz" 3 "Bazzy" PastAndFuture 269.991
                    ]
    ]

------------------------------------------------------------------------------
-- exercise 2-6 - p. 40

ack :: (Num a, Ord a) => a -> a -> a
ack m n
    | m  > 0 && n  > 0 = ack (m - 1) (ack m (n - 1))
    | m  > 0 && n == 0 = ack (m - 1) 1
    | m == 0           = n + 1
ack _ _                = error "bad args to ack"

unzip' :: [(a, b)] -> ([a], [b])
unzip' = foldr (\(x,y) (a,b) -> (x:a, y:b)) ([],[])

e26 :: T.Test
e26 = T.TestList
    [
      U.teq "e2610" (ack (0::Integer) (1::Integer))   2
    , U.teq "e2611" (ack (1::Integer) (0::Integer))   2
    , U.teq "e2612" (ack (1::Integer) (1::Integer))   3 -- TODO check this result
    , U.teq "e2620" (unzip' [(1::Int,2),(3,4::Int)])  ([1,3],[2,4])
    ]

------------------------------------------------------------------------------
-- view patterns - p. 40

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName     -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")      = True
specialClient _                                   = False

vp :: T.Test
vp = T.TestList
    [
      U.teq "vp0" (specialClient (clients !! 0)) False
    , U.teq "vp1" (specialClient (clients !! 5)) True
    ]

------------------------------------------------------------------------------
-- records - p. 41

greet :: ClientR -> String
greet IndividualR { person = PersonR { .. } } = "Hi " ++ firstName
greet CompanyR    { .. }                      = "Hello " ++ clientRName
greet GovOrgR     { }                         = "Welcome"

rc :: T.Test
rc = T.TestList
    [
      U.teq "rc0" (map greet clientsR)
                  [
                    "Hi Harold"
                  , "Hi Flavia"
                  , "Hi Suni"
                  , "Hi Mr."
                  , "Hello Acme"
                  , "Welcome"
                  ]
    ]

------------------------------------------------------------------------------
-- default values idiom - p. 43

-- p. 59 : "smart constructors": only export type and default - no way to create others
-- module Chapter2.DataTypes (ConnOptions(), connDefault) where

data ConnType = TCP | UDP deriving (Eq, Show)
data UseProxy = NoProxy | Proxy String deriving (Eq, Show)
data TimeOut = NoTimeOut | TimeOut Integer deriving (Eq, Show)
data Connection = Connection String ConnOptions deriving (Eq, Show)

data ConnOptions = ConnOptions { connType      :: ConnType
                               , connSpeed     :: Integer
                               , connProxy     :: UseProxy
                               , connCaching   :: Bool
                               , connKeepAlive :: Bool
                               , connTimeOut   :: TimeOut
                               }
                   deriving (Eq, Show)

connect :: String -> ConnOptions -> Connection
connect url options = Connection url options

connDefault :: ConnOptions
connDefault = ConnOptions TCP 0 NoProxy False False NoTimeOut

dv :: T.Test
dv = T.TestList
    [
      U.teq "dv0" (connect "http://apress.com" connDefault)
                  (Connection "http://apress.com"
                   (ConnOptions { connType = TCP
                                , connSpeed = 0
                                , connProxy = NoProxy
                                , connCaching = False
                                , connKeepAlive = False
                                , connTimeOut = NoTimeOut}))
    , U.teq "dv1" (connect "http://apress.com" connDefault { connType = UDP })
                  (Connection "http://apress.com"
                   (ConnOptions { connType = UDP
                                , connSpeed = 0
                                , connProxy = NoProxy
                                , connCaching = False
                                , connKeepAlive = False
                                , connTimeOut = NoTimeOut}))
    ]

------------------------------------------------------------------------------

c02 :: IO T.Counts
c02 = do
    _ <- T.runTestTT e21
    _ <- T.runTestTT e25
    _ <- T.runTestTT e26
    _ <- T.runTestTT vp
    _ <- T.runTestTT rc
    T.runTestTT dv

-- End of file.
