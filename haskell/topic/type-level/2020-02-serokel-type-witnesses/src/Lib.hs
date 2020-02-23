{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Lib where

import Data.List

{-
Type Witnesses in Haskell
Sandeep Chandrika
Friday, February 21st, 2020

runtime witness : value that holds type-level info associated with a polymorphic value

if there is a branch in code like

if (i == 1) then { -- block 1 -- } else { -- block 2 -- }

then can safely assume that i == 1 in block 1,  and i /= in block 2

core idea of type witness
- use witness to get the compiler infer attributes of a polymorphic type
  eg., what inferred type is, constraints, ...

Example app : users with different privileges
-}

data UserPrivilege = Member | Admin | Guest

-- To recover 'User' from 'SomeUser', use this type witness.
-- Use GADTs to link a runtime value to type-level info.
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege 'Member
  WitnessGuest  :: WitnessPrivilege 'Guest
  WitnessAdmin  :: WitnessPrivilege 'Admin

data User (up :: UserPrivilege) = User
  { userId        :: Integer
  , userName      :: String
  , userPrivilege :: WitnessPrivilege up  }
{-
make 'userPrivilege' carry type level info
- e.g., to type check attempting to pass Member to fun that requires Admin

Need to be able to read a user from database without specifying user's privilege.

To hide the type-level privilege, use a GADT wrapper
-}
data SomeUser where
  SomeUser :: forall a. User a -> SomeUser
{-
SomeUser type constructor does not have a type parameter.
Can wrap it around a 'User a' of any privilege.
-}
-- takes : user id (Integer)
-- reads corresponding user from database
-- Note: type level privilege is hidden in return value 'SomeUser'
readUser :: Integer -> IO SomeUser
readUser userId0 = pure $ case find ((== userId0) . (\(a, _, _) -> a)) dbRows of
  Just (id_, name_, type_) -> case type_ of
    "member" -> SomeUser (User id_ name_ WitnessMember)
    "guest"  -> SomeUser (User id_ name_ WitnessGuest)
    "admin"  -> SomeUser (User id_ name_ WitnessAdmin)
    x        -> error ("readUser " ++ x)
  Nothing -> error "User not found"
{-
To convert SomeUser to 'User a', pattern match on 'userPrivilege' witness.
-}
run :: IO ()
run  = do
  SomeUser user <- readUser 12

  putStrLn $ getUserName user -- works on any privilege

  case userPrivilege user of  -- bring type-level privilege in scope by matching
    WitnessAdmin -> deleteStuffAsAdmin user
    _            -> error "Need admin user"

-- does not care about user privilege
getUserName :: User up -> String
getUserName = userName

-- requires Admin privilege
deleteStuffAsAdmin :: User 'Admin -> IO ()
deleteStuffAsAdmin _ = pure ()

dbRows :: [(Integer, String, String)]
dbRows = [ (10, "John" , "member")
         , (11, "alice", "guest")
         , (12, "bob"  , "admin") ]
