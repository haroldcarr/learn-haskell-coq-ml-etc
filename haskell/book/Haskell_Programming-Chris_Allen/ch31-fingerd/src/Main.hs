{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Exception
import           Control.Monad                (forever)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.List                    (intersperse)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Data.Typeable
import           Database.SQLite.Simple       hiding (close)
import qualified Database.SQLite.Simple       as SQLite
import           Database.SQLite.Simple.Types
import           Network.Socket               hiding (close, recv)
import           Network.Socket.ByteString    (recv, sendAll)
import           Text.RawString.QQ

{-
DB: use sqlite-simple to store data in a file in same directory as project.
    repository of users
-}

data User = User {
      userId        :: Integer -- primary key
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
      toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

-- | exception if get more than one user for a particular username
-- should be impossible
data DuplicateData = DuplicateData
                   deriving (Eq, Show, Typeable)
instance Exception DuplicateData

-- | tuples inserted to create a new user
type UserRow = (Null, Text, Text, Text, Text, Text)

-- | `Only` used to pass a single argument instead of a 2-or-greater tuple to query parameters.
-- Needed because base has no one-tuple type and getUserQuery takes a single parameter.
getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only username)
    case results of
        []     -> return $ Nothing
        [user] -> return $ Just user
        _      -> throwIO DuplicateData

-- | Running a second time will error without changing the database.
-- To reset the database, delete the finger.db file.
createDatabase :: IO ()
createDatabase = do
    conn <- open "finger.db"
    execute_ conn createUsers
    execute conn insertUser meRow
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
  where
    meRow :: UserRow
    meRow = (Null, "callen", "/bin/zsh",
             "/home/callen", "Chris Allen",
             "555-123-4567")

main :: IO ()
main = do
  putStrLn "hello world"
