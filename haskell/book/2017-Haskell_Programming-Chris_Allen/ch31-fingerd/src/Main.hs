{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

{-
To create DB:

stack ghci --main-is ch31-fingerd:exe:fingerd

Prelude> createDatabase
=> User {userId = 1, ... noise ... }

Creates sqlite database with name : finger.db in same directory as where fingerd service runs.

sudo `stack exec which fingerd`
finger callen@localhost

-}
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
import           Network.Socket               as NS hiding (recv)
import           Network.Socket.ByteString    (recv, sendAll)
import           System.Environment           (getArgs)
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

-- | Connection to talk to DB.
-- Socket to talk to user.
-- Gets/retruns list of all users in DB.
returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
    rows <- query_ dbConn allUsers
    let usernames = map username rows
        newlineSeparated = T.concat $ intersperse "\n" usernames
    sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
    BS.concat ["Login:\t\t "  , e username , "\t\t\t\t" , "Name: "  , e realName, "\n",
               "Directory:\t" , e homeDir  ,  "\t\t\t"  , "Shell: " , e shell,    "\n"]
  where
    e = encodeUtf8

-- | Given a username.
-- Returns user info (or prints not found and return nothing).
returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
    maybeUser <- getUser dbConn (T.strip username) -- strip newline CR
    case maybeUser of
        Nothing -> do putStrLn ("Couldn't find matching user for username: " ++ show username)
                      return ()
        Just user -> sendAll soc (formatUser user)

-- | rceives up to 1024 bytes of data.
-- Either send list of all users or only a single user.
handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
      msg <- recv soc 1024
      case msg of
        "\r\n" -> returnUsers dbConn soc
        name   -> returnUser  dbConn soc (decodeUtf8 name)

-- | Simple to Debug echo server
handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery dbConn soc
    NS.close soc

service :: String -> IO ()
service portnum = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                             Nothing
                             (Just portnum)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    NS.bind sock (addrAddress serveraddr)
    listen sock 1
    -- only one connection open at a time
    conn <- open "finger.db"
    handleQueries conn sock
    SQLite.close conn
    NS.close sock

main :: IO ()
main = do
    args <- getArgs
    service $ case args of [] -> "79"; (x:_) -> x
