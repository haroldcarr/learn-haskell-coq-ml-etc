{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2014 Jul 30 (Wed) 09:07:30 by Harold Carr.

TODO:
- use Shelly to
  - start up fuseki
  - run test program
- CLI with flags instead of order (and to enable optional arguments)
  - optional: dbAddress; multiple users, groups, permissions in one CLI command
- factor RDF utilities
- use Lens
- threepenny-gui
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Interview where

import           Control.Monad                   (unless)
import           Data.RDF.Types                  (LValue (..), Node (..))
import           Data.String                     (IsString)
import           Data.String.Utils               (replace)
import           Data.Text                       as T (Text, pack)
import           Database.HSparql.Connection
import           Database.HSparql.QueryGenerator
import           System.Environment              (getArgs)

------------------------------------------------------------------------------
-- CLI

main :: IO ()
main = do
    initializeDB
    av <- getArgs
    act av

act :: [String] -> IO ()
act ("new-user"                 : uEmail : [])                       = newUserCLI              uEmail
act ("add-user-to-group"        : uEmail : gName      :[])           = addUserToGroupCLI       uEmail gName
act ("rm-user-from-group"       : uEmail : gName      :[])           = putStrLn $ "TODO: rmUserFromGroup "  ++ uEmail ++ " " ++ gName
act ("list-user-groups"         : uEmail : [])                       = putStrLn $ "TODO: list-user-groups " ++ uEmail
act ("delete-user"              : uEmail : [])                       = putStrLn $ "TODO: delete-user "      ++ uEmail
act ("list-users"               : [])                                = listUsersCLI

act ("new-group"                : gName  : [])                       = newGroupCLI             gName
act ("add-permission-to-group"  : gName  : permission : resource:[]) = addPermissionToGroupCLI gName  permission resource
act ("rm-permission-from-group" : gName  : permission : resource:[]) = putStrLn $ "TODO: rmPermissionFromGroup " ++ gName  ++ " " ++ permission ++ " " ++ resource
act ("list-group-permissions"   : gName  : [])                       = putStrLn $ "TODO: list-user-groups "      ++ gName
act ("delete-group"             : gName  : [])                       = putStrLn $ "TODO: delete-group "          ++ gName
act ("list-groups"              : [])                                = listGroupsCLI

act x                                                                = putStrLn $ "unknown: " ++ show x

newUserCLI :: String -> IO ()
newUserCLI uEmail = do
    r <- newUser uEmail
    print r

listUsersCLI :: IO ()
listUsersCLI = do
    r <- listUsers
    print r

newGroupCLI :: String -> IO ()
newGroupCLI gName = do
    r <- newGroup gName
    print r

addUserToGroupCLI :: String -> String -> IO ()
addUserToGroupCLI uEmail gName = do
    r <- addUserToGroup uEmail gName
    print r

addPermissionToGroupCLI :: String -> String -> String -> IO ()
addPermissionToGroupCLI gName permission resource = do
    r <- addPermissionToGroup gName permission resource
    print r

listGroupsCLI :: IO ()
listGroupsCLI = do
    r <- listGroups
    banner "group"
    case r of
        Just g  -> mapM_ format g
        Nothing -> putStrLn "no groups"
  where
    format (_:n:[]) = (\(Bound v) -> print $ getValue v) n

getValue :: Node -> Text
getValue (Data.RDF.Types.LNode (Data.RDF.Types.PlainL x)) = x

banner :: String -> IO ()
banner x = do
    putStrLn "--------------------------------------------------"
    putStrLn x

------------------------------------------------------------------------------
-- persistent operations
-- Note: if these were high-frequency/volume operations then would batch calls where possible

newUser :: String -> IO Bool
newUser uEmail = do
    update  uid isA          user
    updateL uid emailAddress uEmail
  where
    uid = ohcP $ emailAddressToId uEmail

listUsers :: IO (Maybe [[BindingValue]])
listUsers =
    queryVTT var isA user

newGroup :: String -> IO Bool
newGroup gName = do
    update  uid isA       group
    updateL uid groupName gName
  where
    uid = ohcP gName

addUserToGroup :: String -> String -> IO Bool
addUserToGroup uEmail gName =
    update uid memberOf (ohcP gName)
  where
    uid = ohcP $ emailAddressToId uEmail

addPermissionToGroup :: String -> String -> String -> IO Bool
addPermissionToGroup gName permission resource =
    updateL uid perm resource
  where
    uid  = ohcP gName
    perm = pToP permission

listGroups :: IO (Maybe [[BindingValue]])
listGroups =
    queryVTTVTV isA group groupName

------------------------------------------------------------------------------
-- DB initialization

isDBAlreadyPopulated :: IO Bool
isDBAlreadyPopulated = sendAsk a
  where
    a :: Query AskQuery
    a = do
        _   <- openHcOrgPrefix
        i   <- initialized
        ask <- askTriple i i i
        return AskQuery { queryAsk = [ask] }

initializeDB :: IO ()
initializeDB = do
    dbAlreadyPopulated <- isDBAlreadyPopulated
    unless dbAlreadyPopulated initializeDB'
  where
    haskellCurryEmail = "haskell.curry@projectdelta.com"
    mathGroup         = "mathGroup"
    mathEmail         = "math@projectdelta.com"
    scheduling        = "https://scheduling.office.projectdelta.com/"
    initializeDB'     = do
        -- make Haskell Curry a user and a member of the math group
        newUser haskellCurryEmail
        newGroup mathGroup
        addUserToGroup haskellCurryEmail mathGroup
        -- math group
        --     can read and send emails to the math@projectdelta.com mailing list
        addPermissionToGroup mathGroup readP  mathEmail
        addPermissionToGroup mathGroup writeP mathEmail
        --     can reserve conference rooms via https://scheduling.office.projectdelta.com/
        addPermissionToGroup mathGroup readP  scheduling
        addPermissionToGroup mathGroup writeP scheduling
        --     has read-only access to financial reports at https://reports.finance.projectdelta.com/
        addPermissionToGroup mathGroup readP  "https://reports.finance.projectdelta.com/"

        -- set the initialized marker
        update initialized initialized initialized
        return ()

------------------------------------------------------------------------------
-- rdf utilities

-- Note: cannot do top-level types because `TermLike` is not exported.

-- update :: (TermLike a, TermLike b, TermLike c) => Query a -> Query b -> Query c -> IO Bool
update s p o = sendUpdate u
  where
    u = do
        _  <- openHcOrgPrefix
        s' <- s;    p' <- p;    o' <- o;
        u'  <- updateTriple s' p' o'
        return UpdateQuery { queryUpdate = [u'] }

-- updateL :: (TermLike a, TermLike b) => Query a -> Query b -> [Char] -> IO Bool
updateL s p o = sendUpdate u
  where
    u = do
        _  <- openHcOrgPrefix
        s' <- s;    p' <- p
        u'  <- updateTriple s' p' (T.pack o)
        return UpdateQuery { queryUpdate = [u'] }

-- queryVTT :: (TermLike b, c) => Query Variable -> Query b -> Query c -> IO (Maybe [[BindingValue]])
queryVTT s p o = sendQuery q
  where
    q = do
        _  <- openHcOrgPrefix
        s' <- s;    p' <- p;    o' <- o;
        triple s' p' o'
        return SelectQuery { queryVars = [s'] }

-- queryVTTVTV :: (TermLike a, TermLike b, TermLike c) => Query a -> Query b -> Query c -> IO (Maybe [[BindingValue]])
queryVTTVTV p1 o1 p2 = sendQuery q
  where
    q = do
        _   <- openHcOrgPrefix
        uid <- var;    p1' <- p1;    o1' <- o1
        p2' <- p2;     o2' <- var
        triple uid p1' o1'
        triple uid p2' o2'
        return SelectQuery { queryVars = [uid,o2'] }

sendUpdate :: Query UpdateQuery -> IO Bool
sendUpdate = updateQuery dbUpdateAddress

sendQuery  :: Query SelectQuery -> IO (Maybe [[BindingValue]])
sendQuery  = selectQuery dbQueryAddress

sendAsk    :: Query AskQuery    -> IO Bool
sendAsk    = askQuery    dbQueryAddress

------------------------------------------------------------------------------
-- misc utilities

emailAddressToId :: (Eq a, IsString [a]) => [a] -> [a]
emailAddressToId = replace "@" "AT"

-- pToP :: String -> Query IRIRef
pToP x | x == readP  = readPermission
       | x == writeP = writePermission
       | otherwise   = ohcP x -- TODO: restrict

------------------------------------------------------------------------------
-- constants

dbAddress, dbQueryAddress, dbUpdateAddress, readP, writeP :: String
dbAddress         = "http://localhost:3030/ds/"
dbQueryAddress    = dbAddress ++ "query"
dbUpdateAddress   = dbAddress ++ "update"
readP             = "read"
writeP            = "write"

-- IRIRef not exported
-- ohcP :: Query IRIRef
ohcP x = do { ohc <- openHcOrgPrefix ; return (ohc .:. T.pack x) }
emailAddress      = ohcP "emailAddress"
group             = ohcP "group"
groupName         = ohcP "groupName"
hasPermissions    = ohcP "hasPermissions"
initialized       = ohcP "initialized"
isA               = ohcP "isA"
memberOf          = ohcP "memberOf"
readPermission    = ohcP "readPermission"
user              = ohcP "user"
writePermission   = ohcP "writePermission"

-- openHcOrgPrefix :: Query Prefix
openHcOrgPrefix   = prefix "openHcOrg" (iriRef "http://openhc.org/")

------------------------------------------------------------------------------
-- Experiments

qAll :: IO (Maybe [[BindingValue]])
qAll = sendQuery q
  where
    q = do
        s <- var; p <- var; o <- var
        triple s p o
        return SelectQuery { queryVars = [s, p, o] }

qBook1 :: IO (Maybe [[BindingValue]])
qBook1 = sendQuery q
  where
    q = do
        dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
        ex <- prefix "ex" (iriRef "http://example/")
        x  <- var
        triple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")
        return SelectQuery { queryVars = [x] }

aBook1 :: IO Bool
aBook1 = sendAsk a
  where
    a = do
        dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
        ex <- prefix "ex" (iriRef "http://example/")
        ask <- askTriple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")
        return AskQuery { queryAsk = [ask] }

-- PREFIX dc: <http://purl.org/dc/elements/1.1/> INSERT DATA {  <http://example/book1> dc:title "A new book" ; dc:creator "A.N.Other" . }
uhb :: IO Bool
uhb = sendUpdate u
  where
    u = do
        dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
        ex <- prefix "ex" (iriRef "http://example/")
        ut1 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "A HASKELL book")
        ut2 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "Another HASKELL book")
        ut3 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "Yet another HASKELL book")
        return UpdateQuery { queryUpdate = [ut1,ut2,ut3] }

-- End of file.
