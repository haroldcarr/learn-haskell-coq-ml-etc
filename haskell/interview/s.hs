{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2014 Jan 11 (Sat) 09:53:53 by Harold Carr.

TODO:
- Use Shelly to start up fuseki and then program that runs through it using below
-}

import Control.Monad (unless)
import Data.String.Utils (replace)
import Data.Text as T (pack, unpack)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import System.Environment (getArgs)

dbAddress         = "http://localhost:3030/ds/"
dbQueryAddress    = dbAddress ++ "query"
dbUpdateAddress   = dbAddress ++ "update"
emailAddress      = mkX "emailAddress"
group             = mkX "group"
groupName         = mkX "groupName"
hasPermissions    = mkX "hasPermissions"
initialized       = mkX "initialized"
isA               = mkX "isA"
memberOf          = mkX "memberOf"
openHcOrgPrefix   = prefix "openHcOrg" (iriRef "http://openhc.org/")
readPermission    = mkX "readPermission"
user              = mkX "user"
writePermission   = mkX "writePermission"

mkX x = do { ohc <- openHcOrgPrefix ; return (ohc .:. x) }

main :: IO ()
main = do
    initializeDB
    av <- getArgs
    act av

act ("new-user"                : uEmail : [])                       = newUser uEmail
act ("add-user-to-group"       : uEmail : gName      :[])           = addUserToGroup uEmail gName
act ("list-user-groups"        : uEmail : [])                       = do putStrLn "list-user-groups"
act ("delete-user"             : uEmail : [])                       = do putStrLn "delete-user"
act ("list-users"              : [])                                = do putStrLn "list-users"

act ("new-group"               : gName  : [])                       = newGroup gName
act ("add-permission-to-group" : gName  : permission : resource:[]) = addPermissionToGroup gName permission resource
act ("list-group-permissions"  : gName  : [])                       = do putStrLn "list-user-groups"
act ("delete-group"            : gName  : [])                       = do putStrLn "delete-group"
act ("list-groups"             : [])                                = do putStrLn "list-users"

act x                                                               = do putStrLn $ "unknown: " ++ (show x)

emailAddressToId uEmail =
    replace "@" "AT" uEmail

newUser uEmail = update u
  where
    u = do
        ohc <- openHcOrgPrefix
        id  <- mkX $ T.pack (emailAddressToId uEmail);    ia  <- isA;    u   <- user
        u1  <- updateTriple id ia u
        ema <- emailAddress
        u2  <- updateTriple id ema (T.pack uEmail)
        return UpdateQuery { queryUpdate = [u1,u2] }

newGroup gName = update u
  where
    gnp = (T.pack gName)
    u = do
        ohc <- openHcOrgPrefix
        id  <- mkX (T.pack gName);    ia  <- isA;    grp <- group
        u1  <- updateTriple id ia grp
        gn  <- groupName
        u2  <- updateTriple id gn gnp
        return UpdateQuery { queryUpdate = [u1,u2] }

addUserToGroup uEmail gName = update u
  where
    u = do
        ohc <- openHcOrgPrefix
        id  <- mkX (T.pack $ emailAddressToId uEmail);    mo  <- memberOf;    mg  <- mkX (T.pack gName)
        u1  <- updateTriple id mo mg
        return UpdateQuery { queryUpdate = [u1] }

addPermissionToGroup gName permission resource = do return () {- update u
  where
    u = do
        ohc <- openHcOrgPrefix
-}

isDBAlreadyPopulated :: IO Bool
isDBAlreadyPopulated = da aq
  where
    aq :: Query AskQuery
    aq = do
        ohc <- openHcOrgPrefix
        i   <- initialized
        ask <- askTriple i i i
        return AskQuery { queryAsk = [ask] }

    da :: Query AskQuery -> IO Bool
    da aq = do { r <- askQuery dbQueryAddress aq; return r }

initializeDB :: IO ()
initializeDB = do
    dbAlreadyPopulated <- isDBAlreadyPopulated
    unless dbAlreadyPopulated initializeDB'
  where
    haskellCurryEmail = "haskell.curry@projectdelta.com"
    mathGroup         = "mathGroup"
    initializeDB' = do
        -- make Haskell Curry a user and a member of the math group
        newUser haskellCurryEmail
        newGroup mathGroup
        addUserToGroup haskellCurryEmail mathGroup
        update u
    u = do
        ohc <- openHcOrgPrefix

        -- math group
        let mathEmail  = T.pack "math@projectdelta.com"
            scheduling = T.pack "https://scheduling.office.projectdelta.com/"
        --     can read and send emails to the math@projectdelta.com mailing list
        mg  <- mkX (T.pack mathGroup)
        rp  <- readPermission
        wp  <- writePermission
        u5  <- updateTriple mg rp mathEmail
        u6  <- updateTriple mg wp mathEmail
        --     can reserve conference rooms via https://scheduling.office.projectdelta.com/
        u7  <- updateTriple mg rp scheduling
        u8  <- updateTriple mg wp scheduling
        --     has read-only access to financial reports at https://reports.finance.projectdelta.com/
        u9  <- updateTriple mg rp (T.pack "https://reports.finance.projectdelta.com/")

        -- set the initialized marker
        i   <- initialized
--        u10 <- updateTriple i i i

        return UpdateQuery { queryUpdate = [u5,u6,u7,u8,u9{-,u10-}] }

update x = do
    r <- updateQuery dbUpdateAddress x
    putStrLn $ "result: " ++ show (r::Bool)

------------------------------------------------------------------------------
-- Experiments

q :: Query SelectQuery
q = do
    s <- var
    p <- var
    o <- var
    triple s p o
    return SelectQuery { queryVars = [s, p, o] }

q' :: Query SelectQuery
q' = do
    dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
    ex <- prefix "ex" (iriRef "http://example/")
    x  <- var

    triple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")

    return SelectQuery { queryVars = [x] }

a :: Query AskQuery
a = do
    dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
    ex <- prefix "ex" (iriRef "http://example/")

    ask <- askTriple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")

    return AskQuery { queryAsk = [ask] }

-- PREFIX dc: <http://purl.org/dc/elements/1.1/> INSERT DATA {  <http://example/book1> dc:title "A new book" ; dc:creator "A.N.Other" . }
u :: Query UpdateQuery
u = do
    dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
    ex <- prefix "ex" (iriRef "http://example/")

    ut1 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "A HASKELL book")
    ut2 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "Another HASKELL book")
    ut3 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "Yet another HASKELL book")

    return UpdateQuery { queryUpdate = [ut1,ut2,ut3] }


dq :: Query SelectQuery -> IO ()
dq query = do
    result <- selectQuery "http://localhost:3030/ds/query" query
    case result of
        (Just s) -> putStrLn . show $ s
        Nothing  -> error "bad"

da :: IO ()
da = do
    res <- askQuery "http://localhost:3030/ds/query" a
    putStrLn $ "result: " ++ show (res::Bool)

du :: IO ()
du = do
    res <- updateQuery "http://localhost:3030/ds/update" u
    putStrLn $ "result: " ++ show (res::Bool)

-- End of file.
