{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2014 Jan 10 (Fri) 23:34:46 by Harold Carr.

TODO:
- Use Shelly to start up fuseki and then program that runs through it using below
-}

import Control.Monad (unless)
import Data.List.Split (splitOn)
import Data.Text as T (pack, unpack)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import System.Environment (getArgs)

dbAddress         = "http://localhost:3030/ds/"
dbQueryAddress    = dbAddress ++ "query"
dbUpdateAddress   = dbAddress ++ "update"
emailAddress      = mkX "emailAddress"
group             = mkX "group"
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

act ("new-user"          : userEmail : [])           = newUser userEmail
act ("add-user-to-group" : userEmail : groupName:[]) = do putStrLn "add-user-to-group"
act ("list-user-groups"  : userEmail : [])           = do putStrLn "list-user-groups"
act ("delete-user"       : userEmail : [])           = do putStrLn "delete-user"
act ("list-users"        : [])                       = do putStrLn "list-users"

act ("new-group"              : group : [])                       = do putStrLn "new-group"
act ("new-group-permission"   : group : permission : resource:[]) = do putStrLn "new-group-permission"
act ("list-group-permissions" : group : [])                       = do putStrLn "list-user-groups"
act ("delete-group"           : group : [])                       = do putStrLn "delete-group"
act ("list-groups"            : [])                               = do putStrLn "list-users"

act x  = do putStrLn $ "unknown: " ++ (show x)

newUser userEmail = update u
  where
    u = do
        ohc <- openHcOrgPrefix
        let (name:_:[]) = splitOn ("@"::String) userEmail
            root  = mkX (T.pack name)
        r   <- root
        ia  <- isA
        u   <- user
        u1  <- updateTriple r ia u
        ema <- emailAddress
        u2  <- updateTriple r ema (T.pack userEmail)
        return UpdateQuery { queryUpdate = [u1,u2] }

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
    haskellCurryName = "haskell.curry"
    initializeDB' = (newUser $ haskellCurryName ++ "@projectdelta.com") `seq` update u -- TODO: RIGHT HERE
    u = do
        ohc <- openHcOrgPrefix
        -- make Haskell Curry a user and a member of the math group
        let mathGroup = mkX "mathGroup"
        hc  <- mkX (T.pack haskellCurryName)
        mo  <- memberOf
        mg  <- mathGroup
        u3  <- updateTriple hc mo mg

        -- make math a group
        ia  <- isA
        grp <- group
        u4  <- updateTriple mg ia grp
        -- math group
        let mathEmail  = (T.pack "math@projectdelta.com")
            scheduling = (T.pack "https://scheduling.office.projectdelta.com/")
        --     can read and send emails to the math@projectdelta.com mailing list
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

        return UpdateQuery { queryUpdate = [u3,u4,u5,u6,u7,u8,u9{-,u10-}] }

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
