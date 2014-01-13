{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2014 Jan 12 (Sun) 16:03:43 by Harold Carr.

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
emailAddress      = ohcP "emailAddress"
group             = ohcP "group"
groupName         = ohcP "groupName"
hasPermissions    = ohcP "hasPermissions"
initialized       = ohcP "initialized"
isA               = ohcP "isA"
memberOf          = ohcP "memberOf"
openHcOrgPrefix   = prefix "openHcOrg" (iriRef "http://openhc.org/")
readP             = "read"
readPermission    = ohcP "readPermission"
user              = ohcP "user"
writeP            = "write"
writePermission   = ohcP "writePermission"

ohcP x = do { ohc <- openHcOrgPrefix ; return (ohc .:. (T.pack x)) }

main :: IO ()
main = do
    initializeDB
    av <- getArgs
    act av

act ("new-user"                 : uEmail : [])                       = newUserCLI              uEmail
act ("add-user-to-group"        : uEmail : gName      :[])           = addUserToGroupCLI       uEmail gName
act ("rm-user-from-group"       : uEmail : gName      :[])           = do putStrLn "rmUserFromGroup      uEmail gName"
act ("list-user-groups"         : uEmail : [])                       = do putStrLn "list-user-groups"
act ("delete-user"              : uEmail : [])                       = do putStrLn "delete-user"
act ("list-users"               : [])                                = listUsersCLI

act ("new-group"                : gName  : [])                       = newGroupCLI             gName
act ("add-permission-to-group"  : gName  : permission : resource:[]) = addPermissionToGroupCLI gName  permission resource
act ("rm-permission-from-group" : gName  : permission : resource:[]) = do putStrLn "rmPermissionFromGroup gName  permission resource"
act ("list-group-permissions"   : gName  : [])                       = do putStrLn "list-user-groups"
act ("delete-group"             : gName  : [])                       = do putStrLn "delete-group"
act ("list-groups"              : [])                                = listGroupsCLI

act x                                                                = do putStrLn $ "unknown: " ++ (show x)

newUserCLI uEmail = do
    r <- newUser uEmail
    putStrLn $ show r

newUser uEmail = update u
  where
    u = do
        ohc <- openHcOrgPrefix
        id  <- ohcP $ emailAddressToId uEmail;    ia  <- isA;    u   <- user
        u1  <- updateTriple id ia u
        ema <- emailAddress
        u2  <- updateTriple id ema (T.pack uEmail)
        return UpdateQuery { queryUpdate = [u1,u2] }

listUsersCLI = do
    r <- listUsers
    putStrLn $ show r

listUsers = do query q
  where
    q = do
        s <- var;    ia  <- isA;    u   <- user
        triple s ia u
        return SelectQuery { queryVars = [s] }

newGroupCLI gName = do
    r <- newGroup gName
    putStrLn $ show r

newGroup gName = update u
  where
    gnp = (T.pack gName)
    u = do
        ohc <- openHcOrgPrefix
        id  <- ohcP gName;    ia  <- isA;    grp <- group
        u1  <- updateTriple id ia grp
        gn  <- groupName
        u2  <- updateTriple id gn gnp
        return UpdateQuery { queryUpdate = [u1,u2] }

addUserToGroupCLI uEmail gName = do
    r <- addUserToGroup uEmail gName
    putStrLn $ show r

addUserToGroup uEmail gName = update u
  where
    u = do
        ohc <- openHcOrgPrefix
        id  <- ohcP $ emailAddressToId uEmail;    mo  <- memberOf;    g  <- ohcP gName
        u1  <- updateTriple id mo g
        return UpdateQuery { queryUpdate = [u1] }

addPermissionToGroupCLI gName permission resource = do
    r <- addPermissionToGroup gName permission resource
    putStrLn $ show r

addPermissionToGroup gName permission resource = update u
  where
    perm = pToP permission
    u = do
        ohc <- openHcOrgPrefix
        id  <- ohcP gName
        p   <- perm
        u1  <- updateTriple id p (T.pack resource)
        return UpdateQuery { queryUpdate = [u1] }

listGroupsCLI = do
    r <- listGroups
    putStrLn $ show r

listGroups = do query q
  where
    q = do
        s <- var;    ia  <- isA;    g   <- group
        triple s ia g
        return SelectQuery { queryVars = [s] }

isDBAlreadyPopulated :: IO Bool
isDBAlreadyPopulated = ask aq
  where
    aq :: Query AskQuery
    aq = do
        ohc <- openHcOrgPrefix
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
        update u
        return ()
    u = do
        ohc <- openHcOrgPrefix
        -- set the initialized marker
        i   <- initialized
        u1  <- updateTriple i i i
        return UpdateQuery { queryUpdate = [u1] }

mkDoUpdate s o p = do update u
  where
    u = do
        ohc <- openHcOrgPrefix
        s' <- s;    o' <- o;    p' <- p
        u  <- updateTriple s' o' p'
        return UpdateQuery { queryUpdate = [u] }

update :: Query UpdateQuery -> IO Bool
update u = do r <- updateQuery dbUpdateAddress u; return r

query :: Query SelectQuery -> IO (Maybe [[BindingValue]])
query  q = do r <- selectQuery dbQueryAddress  q; return r

ask :: Query AskQuery -> IO Bool
ask    a = do r <- askQuery    dbQueryAddress  a; return r

emailAddressToId uEmail = replace "@" "AT" uEmail

pToP x | x == readP  = readPermission
       | x == writeP = writePermission
       | otherwise   = ohcP x -- TODO: restrict

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


-- End of file.
