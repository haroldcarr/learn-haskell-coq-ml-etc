{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2014 Jan 12 (Sun) 19:03:44 by Harold Carr.

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

import Control.Monad (unless)
import Data.RDF.Types (Node(..), LValue(..))
import Data.String.Utils (replace)
import Data.Text as T (pack, unpack)
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import System.Environment (getArgs)

------------------------------------------------------------------------------
-- CLI

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

listUsersCLI = do
    r <- listUsers
    putStrLn $ show r

newGroupCLI gName = do
    r <- newGroup gName
    putStrLn $ show r

addUserToGroupCLI uEmail gName = do
    r <- addUserToGroup uEmail gName
    putStrLn $ show r

addPermissionToGroupCLI gName permission resource = do
    r <- addPermissionToGroup gName permission resource
    putStrLn $ show r

listGroupsCLI = do
    r <- listGroups
    banner "group"
    case r of
        Just g  -> mapM_ format g
        Nothing -> putStrLn "no groups"
  where
    format (_:n:[]) = do
        (\(Bound v) -> putStrLn $ show $ getValue $ v) n

getValue (Data.RDF.Types.LNode (Data.RDF.Types.PlainL x)) = x

banner x = do
    putStrLn "--------------------------------------------------"
    putStrLn x

------------------------------------------------------------------------------
-- persistent operations
-- Note: if these were high-frequency/volume operations then would batch calls where possible

newUser uEmail = do
    update  id isA          user
    updateL id emailAddress uEmail
  where
    id = ohcP $ emailAddressToId uEmail

listUsers = do
    queryVTT var isA user

newGroup gName = do
    update  id isA       group
    updateL id groupName gName
  where
    id = ohcP gName

addUserToGroup uEmail gName = do
    update id memberOf (ohcP gName)
  where
    id = ohcP $ emailAddressToId uEmail

addPermissionToGroup gName permission resource =
    updateL id perm resource
  where
    id   = ohcP gName
    perm = pToP permission

listGroups = do
    queryVTTVTV isA group groupName

------------------------------------------------------------------------------
-- DB initialization

isDBAlreadyPopulated :: IO Bool
isDBAlreadyPopulated = sendAsk a
  where
    a :: Query AskQuery
    a = do
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

        -- set the initialized marker
        update initialized initialized initialized
        return ()

------------------------------------------------------------------------------
-- rdf utilities

update s p o = do sendUpdate u
  where
    u = do
        ohc <- openHcOrgPrefix
        s' <- s;    p' <- p;    o' <- o;
        u  <- updateTriple s' p' o'
        return UpdateQuery { queryUpdate = [u] }

updateL s p o = do sendUpdate u
  where
    u = do
        ohc <- openHcOrgPrefix
        s' <- s;    p' <- p
        u  <- updateTriple s' p' (T.pack o)
        return UpdateQuery { queryUpdate = [u] }

queryVTT s p o = do sendQuery q
  where
    q = do
        ohc <- openHcOrgPrefix
        s' <- s;    p' <- p;    o' <- o;
        triple s' p' o'
        return SelectQuery { queryVars = [s'] }

queryVTTVTV p1 o1 p2 = do sendQuery q
  where
    q = do
        ohc <- openHcOrgPrefix
        id  <- var;    p1' <- p1;    o1' <- o1
        p2' <- p2;     o2' <- var
        triple id p1' o1'
        triple id p2' o2'
        return SelectQuery { queryVars = [id,o2'] }

sendUpdate :: Query UpdateQuery -> IO Bool
sendUpdate u = do r <- updateQuery dbUpdateAddress u; return r

sendQuery  :: Query SelectQuery -> IO (Maybe [[BindingValue]])
sendQuery  q = do r <- selectQuery dbQueryAddress  q; return r

sendAsk    :: Query AskQuery    -> IO Bool
sendAsk    a = do r <- askQuery    dbQueryAddress  a; return r

------------------------------------------------------------------------------
-- misc utilities

emailAddressToId uEmail = replace "@" "AT" uEmail

pToP x | x == readP  = readPermission
       | x == writeP = writePermission
       | otherwise   = ohcP x -- TODO: restrict

------------------------------------------------------------------------------
-- constants

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

------------------------------------------------------------------------------
-- Experiments

qAll = do sendQuery q
  where
    q = do
        s <- var; p <- var; o <- var
        triple s p o
        return SelectQuery { queryVars = [s, p, o] }

qBook1 = do sendQuery q
  where
    q = do
        dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
        ex <- prefix "ex" (iriRef "http://example/")
        x  <- var
        triple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")
        return SelectQuery { queryVars = [x] }

aBook1 = do sendAsk a
  where
    a = do
        dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
        ex <- prefix "ex" (iriRef "http://example/")
        ask <- askTriple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")
        return AskQuery { queryAsk = [ask] }

-- PREFIX dc: <http://purl.org/dc/elements/1.1/> INSERT DATA {  <http://example/book1> dc:title "A new book" ; dc:creator "A.N.Other" . }
uhb = do sendUpdate u
  where
    u = do
        dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
        ex <- prefix "ex" (iriRef "http://example/")
        ut1 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "A HASKELL book")
        ut2 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "Another HASKELL book")
        ut3 <- updateTriple (ex .:. "book1")  (dc .:. "title") (T.pack "Yet another HASKELL book")
        return UpdateQuery { queryUpdate = [ut1,ut2,ut3] }

-- End of file.
