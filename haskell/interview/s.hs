{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2013 Dec 24 (Tue) 22:32:34 by carr.
-}

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

q :: Query SelectQuery
q = do
    s <- var
    p <- var
    o <- var
    triple s p o
    return SelectQuery { queryVars = [s, p, o] }

dq :: IO ()
dq = do
    result <- selectQuery "http://localhost:3030/ds/query" q
    case result of
        (Just s) -> putStrLn . show $ s
        Nothing  -> error "bad"

-- End of file.
