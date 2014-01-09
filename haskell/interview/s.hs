{-
Created       : 2013 Dec 23 (Mon) 23:15:11 by carr.
Last Modified : 2014 Jan 03 (Fri) 21:43:11 by Harold Carr.
-}

module SS
where

import Data.Text as T
import Database.HSparql.Connection
import Database.HSparql.QueryGenerator

q :: Query SelectQuery
q = do
    s <- var
    p <- var
    o <- var
    triple s p o
    return SelectQuery { queryVars = [s, p, o] }

-- PREFIX dc: <http://purl.org/dc/elements/1.1/> INSERT DATA {  <http://example/book1> dc:title "A new book" ; dc:creator "A.N.Other" . }
q' :: Query SelectQuery
q' = do
    dc <- prefix "dc" (iriRef "http://purl.org/dc/elements/1.1/")
    ex <- prefix "ex" (iriRef "http://example/")
    x  <- var

    triple (ex .:. "book1")  (dc .:. "title") (T.pack "A new book")

    return SelectQuery { queryVars = [x] }

dq :: Query SelectQuery -> IO ()
dq query = do
    result <- selectQuery "http://localhost:3030/ds/query" query
    case result of
        (Just s) -> putStrLn . show $ s
        Nothing  -> error "bad"

-- End of file.
