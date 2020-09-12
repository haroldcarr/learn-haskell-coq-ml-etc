import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

-- Can only run predicate on files, not directories.
-- Traverse ALL directories, can't skip any.
-- Strict (because in IO) : traverse everything first, then predicate.
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)
