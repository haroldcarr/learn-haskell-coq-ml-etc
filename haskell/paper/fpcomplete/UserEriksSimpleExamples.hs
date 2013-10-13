{-
Created       : 2013 Oct 08 (Tue) 10:35:40 by carr.
Last Modified : 2013 Oct 08 (Tue) 10:39:26 by carr.
-}

import Data.IORef

c = do
    r <- newIORef []
    return r

u ref x = do
    value <- readIORef ref
    writeIORef ref $ x:value

u' ref f = do
    value <- readIORef ref
    writeIORef ref $ f value

p ref = do
    readIORef ref >>= print

main = do
    ref <- c
    u ref 1
    u ref 2
    u ref 3
    u' ref (\ (h:t) -> h*10:t)
    p ref

-- End of file.
