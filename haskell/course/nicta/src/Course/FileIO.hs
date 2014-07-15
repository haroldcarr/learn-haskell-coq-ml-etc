{-
Created       : 2014 Jul 15 (Tue) 05:41:40 by Harold Carr.
Last Modified : 2014 Jul 15 (Tue) 08:09:38 by Harold Carr.
-}

{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FileIO where

import           Course.Applicative
import           Course.Apply
import           Course.Bind
import           Course.Core
import           Course.Functor
import           Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell io.hs "files.txt"

HC:
$ cd src
$ runhaskell Course/FileIO.hs
$ runhaskell Course/FileIO.hs "one"
$ runhaskell Course/FileIO.hs "../share" "files.txt"
$ runhaskell Course/FileIO.hs "one" "two" "three"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
-- HC: modified to supply directory where files are location and filename of initial file
main ::
  IO ()
main =
  getArgs >>= \args ->
  case args of
      (d:.f:.Nil) -> run d f
      _           -> putStrLn "two and only two args must be given"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  FilePath
  -> FilePath
  -> IO ()
run dir top =
  readFile (dir ++ "/" ++ top) >>= \contents ->
  getFiles dir (lines contents) >>= printFiles

{-
[fp1,fp2,fp3]
getFile <$> [fp1,fp2,fp3] => [IO (fp1,c1), IO (fp2,c2), IO (fp3,c3)]
??                        => IO [(fp1,c1),    (fp2,c2),    (fp3,c3)]
-}

getFiles ::
  FilePath
  -> List FilePath
  -> IO (List (FilePath, Chars))
getFiles dir = mapM (getFile dir)

mapM :: Applicative m => (a -> m b) -> List a -> m (List b)
mapM f Nil     = pure Nil
mapM f (x:.xs) = (:.) <$> f x <*> mapM f xs

mapM' f (x:.xs) = f x        >>= \fx ->
                  mapM' f xs >>= \fxs ->
                  pure (fx :. fxs)
-- mapM f (x:.xs) = do fx  <- f x
--                     fxs <- mapM f xs
--                     pure (fx :. fxs)

getFile ::
  FilePath
  -> FilePath
  -> IO (FilePath, Chars)
getFile dir f = readFile (dir ++ "/" ++ f) >>= \c ->
                pure (f, c)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles xs = mapM (uncurry printFile) xs >>= \_ ->
                pure ()

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile fp c =
  putStrLn fp >>= \_ ->
  putStrLn c  >>= \_ ->
  pure ()

-- End of file.
