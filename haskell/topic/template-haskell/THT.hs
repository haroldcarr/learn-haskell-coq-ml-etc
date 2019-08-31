{-
Created       : 2014 Mar 06 (Thu) 17:12:50 by Harold Carr.
Last Modified : 2014 Mar 15 (Sat) 22:46:54 by Harold Carr.

http://www.haskell.org/haskellwiki/Template_Haskell#Generating_records_which_are_variations_of_existing_records
-}

-- mkOptional then generates a new data type with all Names in that module with an added suffix _opt. Here is an example of its use:

{-# LANGUAGE TemplateHaskell #-}

module THT where

import           TH

data Foo = Foo { a,b,c,d,e :: Double, f :: Int }

mapM mkOptional [''Foo]

$(fooey)

-- Generates something like
-- data Foo_opt = Foo_opt {a_opt :: Maybe Double, .....  f_opt :: Maybe Int}

-- End of file.
