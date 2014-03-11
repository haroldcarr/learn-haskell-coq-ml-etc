-- mkOptional then generates a new data type with all Names in that module with an added suffix _opt. Here is an example of its use:

{-# LANGUAGE TemplateHaskell #-}

module THU where

import           TH

data Foo = Foo { a,b,c,d,e :: Double, f :: Int }

mapM mkOptional [''Foo]

$(fooey)

-- Generates something like
-- data Foo_opt = Foo_opt {a_opt :: Maybe Double, .....  f_opt :: Maybe Int}

-- End of file.
