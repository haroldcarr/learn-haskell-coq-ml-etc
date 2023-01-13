{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module FM_DL_free_cofree where

import           Control.Monad.Free       (Free, liftF)
import           Control.Monad.State
import qualified Control.Monad.Trans.Free as TF hiding (Free, Pure)

------------------------------------------------------------------------------
-- http://dlaing.org/cofun/posts/free_and_cofree.html

data AdderF k = Add Int (Bool -> k)
              | Clear            k
              | Total   (Int  -> k)
              deriving (Functor)

-- rather than makeFree ''AdderF
-- generalize in order to work with other effects (the 'm' below)

type Adder    a = Free  AdderF   a
type AdderT m a = TF.FreeT AdderF m a

add :: Monad m => Int -> AdderT m Bool
add x = liftF $ Add x id

clear :: Monad m => AdderT m ()
clear = liftF $ Clear ()

total :: Monad m => AdderT m Int
total = liftF $ Total id

-- findLimit :: Adder Int
-- more general:
findLimit :: Monad m => AdderT m Int
findLimit = do
    -- capture the old count
    t <- total
    -- clear the count
    clear
    -- seek out the limit
    r <- execStateT findLimit' 0
    -- restore the old count
    clear
    _ <- add t
    -- return the result
    return r

findLimit' :: Monad m => StateT Int (TF.FreeT AdderF m) ()
findLimit' = do
    -- add 1 to the total
    r <- lift $ add 1
    -- check for overflow
    when r $ do
        -- if no overflow, add to our state counter ...
        modify (+ 1)
        -- and continue
        findLimit'

