{-
Created       : 2013 Dec 01 (Sun) 08:57:06 by carr.
Last Modified : 2013 Dec 01 (Sun) 09:45:26 by carr.

http://en.wikibooks.org/wiki/Haskell/Monad_transformers
-}

import Data.Char
import Data.Maybe

------------------------------------------------------------------------------
-- non-monadic

askPassword :: IO ()
askPassword = do
    putStrLn "Insert your new password:"
    maybe_value <- getPassword
    putStrLn $ if isJust maybe_value then "GOOD" else "BAD"

getPassword :: IO (Maybe String)
getPassword = do
    s <- getLine
    return $ if isValid s then Just s else Nothing

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

------------------------------------------------------------------------------
-- monadic
{-
MaybeT is a monad m,  wrapper around m (Maybe a)

newtype (Monad m) => MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

- MaybeT type constructor
- term/value constructor: also called MaybeT
- accessor function: runMaybeT

Monad transformers are monads themselves.

instance Monad m => Monad (MaybeT m) where
    return  = MaybeT . return . Just
    -- `Just` injects into `Maybe`
    -- generic `return` injects into `m`
    -- could have written: `return = MaybeT . return . return`

    -- The signature of (>>=), specialized to MaybeT m
    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    x >>= f =
        MaybeT $ do maybe_value <- runMaybeT x
                    case maybe_value of
                        Nothing    -> return Nothing
                        Just value -> runMaybeT $ f value
        -- `runMaybeT` accessor to unwrap x == `m (Maybe a)`
        -- <- extracts `Maybe a` from `m`
        -- `return Nothing` == `m Nothing`
        -- `f :: MaybeT m b` so  `runMaybeT` to give `m (Maybe a)` (i.e., same type as `Nothing` branch)
        -- Wrapped with MaybeT

Same structure as `Maybe` except for extra (un)wrapping:

    -- (>>=) for Maybe
    maybe_value >>= f =
                    case maybe_value of
                        Nothing    -> Nothing
                        Just value -> f value

Useful:

-- Maybe is instance of MonadPlus, so MaybeT should be too
instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ do maybe_value <- runMaybeT x
                            case maybe_value of
                                 Nothing    -> runMaybeT y
                                 Just _     -> return maybe_value

-- take funs from m monad to the MaybeT m monad
instance MonadTrans MaybeT where lift = MaybeT . (liftM Just)
-}


-- no need to check Nothing/Just - bind handles it
-- lift to get getLine/putStrLn into MaybeT IO
-- since MaybeT IO is MonadPlus instance, can use guard : does return mzero (i.e. IO Nothing) for bad

getValidPasswordM :: MaybeT IO String
getValidPasswordM = do
    s <- lift getLine
    guard (isValid s) -- MonadPlus provides guard.
    return s

askPasswordM :: MaybeT IO ()
askPasswordM = do
    lift $ putStrLn "Insert your new password:"
    value <- getValidPassword
    lift $ putStrLn "Storing in database..."

-- use of MonadPlus.msum
askPasswordM' :: MaybeT IO ()
askPasswordM' = do
    lift $ putStrLn "Insert your new password:"
    value <- msum $ repeat getValidPassword
    lift $ putStrLn "Storing in database..."

{-
base monad  : the non-transformer monad on which transformer is based
inner monad : other monad, on which the transformer is applied
-}

-- End of file.
