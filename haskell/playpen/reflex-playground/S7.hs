{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}

import Control.Monad
import Reflex.Dom
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = mainWidget test

test :: MonadWidget t m => m ()
test = do
    let elements = Map.fromList $ zip [0::Int ..] ['a'..'z']
    rec rs      <- el "ul" $ listHoldWithKey elements removes $ \k v ->
                       el "li" $ button $ "Remove Node " ++ [v]
        removes <- liftM (switch . current) $
                       mapDyn (leftmost . Map.elems . Map.mapWithKey
                               (\k e -> fmap (const $ Map.singleton k Nothing) e)) rs
    return ()

testListWithKey :: forall t m. MonadWidget t m => m ()
testListWithKey = do
    let startingElements = Map.fromList $ zip [0::Int ..] ['a'..'z']
    rec mapOfEvents :: Dynamic t (Map Int (Event t Int)) <-
            el "ul" $ listWithKey elements $ \k v -> do
                btnClick <- el "li" $ button $ "Remove Node " ++ show k
                -- Each list element returns Event t Int (where Int is the key)
                return $ fmap (const k) btnClick
        -- Turn Map of events into list
        listOfEvents :: Dynamic t [Event t Int] <- mapDyn Map.elems mapOfEvents
        -- Merge list of events into single event.
        -- https://hackage.haskell.org/package/reflex-0.2/candidate/docs/Reflex-Class.html#v:leftmost
        dynamicMergedEvent :: Dynamic t (Event t Int) <- mapDyn leftmost listOfEvents
        -- This event has the Int key of whichever "Remove Node" button is clicked.
        -- https://hackage.haskell.org/package/reflex-0.2/candidate/docs/Reflex-Class.html#v:switch
        let mergedRemoveEvent :: Event t Int = switch (current dynamicMergedEvent)
        elements <- foldDyn (\r es -> Map.delete r es) startingElements mergedRemoveEvent
    return ()
