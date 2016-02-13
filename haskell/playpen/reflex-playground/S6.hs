{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map

-- MODEL

type ID = Int

data Model = Model { counters :: [(ID, MModel)]
                   , nextID   :: ID }

initModel :: Model
initModel = Model { counters = []
                  , nextID   = 0 }

-- UPDATE

data Action = Insert
            | Remove ID
            | Modify ID AAction

update :: Action -> Model -> Model
update Insert model =
    let newCounter  = (nextID model, iInitCounter 0)
        newCounters = counters model ++ [ newCounter ]
    in model { counters = newCounters
             , nextID   = nextID model + 1 }
update (Remove id) model =
    model { counters = filter (\(counterID,_) -> counterID /= id) (counters model) }
update (Modify id counterAction) model =
    model { counters = map updateCounter (counters model) }
  where
    updateCounter (counterID, counterModel) =
        if counterID == id
        then (counterID, uUpdate counterAction counterModel)
        else (counterID, counterModel)

-- VIEW

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model =
    el "div" $ do
        (insert, _) <- el' "button" $ text "Add"
        counters    <- mapDyn (Map.fromList . counters) model
        counterEvs  <- listWithKey counters (\k m -> do
                                                  (modEv, remEv) <- viewWithRemoveButton m
                                                  return (fmap (k,) modEv, fmap (k,) remEv))
        modEvt      <- mapDyn (leftmost . map fst . Map.elems) counterEvs
        remEvt      <- mapDyn (leftmost . map snd . Map.elems) counterEvs
        return $ leftmost [ fmap (const Insert)   (domEvent Click insert)
                          , fmap (uncurry Modify) (switchPromptlyDyn modEvt)
                          , fmap (Remove . fst)   (switchPromptlyDyn remEvt) ]

main = mainWidget $ do
    rec changes <- view model
        model   <- foldDyn update initModel changes
    return ()

------------------------------------------------------------------------------

type MModel = Int

iInitCounter :: Int -> MModel
iInitCounter count = count

data AAction = Increment | Decrement

uUpdate :: AAction -> MModel -> MModel
uUpdate Increment model = model + 1
uUpdate Decrement model = model - 1

-- we do not return html, it's done by MonadWidget
vView :: MonadWidget t m => Dynamic t MModel -> m (Event t AAction)
vView model =
    el "div" $ do
        (decrement, _) <- el' "button" $ text "-"
        el "div" $ do
            t <- mapDyn show model
            dynText t
        (increment, _) <- el' "button" $ text "+"
        return $ leftmost [ fmap (const Decrement) (domEvent Click decrement)
                          , fmap (const Increment) (domEvent Click increment) ]

viewWithRemoveButton :: MonadWidget t m => Dynamic t MModel -> m (Event t AAction, Event t ())
viewWithRemoveButton model =
    el "div" $ do
        modEv <- vView model
        (remove, _) <- el' "button" $ text "D"
        return ( modEv
               , fmap (const ()) (domEvent Click remove) )
