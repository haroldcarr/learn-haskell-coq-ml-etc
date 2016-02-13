{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom

data Model = Model { topCounter :: MModel
                   , bottomCounter :: MModel }

initCounter :: Int -> Int -> Model
initCounter = Model

data Action
  = Reset
  | Top AAction
  | Bottom AAction

update :: Action -> Model -> Model
update Main.Reset model = initCounter 0 0
update (Top act) model = model { topCounter = uUpdate act (topCounter model) }
update (Bottom act) model = model { bottomCounter = uUpdate act (bottomCounter model) }

view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model =
  el "div" $ do
    tc <- mapDyn topCounter model
    bc <- mapDyn bottomCounter model
    topAction <- vView tc
    bottomAction <- vView bc
    (btn, _) <- el' "button" $ text "RESET"
    return $ leftmost [ fmap Top topAction
                      , fmap Bottom bottomAction
                      , fmap (const Main.Reset) (_el_clicked btn)]

main = mainWidget $ do
  let initial = initCounter 0 0
  rec changes <- view model
      model <- foldDyn update initial changes
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
    return $ leftmost [ fmap (const Decrement) (_el_clicked decrement)
                      , fmap (const Increment) (_el_clicked increment) ]
