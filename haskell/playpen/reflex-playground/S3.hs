{-# LANGUAGE RecursiveDo #-}

-- https://github.com/BartAdv/elm-architecture-tutorial-reflex

import Reflex.Dom

type Model = Int

data Action = Increment | Decrement

update :: Action -> Model -> Model
update Increment model = model + 1
update Decrement model = model - 1

-- we do not return html, it's done by MonadWidget
view :: MonadWidget t m => Dynamic t Model -> m (Event t Action)
view model =
  el "div" $ do
    (decrement, _) <- el' "button" $ text "-"
    el "div" $ do
      t <- mapDyn show model
      dynText t
    (increment, _) <- el' "button" $ text "+"
    return $ leftmost [ fmap (const Decrement) (domEvent Click decrement)
                      , fmap (const Increment) (domEvent Click increment) ]

main = mainWidget $ el "div" $ do
  let initial = 0
  rec changes <- view model
      model <- foldDyn update initial changes
  return ()

