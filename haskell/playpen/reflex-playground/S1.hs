{-# LANGUAGE RecursiveDo #-}

import           Control.Applicative ((<*>), (<$>))
import qualified Data.Map            as Map
import           Reflex
import           Reflex.Dom
import           Safe                (readMay)

main = mainWidget $ el "div" $ do
    nx           <- numberInput
    d            <- dropdown "*" (constDyn ops) def
    ny           <- numberInput
    values       <- combineDyn (,) nx ny
    result       <- combineDyn (\o (x,y) -> stringToOp o <$> x <*> y) (_dropdown_value d) values
    --  resultString <- mapDyn show result
    --  dynText resultString
    button "FOO"
    text " = "
    display result

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = Map.singleton "style" "border-color: red"
      validState = Map.singleton "style" "border-color: green"
  rec n      <- textInput $ def & textInputConfig_inputType    .~ "number"
                                & textInputConfig_initialValue .~ "0"
                                & textInputConfig_attributes   .~ attrs
      result <- mapDyn readMay $ _textInput_value n
      attrs  <- mapDyn (\r -> case r of
                                  Just _  -> validState
                                  Nothing -> errorState) result
  return result

stringToOp s = case s of
    "-" -> (-)
    "*" -> (*)
    "/" -> (/)
    _   -> (+)

ops = Map.fromList [("+", "+"), ("-", "-"), ("*", "*"), ("/", "/")]

