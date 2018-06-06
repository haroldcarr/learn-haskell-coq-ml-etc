{-# LANGUAGE OverloadedStrings #-}
-- | This bot replies "Got it" to every incoming update.
module Main where

import           Data.Text                 (Text)
import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
  | Reply Text  -- ^ Reply some text.
  deriving (Show)

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ _ = Just (Reply "Got it")

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model
handleAction (Reply message) model = model <# do
  replyText message
  pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
