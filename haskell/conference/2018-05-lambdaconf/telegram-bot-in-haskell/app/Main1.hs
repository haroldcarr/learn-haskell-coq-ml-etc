-- | This is a bot that does nothing.
module Main where

import qualified Telegram.Bot.API          as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug

-- | Bot conversation state model.
data Model = Model
  deriving (Show)

-- | Actions bot can perform.
data Action
  = NoAction    -- ^ Perform no action.
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
handleUpdate _ _ = Nothing

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction NoAction model = pure model

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
