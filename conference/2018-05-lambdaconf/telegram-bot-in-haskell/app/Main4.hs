{-# LANGUAGE OverloadedStrings #-}
-- | This is a todo bot which can only add new items to the list.
-- See debug output for saved items.
module Main where

import           Data.Text                        (Text)
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

-- | Bot conversation state model.
data Model = Model
  { todoItems :: [TodoItem]   -- ^ A list of todo items.
  } deriving (Show)

-- | An item in a todo list.
data TodoItem = TodoItem
  { todoItemTitle :: Text   -- ^ Item title.
  } deriving (Show)

-- | Initial state.
inititalModel :: Model
inititalModel = Model { todoItems = [] }

-- | Create a new todo item with just a title.
mkTodoItem :: Text -> TodoItem
mkTodoItem title = TodoItem { todoItemTitle = title }

-- | Add a new item to the model.
addItem :: TodoItem -> Model -> Model
addItem item model = model { todoItems = item : todoItems model }

-- | Actions bot can perform.
data Action
  = NoAction      -- ^ Perform no action.
  | AddItem Text  -- ^ Add a new todo item.
  deriving (Show)

-- | Bot application.
bot :: BotApp Model Action
bot = BotApp
  { botInitialModel = Model []
  , botAction = flip handleUpdate
  , botHandler = handleAction
  , botJobs = []
  }

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate (AddItem <$> text)

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  AddItem title -> addItem (mkTodoItem title) model <# do
    replyText "Noted."
    pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
