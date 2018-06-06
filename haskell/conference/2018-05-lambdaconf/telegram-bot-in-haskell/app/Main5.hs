{-# LANGUAGE OverloadedStrings #-}
-- | This todo bot can add items, remove them
-- and show a current list of things to do.
module Main where

import           Control.Applicative              ((<|>))
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

-- | Add a new item to todo list.
addItem :: TodoItem -> Model -> Model
addItem item model = model { todoItems = item : todoItems model }

-- | Remove an item from todo list
removeItem :: Text -> Model -> Model
removeItem title model = model { todoItems = filter p (todoItems model) }
  where
    p item = todoItemTitle item /= title

-- | Pretty print a single todo item.
ppTodoItem :: TodoItem -> Text
ppTodoItem item = "- " <> todoItemTitle item <> "\n"

-- | Pretty print a todo list.
ppItems :: Model -> Text
ppItems model =
  case foldMap ppTodoItem (todoItems model) of
    ""    -> "No things to do yet. Would like to add a new item? :)"
    items -> "Some things for you to do:\n" <> items

-- | Actions bot can perform.
data Action
  = NoAction        -- ^ Perform no action.
  | AddItem Text    -- ^ Add a new todo item.
  | RemoveItem Text -- ^ Remove an item by its title.
  | ShowItems       -- ^ Display all items.
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
handleUpdate _ = parseUpdate
    $ RemoveItem  <$> (command "remove" <|> command "done")
  <|> ShowItems   <$  command "show"
  <|> AddItem     <$> text

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  AddItem title -> addItem (mkTodoItem title) model <# do
    replyText "Noted."
    pure NoAction
  RemoveItem title -> removeItem title model <# do
    replyText ("Removed item: " <> title)
    pure ShowItems
  ShowItems -> model <# do
    replyText (ppItems model)
    pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  startBot_ (traceBotDefault bot) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
