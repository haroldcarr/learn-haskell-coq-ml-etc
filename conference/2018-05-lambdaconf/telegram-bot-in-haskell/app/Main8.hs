{-# LANGUAGE OverloadedStrings #-}
-- | This todo bot can add items, remove them
-- and show an interactive list of things to do,
-- where you can easily mark items as done
-- or set reminders in 1 or 5 minutes.
--
-- Try /start command to see a helpful description of this bot.
--
-- Use reply keyboard for some helpful todo suggestions.
module Main where

import           Control.Applicative              ((<|>))
import           Control.Concurrent               (threadDelay)
import           Control.Monad.Trans              (liftIO)
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Time
import qualified Telegram.Bot.API                 as Telegram
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.Debug
import           Telegram.Bot.Simple.UpdateParser

-- | Bot conversation state model.
data Model = Model
  { todoItems   :: [TodoItem]   -- ^ A list of todo items.
  , currentTime :: UTCTime      -- ^ Current time.
  } deriving (Show)

-- | An item in a todo list.
data TodoItem = TodoItem
  { todoItemTitle    :: Text            -- ^ Item title.
  , todoItemReminder :: Maybe UTCTime   -- ^ Optional notification time.
  } deriving (Show)

-- | Initial state.
inititalModel :: IO Model
inititalModel = do
  now <- getCurrentTime
  pure Model { todoItems = [], currentTime = now }

-- | Create a new todo item with just a title.
mkTodoItem :: Text -> TodoItem
mkTodoItem title = TodoItem
  { todoItemTitle    = title
  , todoItemReminder = Nothing
  }

-- | Add a new item to todo list.
addItem :: TodoItem -> Model -> Model
addItem item model = model { todoItems = item : todoItems model }

-- | Remove an item from todo list
removeItem :: Text -> Model -> Model
removeItem title model = model { todoItems = filter p (todoItems model) }
  where
    p item = todoItemTitle item /= title

-- | Set alarm time for an item with a given title
-- in a specified number of minutes from now.
setReminderIn :: Int -> Text -> Model -> Model
setReminderIn minutes title model = setReminder title alarmTime model
  where
    now = currentTime model
    alarmTime = addUTCTime (fromIntegral (60 * minutes)) now

-- | Set an absolute alarm time for an item with a given title.
setReminder :: Text -> UTCTime -> Model -> Model
setReminder title datetime model = model
  { todoItems = map updateReminder (todoItems model) }
    where
      updateReminder item
        | title /= todoItemTitle item = item
        | otherwise = item { todoItemReminder = Just datetime }

-- | Pretty print a single todo item.
ppTodoItem :: TodoItem -> Text
ppTodoItem item = "- " <> todoItemTitle item <> "\n"

-- | Pretty print a todo list.
ppItems :: Model -> Text
ppItems model =
  case foldMap ppTodoItem (todoItems model) of
    ""    -> "No things to do yet. Would you like to add a new item? :)"
    items -> "Some things for you to do:\n" <> items

-- | Display todo items as a message with inline keyboard,
-- where each button represents a single item.
itemsAsInlineKeyboard :: Model -> EditMessage
itemsAsInlineKeyboard model =
  case todoItems model of
    [] -> "No things to do yet. Would you like to add a new item? :)"
    items -> (toEditMessage "Some things for you to do:")
      { editMessageReplyMarkup = Just $
          Telegram.SomeInlineKeyboardMarkup (itemsInlineKeyboard items)
      }

-- | Inline keyboard with every button representing one item.
itemsInlineKeyboard :: [TodoItem] -> Telegram.InlineKeyboardMarkup
itemsInlineKeyboard
  = Telegram.InlineKeyboardMarkup .  map (pure . itemInlineKeyboardButton)

-- | Inline keyboard button for a given todo item.
itemInlineKeyboardButton :: TodoItem -> Telegram.InlineKeyboardButton
itemInlineKeyboardButton item = actionButton title (RevealItemActions title)
  where
    title = todoItemTitle item

-- | Actions bot can perform.
data Action
  = NoAction                -- ^ Perform no action.
  | SetTime UTCTime         -- ^ Update current time.
  | AddItem Text            -- ^ Add a new todo item.
  | RemoveItem Text         -- ^ Remove an item by its title.
  | ShowItems               -- ^ Display all items (either with a new message or by updating existing one).
  | Start                   -- ^ Display start message.
  | RevealItemActions Text  -- ^ Update list of items to display item actions.
  | SetReminderIn Int Text  -- ^ Set a reminder for an item in a given amount of minutes from now.
  deriving (Show, Read)

-- | Bot application.
initBot :: IO (BotApp Model Action)
initBot = do
  model <- inititalModel
  pure BotApp
    { botInitialModel = model
    , botAction = flip handleUpdate
    , botHandler = handleAction
    , botJobs =
      [ BotJob
        { botJobSchedule = "* * * * *"  -- every minute
        , botJobTask = todoReminder
        }
      ]
    }

-- | Remind user of things to do (if there are any).
todoReminder :: Model -> Eff Action Model
todoReminder model = do
  newItems <- mapM itemReminder (todoItems model)
  pure model { todoItems = newItems }
  where
    itemReminder item =
      case todoItemReminder item of
        Just alarmTime | alarmTime <= currentTime model -> do
          eff $ do
            replyText ("Reminder: " <> todoItemTitle item)
            return NoAction
          return item { todoItemReminder = Nothing }
        _ -> return item

-- | A help message to show on conversation start with bot.
startMessage :: Text
startMessage = Text.unlines
 [ "Hi there! I am your personal todo bot :)"
 , ""
 , "I can help you keep track of things to do:"
 , ""
 , "- Just type what you need to do an I'll remember it!"
 , "- Use /remove <item> to remove an item"
 , "- Use /show to show all current things to do"
 , ""
 , "So what's the first thing on your to do list? :)"
 ]

-- | A start keyboard with some helpful todo suggestions.
startMessageKeyboard :: Telegram.ReplyKeyboardMarkup
startMessageKeyboard = Telegram.ReplyKeyboardMarkup
  { Telegram.replyKeyboardMarkupKeyboard =
      [ [ "Drink water", "Eat fruit" ]
      , [ "Build a house", "Raise a son", "Plant a tree" ]
      , [ "Spend time with family", "Call parents" ]
      ]
  , Telegram.replyKeyboardMarkupResizeKeyboard = Just True
  , Telegram.replyKeyboardMarkupOneTimeKeyboard = Just True
  , Telegram.replyKeyboardMarkupSelective = Just True
  }

-- | Actions to do with an item as an inline keyboard message.
itemActionsMessage :: Text -> EditMessage
itemActionsMessage title = (toEditMessage ("«" <> title <> "»"))
  { editMessageReplyMarkup = Just $
      Telegram.SomeInlineKeyboardMarkup (itemActionsKeyboard title) }

-- | Actions to do with an item as an inline keyboard.
itemActionsKeyboard :: Text -> Telegram.InlineKeyboardMarkup
itemActionsKeyboard title = Telegram.InlineKeyboardMarkup
  [ [ btnRemove ]
  , [ btnRemindIn 1, btnRemindIn 5 ]
  , [ btnBack ]
  ]
    where
      btnRemove = actionButton "\x2705 Done" (RemoveItem title)
      btnBack   = actionButton "\x2B05 Back to items list" ShowItems

      btnRemindIn n = actionButton
        ("\x23F0 " <> Text.pack (show n) <> " min")
        (SetReminderIn n title)

-- | How to process incoming 'Telegram.Update's
-- and turn them into 'Action's.
handleUpdate :: Model -> Telegram.Update -> Maybe Action
handleUpdate _ = parseUpdate
    $ ShowItems   <$  command "show"
  <|> RemoveItem  <$> (command "remove" <|> command "done")
  <|> Start       <$  command "start"
  <|> callbackQueryDataRead

  <|> AddItem     <$> text

-- | How to handle 'Action's.
handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoAction -> pure model
  SetTime t -> model { currentTime = t } <# do
    SetTime <$> liftIO (threadDelay 1000 >> getCurrentTime)
  AddItem title -> addItem (mkTodoItem title) model <# do
    replyText "Noted."
    pure NoAction
  RemoveItem title -> removeItem title model <# do
    replyText ("Removed item: " <> title)
    pure ShowItems
  ShowItems -> model <# do
    replyOrEdit (itemsAsInlineKeyboard model)
    pure NoAction
  Start -> do
    eff $ do
      reply (toReplyMessage startMessage)
        { replyMessageReplyMarkup = Just $
            Telegram.SomeReplyKeyboardMarkup startMessageKeyboard
        }
      pure NoAction
    eff $ SetTime <$> liftIO getCurrentTime
    pure model
  RevealItemActions title -> model <# do
    editUpdateMessage (itemActionsMessage title)
    pure NoAction
  SetReminderIn minutes title -> setReminderIn minutes title model <# do
    replyText "Ok, I will remind you."
    pure NoAction

-- | Run bot with a given 'Telegram.Token'.
run :: Telegram.Token -> IO ()
run token = do
  env <- Telegram.defaultTelegramClientEnv token
  bot <- initBot
  startBot_ (useLatestUpdateInJobs (traceBotDefault bot)) env

-- | Run bot using 'Telegram.Token' from @TELEGRAM_BOT_TOKEN@ environment.
main :: IO ()
main = getEnvToken "TELEGRAM_BOT_TOKEN" >>= run
