module Game.Halma.TelegramBot.Controller.Helpers
  ( translate
  , sendI18nMsg
  , mkKeyboard
  , Msg
  , sendMsg
  , textMsg
  , textMsgWithKeyboard
  ) where

import Game.Halma.TelegramBot.Controller.BotM
import Game.Halma.TelegramBot.Model
import Game.Halma.TelegramBot.View.I18n

import Control.Monad (void)
import Control.Monad.State.Class (gets)
import qualified Web.Telegram.API.Bot as TG
import qualified Data.Text as T

translate :: (HalmaLocale -> a) -> BotM a
translate getTranslation = gets (getTranslation . localeById . hcLocale)

sendI18nMsg :: (HalmaLocale -> T.Text) -> BotM ()
sendI18nMsg getText = do
  text <- translate getText
  sendMsg $ textMsg text -- todo: url link suppression

mkButton :: T.Text -> TG.KeyboardButton
mkButton text =
  TG.KeyboardButton
    { TG.kb_text = text
    , TG.kb_request_contact = Nothing
    , TG.kb_request_location = Nothing
    }

mkKeyboard :: [[T.Text]] -> TG.ReplyKeyboard
mkKeyboard buttonLabels =
  TG.ReplyKeyboardMarkup
    { TG.reply_keyboard = fmap mkButton <$> buttonLabels
    , TG.reply_resize_keyboard = Just True
    , TG.reply_one_time_keyboard = Just True
    , TG.reply_selective = Just False
    }

type Msg = ChatId -> TG.SendMessageRequest

sendMsg :: Msg -> BotM ()
sendMsg createMsg = do
  chatId <- gets hcId
  void $ runReq $ \token -> TG.sendMessage token (createMsg chatId)

textMsg :: T.Text -> Msg
textMsg text chatId =
  TG.SendMessageRequest
    { TG.message_chat_id = TG.ChatId chatId
    , TG.message_text = text
    , TG.message_parse_mode = Just TG.Markdown
    , TG.message_disable_web_page_preview = Just True
    , TG.message_disable_notification = Nothing
    , TG.message_reply_to_message_id = Nothing
    , TG.message_reply_markup = Nothing
    }

textMsgWithKeyboard :: T.Text -> TG.ReplyKeyboard -> Msg
textMsgWithKeyboard text keyboard chatId =
  (TG.sendMessageRequest (TG.ChatId chatId) text)
  { TG.message_reply_markup = Just keyboard }