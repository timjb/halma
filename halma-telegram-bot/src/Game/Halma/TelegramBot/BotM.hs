{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Game.Halma.TelegramBot.BotM
  ( GeneralBotM
  , GlobalBotM
  , BotM
  , evalGlobalBotM
  , stateZoom
  , runReq
  , Msg
  , textMsg
  , sendMsg
  , translate
  , printError
  , logErrors
  ) where

import Game.Halma.TelegramBot.Model
import Game.Halma.TelegramBot.View.I18n

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Network.HTTP.Client (Manager)
import Servant.Common.Req (ServantError)
import System.IO (hPrint, stderr)
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

newtype GeneralBotM s a
  = GeneralBotM
  { unGeneralBotM :: ReaderT BotConfig (StateT s IO) a
  } deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadMask
    , MonadState s, MonadReader BotConfig
    )

type GlobalBotM = GeneralBotM BotState
type BotM = GeneralBotM HalmaChat

evalGlobalBotM :: GlobalBotM a -> BotConfig -> IO a
evalGlobalBotM action cfg =
  evalStateT (runReaderT (unGeneralBotM action) cfg) initialBotState

stateZoom :: t -> GeneralBotM t a -> GeneralBotM s (a, t)
stateZoom initial action = do
  GeneralBotM $
    ReaderT $ \cfg ->
      liftIO $ runStateT (runReaderT (unGeneralBotM action) cfg) initial

runReq :: (TG.Token -> Manager -> IO a) -> GeneralBotM s a
runReq reqAction = do
  cfg <- ask
  liftIO $ reqAction (bcToken cfg) (bcManager cfg)

type Msg = ChatId -> TG.SendMessageRequest

textMsg :: T.Text -> Msg
textMsg text chatId =
  TG.SendMessageRequest
    { TG.message_chat_id = T.pack (show chatId)
    , TG.message_text = text
    , TG.message_parse_mode = Just TG.Markdown
    , TG.message_disable_web_page_preview = Just True
    , TG.message_disable_notification = Nothing
    , TG.message_reply_to_message_id = Nothing
    , TG.message_reply_markup = Nothing
    }

sendMsg :: Msg -> BotM ()
sendMsg createMsg = do
  chatId <- gets hcId
  logErrors $ runReq $ \token -> TG.sendMessage token (createMsg chatId)

translate :: (HalmaLocale -> a) -> BotM a
translate getTranslation = gets (getTranslation . localeById . hcLocale)

printError :: (MonadIO m, Show a) => a -> m ()
printError val = liftIO (hPrint stderr val)

logErrors :: BotM (Either ServantError a) -> BotM ()
logErrors action =
  action >>= \case
    Left err -> printError err
    Right _res -> return ()