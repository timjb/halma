{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Game.Halma.TelegramBot.BotM
  ( BotM
  , evalBotM
  , runReq
  , Msg
  , textMsg
  , sendMsg
  , printError
  , logErrors
  ) where

import Game.Halma.TelegramBot.Types

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState, gets)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Common.Req (ServantError)
import System.IO (hPrint, stderr)
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

newtype BotM a
  = BotM
  { unBotM :: ReaderT Manager (StateT BotState IO) a
  } deriving
    ( Functor, Applicative, Monad
    , MonadIO, MonadThrow, MonadCatch, MonadMask
    , MonadState BotState, MonadReader Manager
    )

evalBotM :: BotM a -> BotState -> IO a
evalBotM action initialState = do
  manager <- newManager tlsManagerSettings
  evalStateT (runReaderT (unBotM action) manager) initialState

runReq :: (TG.Token -> Manager -> IO a) -> BotM a
runReq reqAction = do
  manager <- ask
  token <- gets bsToken
  liftIO (reqAction token manager)

type Msg = ChatId -> TG.SendMessageRequest

textMsg :: T.Text -> Msg
textMsg text chatId = TG.sendMessageRequest chatId text

sendMsg :: Msg -> BotM ()
sendMsg createMsg = do
  chatId <- gets bsChatId
  logErrors $ runReq $ \token -> TG.sendMessage token (createMsg chatId)

printError :: (MonadIO m, Show a) => a -> m ()
printError val = liftIO (hPrint stderr val)

logErrors :: BotM (Either ServantError a) -> BotM ()
logErrors action =
  action >>= \case
    Left err -> printError err
    Right _res -> return ()