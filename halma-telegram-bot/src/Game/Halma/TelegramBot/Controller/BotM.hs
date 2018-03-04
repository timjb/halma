{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Game.Halma.TelegramBot.Controller.BotM
  ( GeneralBotM
  , GlobalBotM
  , BotM
  , evalGlobalBotM
  , stateZoom
  , runReq
  , printError
  , logErrors
  ) where

import Game.Halma.TelegramBot.Controller.Types
import Game.Halma.TelegramBot.Model

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Network.HTTP.Client (Manager)
import Servant.Client (ServantError)
import System.IO (hPrint, stderr)
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

initialBotState :: BotState
initialBotState =
  BotState
    { bsNextId = 0
    , bsChats = mempty
    }

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

printError :: (MonadIO m, Show a) => a -> m ()
printError val = liftIO (hPrint stderr val)

logErrors :: BotM (Either ServantError a) -> BotM ()
logErrors action =
  action >>= \case
    Left err -> printError err
    Right _res -> return ()