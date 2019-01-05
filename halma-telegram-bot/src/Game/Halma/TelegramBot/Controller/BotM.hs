{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Game.Halma.TelegramBot.Controller.BotM
  ( GeneralBotM
  , GlobalBotM
  , BotM
  , evalGlobalBotM
  , stateZoom
  , runReq
  , botThrow
  , botCatch
  ) where

import Game.Halma.TelegramBot.Controller.Types
import Game.Halma.TelegramBot.Model

import Control.Arrow (left)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, catchE)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Network.HTTP.Client (Manager)
import qualified Web.Telegram.API.Bot as TG

type BotError = String

newtype GeneralBotM s a
  = GeneralBotM
  { unGeneralBotM :: ReaderT BotConfig (ExceptT BotError (StateT s IO)) a
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

evalGlobalBotM :: GlobalBotM a -> BotConfig -> IO (Either BotError a)
evalGlobalBotM action cfg =
  evalStateT (runExceptT (runReaderT (unGeneralBotM action) cfg)) initialBotState

stateZoom :: t -> GeneralBotM t a -> GeneralBotM s (a, t)
stateZoom initial action =
  GeneralBotM $ ReaderT $ \cfg -> ExceptT $ lift $
    (\(e, y) -> (,y) <$> e) <$> runStateT (runExceptT (runReaderT (unGeneralBotM action) cfg)) initial

runReq :: Show e => (TG.Token -> Manager -> IO (Either e a)) -> GeneralBotM s a
runReq reqAction =
  GeneralBotM $ ReaderT $ \cfg ->
    ExceptT $ lift $ left show <$> reqAction (bcToken cfg) (bcManager cfg)

botThrow :: String -> GeneralBotM s a
botThrow e = GeneralBotM $ lift $ throwE e

botCatch :: GeneralBotM s a -> (String -> GeneralBotM s a) -> GeneralBotM s a
botCatch action handler =
  GeneralBotM $ ReaderT $ \cfg ->
    runReaderT (unGeneralBotM action) cfg `catchE` \e ->
    runReaderT (unGeneralBotM (handler e)) cfg