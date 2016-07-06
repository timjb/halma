module Game.Halma.TelegramBot.Controller.Types
  ( BotConfig (..)
  , BotState (..)
  ) where

import Game.Halma.TelegramBot.Model.Types

import Network.HTTP.Client (Manager)
import qualified Data.Map as M
import qualified Web.Telegram.API.Bot as TG

data BotConfig
  = BotConfig
  { bcToken :: TG.Token
  , bcOutputDirectory :: Maybe FilePath
  , bcManager :: Manager
  }

data BotState
  = BotState
  { bsNextId :: Int
  , bsChats :: M.Map ChatId HalmaChat
  } deriving (Show)