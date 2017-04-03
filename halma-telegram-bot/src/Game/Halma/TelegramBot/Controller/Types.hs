module Game.Halma.TelegramBot.Controller.Types
  ( BotPersistence (..)
  , BotConfig (..)
  , BotState (..)
  ) where

import Game.Halma.TelegramBot.Model.Types

import Network.HTTP.Client (Manager)
import qualified Data.Map as M
import qualified Web.Telegram.API.Bot as TG

data BotPersistence
  = BotPersistence
  { bpSave :: HalmaChat -> IO ()
  , bpLoad :: ChatId -> IO (Maybe HalmaChat)
  }

data BotConfig
  = BotConfig
  { bcToken :: TG.Token
  , bcPersistence :: BotPersistence
  , bcManager :: Manager
  }

data BotState
  = BotState
  { bsNextId :: Int
  , bsChats :: M.Map ChatId HalmaChat
  } deriving (Show)