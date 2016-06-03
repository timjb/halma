{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Halma.TelegramBot.Types
  ( Player (..)
  , showUser
  , showPlayer
  , GameResult (..)
  , Match (..)
  , newMatch
  , MatchState (..)
  , ChatId
  , PlayersSoFar (..)
  , BotState (..)
  , initialBotState
  ) where

import Game.Halma.Configuration
import Game.Halma.State

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

-- TODO: remove this orphan instance
deriving instance Eq TG.User

data Player
  = AIPlayer
  | TelegramPlayer TG.User
  deriving (Eq, Show)

showUser :: TG.User -> T.Text
showUser user =
  case TG.user_username user of
    Just username -> "@" <> username
    Nothing ->
      TG.user_first_name user <>
      maybe "" (" " <>) (TG.user_last_name user)

showPlayer :: Player -> T.Text
showPlayer player =
  case player of
    AIPlayer -> "AI"
    TelegramPlayer user -> showUser user

newtype GameResult
  = GameResult
  { numberOfMoves :: [(Player, Int)]
  } deriving (Show)

data Match size
  = Match
  { matchConfig :: Configuration size Player
  , matchHistory :: [GameResult]
  , matchCurrentGame :: Maybe (HalmaState size Player)
  } deriving (Show)

newMatch :: Configuration size Player -> Match size
newMatch config =
  Match
    { matchConfig = config
    , matchHistory = []
    , matchCurrentGame = Just (newGame config)
    }

data PlayersSoFar a
  = NoPlayers
  | OnePlayer a
  | forall size. EnoughPlayers (Configuration size a)

deriving instance Show a => Show (PlayersSoFar a)

data MatchState
  = NoMatch
  | GatheringPlayers (PlayersSoFar Player)
  | forall size. MatchRunning (Match size)

deriving instance Show MatchState

type ChatId = T.Text

data BotState
  = BotState
  { bsChatId :: ChatId
  , bsNextId :: Int
  , bsToken :: TG.Token
  , bsMatchState :: MatchState
  } deriving (Show)

initialBotState :: T.Text -> TG.Token -> BotState
initialBotState chatId token =
  BotState
    { bsChatId = chatId
    , bsToken = token
    , bsNextId = 0
    , bsMatchState = NoMatch
    }
