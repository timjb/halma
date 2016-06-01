{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Halma.TelegramBot.Types
  ( Player (..)
  , showPlayer
  , MatchResult (..)
  , Match (..)
  , newMatch
  , MatchState (..)
  , ChatId
  , BotState (..)
  , initialBotState
  , CmdCall (..)
  , parseCmdCall
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.State

import Control.Monad (guard)
import Data.Char (isAlphaNum, isSpace)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

data Player
  = AIPlayer
  | TelegramPlayer TG.User
  deriving (Show)

showPlayer :: Player -> T.Text
showPlayer player =
  case player of
    AIPlayer -> "AI"
    TelegramPlayer user ->
      case TG.user_username user of
        Just username -> username
        Nothing ->
          TG.user_first_name user <>
          maybe "" (" " <>) (TG.user_last_name user)

newtype MatchResult
  = MatchResult
  { numberOfMoves :: [(Player, Int)]
  } deriving (Show)

data Match
  = Match
  { matchPlayers :: [Player]
  , matchHistory :: [MatchResult]
  , matchCurrentGame :: Maybe (HalmaState 'S)
  } deriving (Show)

newMatch :: [Player] -> Either T.Text Match
newMatch players =
  let
    config =
      case length players of
        i | i < 2 -> Left "can't start a match with at least two players!"
        2 -> Right (Configuration SmallGrid TwoPlayers)
        3 -> Right (Configuration SmallGrid ThreePlayers)
        _ -> Left "can't start a match with more than three players!"
  in
    mkMatch <$> config
  where
    mkMatch config =
      Match
      { matchPlayers = players
      , matchHistory = []
      , matchCurrentGame = Just (newGame config)
      }

data MatchState
  = NoMatch
  | GatheringPlayers [Player]
  | MatchRunning Match
  deriving (Show)

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

data CmdCall
  = CmdCall
  { cmdCallName :: T.Text
  , cmdCallArgs :: Maybe T.Text
  } deriving (Show, Eq)

parseCmdCall :: T.Text -> Maybe CmdCall
parseCmdCall str = do
  guard $ not (T.null str)
  guard $ T.head str == '/'
  let rest = T.tail str
      isCmdChar c = isAlphaNum c || c `elem` ("_-" :: String)
      (cmdName, rest') = T.span isCmdChar rest
  guard $ not (T.null cmdName)
  let rest'' = T.dropWhile isSpace rest'
      args = if T.null rest'' then Nothing else Just rest''
  pure $ CmdCall { cmdCallName = cmdName, cmdCallArgs = args }