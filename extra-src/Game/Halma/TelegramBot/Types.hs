{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Halma.TelegramBot.Types
  ( Player (..)
  , showUser
  , showPlayer
  , GameResult (..)
  , HalmaState (..)
  , doMove
  , undoLastMove
  , Match (..)
  , newMatch
  , MatchState (..)
  , ChatId
  , PlayersSoFar (..)
  , BotState (..)
  , initialBotState
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.Rules
import Game.Halma.TelegramBot.I18n
import Game.TurnCounter

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

data HalmaState size a
  = HalmaState
  { hsBoard :: HalmaBoard size
  , hsTurnCounter :: TurnCounter (Team, a)
  , hsLastMove :: Maybe Move
  } deriving (Eq, Show)

newGame :: Configuration size a -> HalmaState size a
newGame (Configuration halmaGrid nop) =
  HalmaState
    { hsBoard = initialBoard halmaGrid isActive
    , hsTurnCounter = newTurnCounter players
    , hsLastMove = Nothing
    }
  where
    players = getPlayers nop
    isActive color = color `elem` (fst <$> players)

doMove :: Move -> HalmaState size a -> Either String (HalmaState size a)
doMove move state =
  case movePiece move (hsBoard state) of
    Left err -> Left err
    Right board' ->
      Right $
        HalmaState
          { hsBoard = board'
          , hsTurnCounter = nextTurn (hsTurnCounter state)
          , hsLastMove = Just move
          }

undoLastMove :: HalmaState size a -> Maybe (HalmaState size a)
undoLastMove state = do
  move <- hsLastMove state
  board' <- eitherToMaybe $ movePiece (invertMove move) (hsBoard state)
  Just $
    HalmaState
      { hsBoard = board'
      , hsTurnCounter = previousTurn (hsTurnCounter state)
      , hsLastMove = Nothing
      }
  where
    eitherToMaybe = either (const Nothing) Just
    invertMove (Move { moveFrom = from, moveTo = to }) =
      Move { moveFrom = to, moveTo = from }

data Match size
  = Match
  { matchConfig :: Configuration size Player
  , matchRules :: RuleOptions
  , matchHistory :: [GameResult]
  , matchCurrentGame :: Maybe (HalmaState size Player)
  } deriving (Show)

newMatch :: Configuration size Player -> Match size
newMatch config =
  Match
    { matchConfig = config
    , matchRules = rules
    , matchHistory = []
    , matchCurrentGame = Just (newGame config)
    }
  where
    rules =
      RuleOptions
        { movingBackwards = Temporarily
        , invading = Allowed
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
  { bsLocale :: HalmaLocale
  , bsChatId :: ChatId
  , bsNextId :: Int
  , bsToken :: TG.Token
  , bsMatchState :: MatchState
  } deriving (Show)

initialBotState :: T.Text -> TG.Token -> BotState
initialBotState chatId token =
  BotState
    { bsLocale = deHalmaLocale
    , bsChatId = chatId
    , bsToken = token
    , bsNextId = 0
    , bsMatchState = NoMatch
    }
