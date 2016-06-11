{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
  , Party (..)
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

import Data.Aeson ((.=), (.:))
import Data.Monoid ((<>))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as TG

-- TODO: remove this orphan instance
deriving instance Eq TG.User

data Player
  = AIPlayer
  | TelegramPlayer TG.User
  deriving (Eq, Show)

instance A.ToJSON Player where
  toJSON = \case
    AIPlayer -> "AIPlayer"
    TelegramPlayer user -> A.toJSON user

instance A.FromJSON Player where
  parseJSON = \case
    A.String "AIPlayer" -> pure AIPlayer
    other -> TelegramPlayer <$> A.parseJSON other

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

data PlayerResult
  = PlayerResult
  { prPlayer :: Player -- ^ the player
  , prCount :: Int -- ^ number of moves
  } deriving (Show, Eq)

instance A.ToJSON PlayerResult where
  toJSON playerResult =
    A.object
      [ "player" .= prPlayer playerResult
      , "count" .= prCount playerResult
      ]

instance A.FromJSON PlayerResult where
  parseJSON =
    A.withObject "PlayerResult" $ \o -> do
      player <- o .: "player"
      count <- o .: "count"
      pure PlayerResult { prPlayer = player, prCount = count }

newtype GameResult
  = GameResult
  { grNumberOfMoves :: [PlayerResult]
  } deriving (Show)

instance A.ToJSON GameResult where
  toJSON gameResult =
    A.object
      [ "number_of_moves" .= grNumberOfMoves gameResult
      ]

instance A.FromJSON GameResult where
  parseJSON =
    A.withObject "GameResult" $ \o ->
      GameResult <$> o .: "number_of_moves"

data Party
  = Party
  { partyHomeCorner :: HalmaDirection
  , partyPlayer :: Player
  } deriving (Show, Eq)

instance A.ToJSON Party where
  toJSON party =
    A.object
      [ "home_corner" .= partyHomeCorner party
      , "player" .= partyPlayer party
      ]

instance A.FromJSON Party where
  parseJSON =
    A.withObject "Party" $ \o -> do
      homeCorner <- o .: "home_corner"
      player <- o .: "player"
      pure Party { partyHomeCorner = homeCorner, partyPlayer = player }

data HalmaState
  = HalmaState
  { hsBoard :: HalmaBoard
  , hsTurnCounter :: TurnCounter Party
  , hsLastMove :: Maybe Move
  } deriving (Eq, Show)

instance A.ToJSON HalmaState where
  toJSON game =
    A.object
      [ "board" .= hsBoard game
      , "parties" .= tcPlayers (hsTurnCounter game)
      , "total_moves" .= tcCounter (hsTurnCounter game)
      , "last_move" .= hsLastMove game
      ]

instance A.FromJSON HalmaState where
  parseJSON =
    A.withObject "HalmaState" $ \o -> do
      board <- o .: "board"
      parties <- o .: "parties"
      totalMoves <- o .: "total_moves"
      mLastMove <- o .: "last_move"
      let
        turnCounter =
          TurnCounter
            { tcPlayers = parties
            , tcCounter = totalMoves
            }
      pure
        HalmaState
          { hsBoard = board
          , hsTurnCounter = turnCounter
          , hsLastMove = mLastMove
          }

initialHalmaState :: Configuration Player -> HalmaState
initialHalmaState config =
  let
    (board, turnCounter) = newGame config
  in
    HalmaState
      { hsBoard = board
      , hsTurnCounter = toParty <$> turnCounter
      , hsLastMove = Nothing
      }
  where
    toParty (dir, player) = Party { partyPlayer = player, partyHomeCorner = dir }

doMove :: Move -> HalmaState -> Either String HalmaState
doMove move state =
  case movePiece move (hsBoard state) of
    Left err -> Left err
    Right board' ->
      Right
        HalmaState
          { hsBoard = board'
          , hsTurnCounter = nextTurn (hsTurnCounter state)
          , hsLastMove = Just move
          }

undoLastMove :: HalmaState -> Maybe HalmaState
undoLastMove state = do
  move <- hsLastMove state
  board' <- eitherToMaybe $ movePiece (invertMove move) (hsBoard state)
  Just
    HalmaState
      { hsBoard = board'
      , hsTurnCounter = previousTurn (hsTurnCounter state)
      , hsLastMove = Nothing
      }
  where
    eitherToMaybe = either (const Nothing) Just
    invertMove Move { moveFrom = from, moveTo = to } =
      Move { moveFrom = to, moveTo = from }

data Match
  = Match
  { matchConfig :: Configuration Player
  , matchRules :: RuleOptions
  , matchHistory :: [GameResult]
  , matchCurrentGame :: Maybe HalmaState
  } deriving (Show)

instance A.ToJSON Match where
  toJSON match =
    A.object
      [ "config" .= matchConfig match
      , "rules" .= matchRules match
      , "history" .= matchHistory match
      , "current_game" .= matchCurrentGame match
      ]

instance A.FromJSON Match where
  parseJSON =
    A.withObject "Match size" $ \o -> do
      config <- o .: "config"
      rules <- o .: "rules"
      history <- o .: "history"
      currentGame <- o .: "current_game"
      pure
        Match
          { matchConfig = config
          , matchRules = rules
          , matchHistory = history
          , matchCurrentGame = currentGame
          }

newMatch :: Configuration Player -> Match
newMatch config =
  Match
    { matchConfig = config
    , matchRules = rules
    , matchHistory = []
    , matchCurrentGame = Just (initialHalmaState config)
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
  | EnoughPlayers (Configuration a)

deriving instance Show a => Show (PlayersSoFar a)

data MatchState
  = NoMatch
  | GatheringPlayers (PlayersSoFar Player)
  | MatchRunning Match

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
