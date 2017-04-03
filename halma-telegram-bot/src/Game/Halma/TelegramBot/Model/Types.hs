{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Game.Halma.TelegramBot.Model.Types
  ( Player (..)
  , PartyResult (..)
  , ExtendedPartyResult (..)
  , GameResult (..)
  , HalmaState (..)
  , Party (..)
  , Match (..)
  , MatchState (..)
  , ChatId
  , PlayersSoFar (..)
  , LocaleId (..)
  , HalmaChat (..)
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.Rules
import Game.TurnCounter

import Data.Aeson ((.=), (.:))
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.Vector as V
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

data PartyResult
  = PartyResult
  { prParty :: Party
  , prNumberOfTurns :: Int
  } deriving (Eq, Show)

instance A.ToJSON PartyResult where
  toJSON pr =
    A.object
      [ "party" .= prParty pr
      , "number_of_turns" .= prNumberOfTurns pr
      ]

instance A.FromJSON PartyResult where
  parseJSON =
    A.withObject "PartyResult" $ \o -> do
      prParty <- o .: "party"
      prNumberOfTurns <- o .: "number_of_turns"
      pure PartyResult {..}

data ExtendedPartyResult
  = ExtendedPartyResult
  { eprPartyResult :: PartyResult
  , eprPlace :: Int -- ^ zero-based position
  , eprPlaceShared :: Bool -- ^ has another player finished in the same round?
  , eprLag :: Int -- ^ number of moves after winner
  , eprNumberOfPlayers :: Int
  } deriving (Eq, Show)

newtype GameResult
  = GameResult
  { grNumberOfMoves :: [PartyResult]
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
  , hsFinished :: [PartyResult]
  } deriving (Eq, Show)

instance A.ToJSON HalmaState where
  toJSON game =
    A.object
      [ "board" .= hsBoard game
      , "parties" .= tcPlayers (hsTurnCounter game)
      , "total_moves" .= tcCounter (hsTurnCounter game)
      , "last_move" .= hsLastMove game
      , "finished" .= hsFinished game
      ]

instance A.FromJSON HalmaState where
  parseJSON =
    A.withObject "HalmaState" $ \o -> do
      hsBoard <- o .: "board"
      tcPlayers <- o .: "parties"
      tcCounter <- o .: "total_moves"
      hsLastMove <- o .: "last_move"
      hsFinished <- o .: "finished"
      let hsTurnCounter = TurnCounter {..}
      pure HalmaState {..}

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

data PlayersSoFar a
  = NoPlayers
  | OnePlayer a
  | EnoughPlayers (Configuration a)
  deriving (Show)

instance A.ToJSON a => A.ToJSON (PlayersSoFar a) where
  toJSON =
    \case
      NoPlayers -> A.Array mempty
      OnePlayer p -> A.toJSON [p]
      EnoughPlayers config -> A.toJSON config

instance A.FromJSON a => A.FromJSON (PlayersSoFar a) where
  parseJSON val =
    parseEnoughPlayers val <|> parseTooFewPlayers val
    where
      parseEnoughPlayers v =
        EnoughPlayers <$> A.parseJSON v
      parseTooFewPlayers =
        A.withArray "PlayersSoFar" $ \v ->
          case V.length v of
            0 -> pure NoPlayers
            1 -> OnePlayer <$> A.parseJSON (V.head v)
            _ -> fail "expected an array of length 1 or 2"

data MatchState
  = NoMatch
  | GatheringPlayers (PlayersSoFar Player)
  | MatchRunning Match
  deriving (Show)

instance A.ToJSON MatchState where
  toJSON =
    \case
      NoMatch ->
        A.object [ "state" .= ("no_match" :: T.Text) ]
      GatheringPlayers playersSoFar ->
        A.object
          [ "state" .= ("gathering_players" :: T.Text)
          , "players_so_far" .= playersSoFar
          ]
      MatchRunning match ->
        A.object
          [ "state" .= ("match_running" :: T.Text)
          , "match" .= match
          ]

instance A.FromJSON MatchState where
  parseJSON =
    A.withObject "MatchState" $ \o -> do
      state <- o .: "state"
      case state :: T.Text of
        "no_match" ->
          pure NoMatch
        "gathering_players" ->
          GatheringPlayers <$> (o .: "players_so_far")
        "match_running" ->
          MatchRunning <$> (o .: "match")
        _other ->
          fail $ "unexpected state: " ++ T.unpack state

data LocaleId
  = En
  | De
  deriving (Show, Eq, Bounded, Enum)

showLocaleId :: LocaleId -> T.Text
showLocaleId localeId =
  case localeId of
    En -> "en"
    De -> "de"

parseLocaleId :: T.Text -> Maybe LocaleId
parseLocaleId text =
  case T.toLower text of
    "de" -> Just De
    "en" -> Just En
    _ -> Nothing

instance A.ToJSON LocaleId where
  toJSON = A.String . showLocaleId

instance A.FromJSON LocaleId where
  parseJSON =
    A.withText "LocaleId" $ \t ->
      case parseLocaleId t of
        Nothing -> fail "unrecognized locale id"
        Just localeId -> pure localeId

type ChatId = Integer

data HalmaChat
  = HalmaChat
  { hcId :: ChatId
  , hcLocale :: LocaleId
  , hcMatchState :: MatchState
  } deriving (Show)

instance A.ToJSON HalmaChat where
  toJSON HalmaChat {..} =
    A.object
    [ "id" .= hcId
    , "locale" .= hcLocale
    , "match_state" .= hcMatchState
    ]

instance A.FromJSON HalmaChat where
  parseJSON =
    A.withObject "HalmaChat" $ \o -> do
      hcId <- o .: "id"
      hcLocale <- o .: "locale"
      hcMatchState <- o .: "match_state"
      pure HalmaChat {..}
