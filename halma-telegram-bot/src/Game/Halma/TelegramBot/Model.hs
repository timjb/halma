module Game.Halma.TelegramBot.Model
  ( module Game.Halma.TelegramBot.Model.MoveCmd
  , module Game.Halma.TelegramBot.Model.Types
  , AfterMove (..)
  , doMove
  , undoLastMove
  , newMatch
  , newRound
  , initialHalmaChat
  , newRound
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.Rules
import Game.Halma.TelegramBot.Model.Types
import Game.Halma.TelegramBot.Model.MoveCmd
import Game.TurnCounter

import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

initialHalmaChat :: ChatId -> HalmaChat
initialHalmaChat chatId =
  HalmaChat
    { hcId = chatId
    , hcLastUpdateId = 0
    , hcLocale = En
    , hcMatchState = NoMatch
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
      , hsFinished = []
      }
  where
    toParty (dir, player) = Party { partyPlayer = player, partyHomeCorner = dir }

data AfterMove
  = GameContinues (Maybe ExtendedPartyResult) HalmaState -- ^ result if a player finished in the last turn
  | GameEnded ExtendedPartyResult -- ^ result of the last player to finish
  deriving (Show, Eq)

doMove
  :: Move
  -> HalmaState
  -> Either String AfterMove
     -- ^ an error or the new state and whether the party has just won
doMove move state =
  case movePiece move (hsBoard state) of
    Left err -> Left err
    Right board' ->
      let
        wasLastMove = hasFinished board' (partyHomeCorner party)
        mExtendedPartyResult =
          if wasLastMove then
            Just $ getExtendedPartyResult (hsTurnCounter state) (hsFinished state)
          else
            Nothing
        finished' =
          hsFinished state ++
          toList (eprPartyResult <$> mExtendedPartyResult)
        numberOfPlayers = length $ tcPlayers $ hsTurnCounter state
        isGameEnd = length finished' == numberOfPlayers
        turnCounter' =
          fromMaybe (hsTurnCounter state) $
          nextTurnWith (`notElem` map prParty finished') $
          hsTurnCounter state
        state' =
          HalmaState
            { hsBoard = board'
            , hsTurnCounter = turnCounter'
            , hsLastMove = Just move
            , hsFinished = finished'
            }
      in
      Right $
      case mExtendedPartyResult of
        Just extendedPartyResult | isGameEnd ->
          GameEnded extendedPartyResult
        _ ->
          GameContinues mExtendedPartyResult state'
  where
    party = currentPlayer (hsTurnCounter state)
    getExtendedPartyResult turnCounter finishedBefore =
      let
        numberOfTurns = currentRound turnCounter + 1
        place =
          length $ filter ((< numberOfTurns) . prNumberOfTurns) finishedBefore
      in
      ExtendedPartyResult
        { eprPartyResult =
            PartyResult
              { prParty = party
              , prNumberOfTurns = numberOfTurns
              }
        , eprPlace = place
        , eprPlaceShared = place < length finishedBefore
        , eprLag =
            case finishedBefore of
              [] -> 0
              winner:_ -> numberOfTurns - prNumberOfTurns winner
        , eprNumberOfPlayers = length (tcPlayers turnCounter)
      }

undoLastMove :: HalmaState -> Maybe HalmaState
undoLastMove state = do
  move <- hsLastMove state
  board' <- eitherToMaybe $ movePiece (invertMove move) (hsBoard state)
  let
    finished' = 
      filter (hasFinished board' . partyHomeCorner . prParty) $
      hsFinished state
  Just
    HalmaState
      { hsBoard = board'
      , hsTurnCounter =
          fromMaybe (hsTurnCounter state) $
          nextTurnWith (`notElem` map prParty finished') $
          hsTurnCounter state
      , hsLastMove = Nothing
      , hsFinished = finished'
      }
  where
    eitherToMaybe = either (const Nothing) Just
    invertMove Move { moveFrom = from, moveTo = to } =
      Move { moveFrom = to, moveTo = from }

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

newRound :: Match -> Match
newRound match = 
  match
    { matchHistory = matchHistory match ++ toList mGameResult
    , matchCurrentGame = Just game'
    }
  where
    game' = initialHalmaState (matchConfig match)
    mGameResult = GameResult . hsFinished <$> matchCurrentGame match
    