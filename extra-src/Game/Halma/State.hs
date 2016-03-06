module Game.Halma.State
  ( HalmaState (..)
  , newGame
  ) where

import Game.TurnCounter
import Game.Halma.Configuration

import Game.Halma.Board
import Game.Halma.Rules

import Data.Default (def)

data HalmaState size =
  HalmaState
  { hsRuleOptions :: RuleOptions
  , hsBoard :: HalmaBoard size
  , hsTurnCounter :: TurnCounter Team
  , hsLastMoved :: Maybe (Int, Int)
  } deriving (Eq, Show)

newGame :: Configuration size -> HalmaState size
newGame (Configuration halmaGrid nop) =
  HalmaState
    { hsRuleOptions = def
    , hsBoard = initialBoard halmaGrid (flip elem players)
    , hsTurnCounter = newTurnCounter players
    , hsLastMoved = Nothing
    }
  where players = getPlayers nop