module Game.Halma.GUI.State
  ( HalmaState (..)
  , initialHalmaState
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.Rules
import Game.TurnCounter

import Data.Default (def)

data HalmaState a
  = HalmaState
  { hsRuleOptions :: RuleOptions
  , hsBoard :: HalmaBoard
  , hsTurnCounter :: TurnCounter (Team, a)
  , hsLastMoved :: Maybe (Int, Int)
  } deriving (Eq, Show)

initialHalmaState :: Configuration a -> HalmaState a
initialHalmaState config =
  let
    (board, turnCounter) = newGame config
  in
    HalmaState
      { hsRuleOptions = def
      , hsBoard = board
      , hsTurnCounter = turnCounter
      , hsLastMoved = Nothing
      }
