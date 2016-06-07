module Game.Halma.GUI.State
  ( HalmaState (..)
  , newGame
  ) where

import Game.Halma.Board
import Game.Halma.Configuration
import Game.Halma.Rules
import Game.TurnCounter

import Data.Default (def)

data HalmaState size a
  = HalmaState
  { hsRuleOptions :: RuleOptions
  , hsBoard :: HalmaBoard size
  , hsTurnCounter :: TurnCounter (Team, a)
  , hsLastMoved :: Maybe (Int, Int)
  } deriving (Eq, Show)

newGame :: Configuration size a -> HalmaState size a
newGame (Configuration halmaGrid nop) =
  HalmaState
    { hsRuleOptions = def
    , hsBoard = initialBoard halmaGrid isActive
    , hsTurnCounter = newTurnCounter players
    , hsLastMoved = Nothing
    }
  where
    players = getPlayers nop
    isActive color = color `elem` (fst <$> players)
