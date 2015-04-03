module Game.Halma.AI.Competitive
  ( aiMove
  ) where

import Game.Halma.Board
import Game.Halma.Rules


-- The perspective of the first team as opposed to the second team.
type Perspective = (Team, Team)

flipPersp :: Perspective -> Perspective
flipPersp (t0, t1) = (t1, t0)


weighting :: Perspective -> Weighting

